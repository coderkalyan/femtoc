const std = @import("std");
const Compilation = @import("../../Compilation.zig");
const llvm = @import("llvm.zig");
const Value = @import("../../value.zig").Value;
const Mir = @import("../../Mir.zig");
const typing = @import("../../typing.zig");

const Allocator = std.mem.Allocator;
// const Decl = Compilation.Decl;
const CodeGen = @This();
const Type = typing.Type;

gpa: Allocator,
comp: *const Compilation,
mir: *const Mir,
// TODO: is LLVM thread safe?
// module: llvm.Module,
map: std.AutoHashMapUnmanaged(Mir.Index, llvm.Value),
// function: llvm.Value,
builder: llvm.Builder,
alloc_block: llvm.c.LLVMBasicBlockRef,

const Error = Allocator.Error || @import("../../interner.zig").Error || error { NotImplemented };
const c = llvm.c;

pub fn generate(codegen: *CodeGen) !void {
    const block_inst = @intCast(u32, codegen.mir.insts.len - 1);
    var builder = codegen.builder;

    codegen.alloc_block = builder.appendBlock("common.alloca");
    const entry = builder.appendBlock("common.entry");
    builder.positionAtEnd(entry);
    _ = try codegen.block(block_inst);
    builder.positionAtEnd(codegen.alloc_block);
    builder.addBranch(entry);
}

fn resolveRef(codegen: *CodeGen, mirref: Mir.Ref) llvm.Value {
    if (Mir.refToIndex(mirref)) |index| {
        // TODO: we probably still have forward declaration issues
        const mir = codegen.mir;
        if (mir.insts.items(.tag)[index] == .load_decl) {
            const pl = mir.insts.items(.data)[index].pl;
            const decl = codegen.comp.decls.at(pl);
            const backend = codegen.builder.backend;
            if (decl.val.kind() == .function) {
                return llvm.c.LLVMGetNamedFunction(backend.module.module, decl.name);
            } else {
                return llvm.c.LLVMGetNamedGlobal(backend.module.module, decl.name);
            }
        } else {
            return codegen.map.get(index).?;
        }
    } else {
        return switch (mirref) {
            .zero_val => codegen.builder.addUint(Type.initInt(64, false), 0),
            .one_val => codegen.builder.addUint(Type.initInt(64, false), 1),
            else => unreachable,
        };
    }
}

fn block(codegen: *CodeGen, block_inst: Mir.Index) Error!c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[block_inst];
    const block_data = mir.extraData(data.pl, Mir.Inst.Block);

    const after_block = codegen.builder.appendBlock("yield.exit");
    var yield_val: c.LLVMValueRef = null;
    var yield_jump: bool = false;

    const extra_base = data.pl + 1;
    const insts = mir.extra[extra_base..extra_base + block_data.insts_len];
    for (insts) |inst, i| {
        const ref = switch (mir.insts.items(.tag)[inst]) {
            .constant => try codegen.constant(inst),
            .add, .sub, .mul, .div, .mod => try codegen.binaryOp(inst),
            .ret => codegen.ret(inst),
            .alloc => try codegen.alloc(inst),
            .load => codegen.load(inst),
            .store => codegen.store(inst),
            .cmp_eq, .cmp_ne,
            .cmp_ule, .cmp_uge, .cmp_ult, .cmp_ugt,
            .cmp_sle, .cmp_sge, .cmp_slt, .cmp_sgt,
            .cmp_fle, .cmp_fge, .cmp_flt, .cmp_fgt => codegen.cmp(inst),
            .zext => try codegen.zext(inst),
            .sext => try codegen.sext(inst),
            .fpext => try codegen.fpext(inst),
            .param => try codegen.functionParam(inst, @intCast(u32, i)),
            .call => try codegen.call(inst),
            .branch_single => {
                try codegen.branchSingle(inst);
                continue;
            },
            .branch_double => {
                try codegen.branchDouble(inst);
                continue;
            },
            .dbg_value => try codegen.dbgValue(inst),
            .ty => continue,
            .yield => {
                yield_val = codegen.yield(inst);
                break;
            },
            .loop => {
                try codegen.loop(inst);
                continue;
            },
            .block => try codegen.block(inst),
            .load_decl => continue,
            else => {
                std.debug.print("{}\n", .{mir.insts.items(.tag)[inst]});
                continue;
            },
        };
        try codegen.map.put(codegen.gpa, inst, ref);
    }

    if (yield_jump) {
        codegen.builder.addBranch(after_block);
        codegen.builder.positionAtEnd(after_block);
    } else {
        codegen.builder.deleteBlock(after_block);
    }

    return yield_val;
}

fn constant(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst].ty_pl;
    const ty = mir.resolveType(data.ty);

    var builder = codegen.builder;
    switch (ty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => return null,

        .uint => return builder.addUint(ty, mir.refToInt(Mir.indexToRef(inst))),
        .sint => return builder.addSint(ty, mir.refToInt(Mir.indexToRef(inst))),
        .float => return builder.addFloat(ty, mir.refToFloat(Mir.indexToRef(inst))),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
   const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const lref = codegen.resolveRef(data.bin_op.lref);
    const rref = codegen.resolveRef(data.bin_op.rref);
    var builder = codegen.builder;
    
    const lty = mir.resolveType(data.bin_op.lref);
    return switch (lty.kind()) {
        .uint => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildUDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(builder.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .sint => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildSDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildSRem(builder.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .float => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildFAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildFSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildFMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildFDiv(builder.builder, lref, rref, ""),
            .mod => c.LLVMBuildFRem(builder.builder, lref, rref, ""),
            else => unreachable,
        },
        else => unreachable,
    };
}


fn alloc(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    var builder = codegen.builder;

    const bb = builder.getInsertBlock();
    builder.positionAtEnd(codegen.alloc_block);
    const alloca = builder.addAlloca(mir.resolveType(data.un_op));
    builder.positionAtEnd(bb);
    return alloca;
}

fn load(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    const addr = codegen.resolveRef(data.un_op);
    return codegen.builder.addLoad(addr);
}

fn store(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    const addr = codegen.resolveRef(data.bin_op.lref);
    const val = codegen.resolveRef(data.bin_op.rref);
    return codegen.builder.addStore(addr, val);
}

fn cmp(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const mir = codegen.mir;
    const tag = mir.insts.items(.tag)[inst];
    const data = mir.insts.items(.data)[inst];

    const lref = codegen.resolveRef(data.bin_op.lref);
    const rref = codegen.resolveRef(data.bin_op.rref);
    const kind: llvm.Builder.Cmp = switch (tag) {
        .cmp_eq => ref: {
            const ty = c.LLVMTypeOf(lref);
            break :ref switch (c.LLVMGetTypeKind(ty)) {
                c.LLVMIntegerTypeKind => .cmp_ieq,
                c.LLVMFloatTypeKind => .cmp_feq,
                else => unreachable,
            };
        },
        .cmp_ne => ref: {
            const ty = c.LLVMTypeOf(lref);
            break :ref switch (c.LLVMGetTypeKind(ty)) {
                c.LLVMIntegerTypeKind => .cmp_ine,
                c.LLVMFloatTypeKind => .cmp_fne,
                else => unreachable,
            };
        },

        .cmp_ule => .cmp_ule,
        .cmp_uge => .cmp_uge,
        .cmp_ult => .cmp_ult,
        .cmp_ugt => .cmp_ugt,

        .cmp_sle => .cmp_sle,
        .cmp_sge => .cmp_sge,
        .cmp_slt => .cmp_slt,
        .cmp_sgt => .cmp_sgt,

        .cmp_fle => .cmp_fle,
        .cmp_fge => .cmp_fge,
        .cmp_flt => .cmp_flt,
        .cmp_fgt => .cmp_fgt,

        // .cmp_ule => c.LLVMBuildICmp(codegen.builder, c.LLVMIntULE, lref, rref, ""),
        // .cmp_uge => c.LLVMBuildICmp(codegen.builder, c.LLVMIntUGE, lref, rref, ""),
        // .cmp_ult => c.LLVMBuildICmp(codegen.builder, c.LLVMIntULT, lref, rref, ""),
        // .cmp_ugt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntUGT, lref, rref, ""),

        // .cmp_sle => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSLE, lref, rref, ""),
        // .cmp_sge => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSGE, lref, rref, ""),
        // .cmp_slt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSLT, lref, rref, ""),
        // .cmp_sgt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSGT, lref, rref, ""),

        // .cmp_fle => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOLE, lref, rref, ""),
        // .cmp_fge => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOGE, lref, rref, ""),
        // .cmp_flt => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOLT, lref, rref, ""),
        // .cmp_fgt => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOGT, lref, rref, ""),
        else => unreachable,
    };

    return codegen.builder.addCmp(kind, lref, rref);
}

fn branchSingle(codegen: *CodeGen, inst: Mir.Index) Error!void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    var builder = codegen.builder;

    const condition = codegen.resolveRef(data.op_pl.op);
    const exec_true = builder.appendBlock("if.true");
    const exit = builder.appendBlock("if.exit");
    builder.addCondBranch(condition, exec_true, exit);

    builder.positionAtEnd(exec_true);
    // TODO: branch expressions
    _ = try codegen.block(data.op_pl.pl);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        builder.addBranch(exit);

    builder.positionAtEnd(exit);
}

fn branchDouble(codegen: *CodeGen, inst: Mir.Index) Error!void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const condbr = mir.extraData(data.op_pl.pl, Mir.Inst.CondBr);
    var builder = codegen.builder;

    const condition = codegen.resolveRef(data.op_pl.op);
    const exec_true = builder.appendBlock("ifelse.true");
    const exec_false = builder.appendBlock("ifelse.false");
    const exit = builder.appendBlock("ifelse.exit");
    builder.addCondBranch(condition, exec_true, exec_false);

    builder.positionAtEnd(exec_true);
    // TODO: branch expressions
    _ = try codegen.block(condbr.exec_true);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        builder.addBranch(exit);

    builder.positionAtEnd(exec_false);
    _ = try codegen.block(condbr.exec_false);
    if (c.LLVMGetBasicBlockTerminator(exec_false) == null)
        builder.addBranch(exit);

    builder.positionAtEnd(exit);
}

fn functionParam(codegen: *CodeGen, inst: Mir.Index, index: u32) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const id = data.ty_pl.pl;
    const param_str = try mir.interner.get(id);
    const param_ref = c.LLVMGetParam(codegen.builder.function, index);
    c.LLVMSetValueName2(param_ref, param_str.ptr, param_str.len);
    return param_ref;
}

fn dbgValue(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const id = data.op_pl.pl;
    const ident_str = try mir.interner.get(id);
    const ref = codegen.resolveRef(data.op_pl.op);
    // TODO: fix, change dbgValue to an actual export decl
    _ = ident_str;
    // c.LLVMSetValueName2(ref, ident_str.ptr, ident_str.len);
    return ref;
}

fn zext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = mir.resolveType(data.ty_op.ty);
    const ref = codegen.resolveRef(data.ty_op.op);
    return codegen.builder.addZext(ty, ref);
}

fn sext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = mir.resolveType(data.ty_op.ty);
    const ref = codegen.resolveRef(data.ty_op.op);
    return codegen.builder.addSext(ty, ref);
}

fn fpext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = mir.resolveType(data.ty_op.ty);
    const ref = codegen.resolveRef(data.ty_op.op);
    return codegen.builder.addFpext(ty, ref);
}

fn yield(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    return codegen.resolveRef(data.un_op);
}

fn loop(codegen: *CodeGen, inst: Mir.Index) !void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const condition = data.bin_pl.l;
    const body = data.bin_pl.r;
    var builder = codegen.builder;
    
    const entry_block = builder.appendBlock("loop.entry");
    const condition_block = builder.appendBlock("loop.cond");
    builder.addBranch(condition_block);

    builder.positionAtEnd(condition_block);
    const condition_ref = try codegen.block(condition);
    const exit_block = builder.appendBlock("loop.exit");
    builder.addCondBranch(condition_ref, entry_block, exit_block);

    builder.positionAtEnd(entry_block);
    _ = try codegen.block(body);
    builder.addBranch(condition_block);
    
    builder.positionAtEnd(exit_block);
}

fn call(codegen: *CodeGen, inst: Mir.Index) !llvm.Value {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst].op_pl;
    const call_data = mir.extraData(data.pl, Mir.Inst.Call);

    const addr = codegen.resolveRef(data.op);
    const args = try codegen.gpa.alloc(llvm.Value, call_data.args_len);
    defer codegen.gpa.free(args);
    const mir_args = mir.extra[data.pl + 1..data.pl + 1 + call_data.args_len];
    for (mir_args) |arg, i| {
        args[i] = codegen.resolveRef(@intToEnum(Mir.Ref, arg));
    }

    const ty = mir.resolveType(data.op);
    return codegen.builder.addCall(ty, addr, args);
}

fn ret(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    
    const ref = if (data.un_op == Mir.Ref.void_val) null else codegen.resolveRef(data.un_op);
    return codegen.builder.addReturn(ref);
}
