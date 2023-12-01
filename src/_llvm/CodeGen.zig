const std = @import("std");
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const Hir = @import("../Hir.zig");
const Type = @import("../typing.zig").Type;
const Allocator = std.mem.Allocator;
const c = Context.c;

const CodeGen = @This();

arena: Allocator,
builder: *Context.Builder,
hir: *const Hir,
func: *Hir.Inst.Function,
// TODO: is LLVM thread safe?
map: std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),

const Error = Allocator.Error || @import("../interner.zig").Error || error{NotImplemented};

pub fn generate(codegen: *CodeGen) !void {
    var builder = codegen.builder;

    // for (codegen.func.params) |param| {
    //     std.debug.print("{}\n", .{param});
    // }

    const entry = builder.appendBlock("entry");
    builder.positionAtEnd(entry);
    _ = try codegen.block(codegen.func.body);
}

fn resolveRef(codegen: *CodeGen, ref: Hir.Ref) c.LLVMValueRef {
    if (Hir.Inst.refToIndex(ref)) |index| {
        const hir = codegen.hir;
        if (hir.insts.items(.tag)[index] == .load_global) {
            // TODO: we probably still have forward declaration issues
            unreachable;
            // const pl = hir.insts.items(.data)[index].pl;
            // const decl = codegen.comp.decls.at(pl);
            // const backend = codegen.builder.backend;
            // if (decl.val.kind() == .function) {
            //     return llvm.c.LLVMGetNamedFunction(backend.module.module, decl.name);
            // } else {
            //     return llvm.c.LLVMGetNamedGlobal(backend.module.module, decl.name);
            // }
        } else {
            return codegen.map.get(index).?;
        }
    } else {
        return switch (ref) {
            .zero_val, .btrue_val => codegen.builder.addUint(Type.initInt(64, false), 0) catch unreachable,
            .one_val, .bfalse_val => codegen.builder.addUint(Type.initInt(64, false), 1) catch unreachable,
            else => unreachable,
        };
    }
}

fn block(codegen: *CodeGen, block_inst: Hir.Index) Error!c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.insts.items(.data)[block_inst];
    const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);

    const after_block = codegen.builder.appendBlock("yield_exit");
    var yield_val: c.LLVMValueRef = null;
    var yield_jump: bool = false;

    const insts = hir.block_slices[block_data.head];
    for (insts, 0..) |inst, i| {
        const ref = switch (hir.insts.items(.tag)[inst]) {
            .constant => try codegen.constant(inst),
            .add, .sub, .mul, .div, .mod => try codegen.binaryOp(inst),
            .ret_node => codegen.ret(inst, false),
            .ret_implicit => codegen.ret(inst, true),
            .alloca => try codegen.alloca(inst),
            .load => codegen.load(inst),
            .store => codegen.store(inst),
            .icmp_eq, .icmp_ne, .icmp_ugt, .icmp_uge, .icmp_ult, .icmp_ule, .icmp_sgt, .icmp_sge, .icmp_slt, .icmp_sle => try codegen.icmp(inst),
            .fcmp_gt, .fcmp_ge, .fcmp_lt, .fcmp_le => try codegen.fcmp(inst),
            .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge, .cmp_lt, .cmp_gt => unreachable,
            .zext => try codegen.zext(inst),
            .sext => try codegen.sext(inst),
            .fpext => try codegen.fpext(inst),
            // TODO
            .param => try codegen.functionParam(inst, @intCast(i)),
            .call => try codegen.call(inst),
            .branch_single => {
                try codegen.branchSingle(inst);
                continue;
            },
            .branch_double => {
                try codegen.branchDouble(inst);
                continue;
            },
            // .dbg_value => try codegen.dbgValue(inst),
            .ty => continue,
            .yield_implicit => {
                yield_val = codegen.yield(inst, true);
                break;
            },
            .yield_node => {
                yield_val = codegen.yield(inst, false);
                break;
            },
            .loop => {
                try codegen.loop(inst);
                continue;
            },
            .block => try codegen.block(inst),
            else => {
                std.debug.print("{}\n", .{hir.insts.items(.tag)[inst]});
                continue;
            },
        };
        try codegen.map.put(codegen.arena, inst, ref);
    }

    if (yield_jump) {
        codegen.builder.addBranch(after_block);
        codegen.builder.positionAtEnd(after_block);
    } else {
        codegen.builder.deleteBlock(after_block);
    }

    return yield_val;
}

fn constant(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const ty = hir.resolveType(data.ty);

    var builder = codegen.builder;
    switch (ty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => return null,

        .uint => return builder.addUint(ty, hir.refToInt(Hir.Inst.indexToRef(inst))),
        .sint => return builder.addSint(ty, hir.refToInt(Hir.Inst.indexToRef(inst))),
        .float => return builder.addFloat(ty, hir.refToFloat(Hir.Inst.indexToRef(inst))),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Binary);
    const lref = codegen.resolveRef(data.lref);
    const rref = codegen.resolveRef(data.rref);
    var builder = codegen.builder;

    const lty = hir.resolveType(data.lref);
    return switch (lty.kind()) {
        .uint => switch (hir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildUDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(builder.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .sint => switch (hir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildSDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildSRem(builder.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .float => switch (hir.insts.items(.tag)[inst]) {
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

fn alloca(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.insts.items(.data)[inst];
    var builder = codegen.builder;

    return builder.addAlloca(hir.resolveType(data.un_node.operand));
}

fn load(codegen: *CodeGen, inst: Hir.Index) c.LLVMValueRef {
    const pl = codegen.hir.insts.items(.data)[inst].pl_node.pl;

    const addr = codegen.resolveRef(Hir.Inst.indexToRef(pl));
    return codegen.builder.addLoad(addr);
}

fn store(codegen: *CodeGen, inst: Hir.Index) c.LLVMValueRef {
    const pl = codegen.hir.insts.items(.data)[inst].pl_node.pl;
    const data = codegen.hir.extraData(pl, Hir.Inst.Store);

    const addr = codegen.resolveRef(Hir.Inst.indexToRef(data.addr));
    const val = codegen.resolveRef(data.val);
    return codegen.builder.addStore(addr, val);
}

fn icmp(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const tag = hir.insts.items(.tag)[inst];
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Binary);

    const lref = codegen.resolveRef(data.lref);
    const rref = codegen.resolveRef(data.rref);
    const kind: Context.Builder.Cmp = switch (tag) {
        .icmp_eq => .cmp_ieq,
        .icmp_ne => .cmp_ine,
        .icmp_ule => .cmp_ule,
        .icmp_uge => .cmp_uge,
        .icmp_ult => .cmp_ult,
        .icmp_ugt => .cmp_ugt,
        .icmp_sle => .cmp_sle,
        .icmp_sge => .cmp_sge,
        .icmp_slt => .cmp_slt,
        .icmp_sgt => .cmp_sgt,
        else => unreachable,
    };

    return codegen.builder.addCmp(kind, lref, rref);
}

fn fcmp(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const tag = hir.insts.items(.tag)[inst];
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Binary);

    const lref = codegen.resolveRef(data.lref);
    const rref = codegen.resolveRef(data.rref);
    const kind: Context.Builder.Cmp = switch (tag) {
        .fcmp_le => .cmp_fle,
        .fcmp_ge => .cmp_fge,
        .fcmp_lt => .cmp_flt,
        .fcmp_gt => .cmp_fgt,
        else => unreachable,
    };

    return codegen.builder.addCmp(kind, lref, rref);
}

fn branchSingle(codegen: *CodeGen, inst: Hir.Index) Error!void {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.BranchSingle);
    var builder = codegen.builder;

    const condition = codegen.resolveRef(data.condition);
    const exec_true = builder.appendBlock("if_true");
    const prev = builder.getInsertBlock();
    builder.positionAtEnd(exec_true);

    // TODO: branch expressions
    _ = try codegen.block(data.exec_true);
    const exit = builder.appendBlock("if_exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null)
        builder.addBranch(exit);

    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exit);

    builder.positionAtEnd(exit);
}

fn branchDouble(codegen: *CodeGen, inst: Hir.Index) Error!void {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.BranchDouble);
    var builder = codegen.builder;

    const condition = codegen.resolveRef(data.condition);

    // TODO: branch expressions
    const prev = builder.getInsertBlock();
    const exec_true = builder.appendBlock("ifelse_true");
    builder.positionAtEnd(exec_true);
    _ = try codegen.block(data.exec_true);

    const false_prev = builder.getInsertBlock();
    const exec_false = builder.appendBlock("ifelse_false");
    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exec_false);
    builder.positionAtEnd(exec_false);
    _ = try codegen.block(data.exec_false);

    const exit = builder.appendBlock("ifelse_exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null)
        builder.addBranch(exit);
    if (c.LLVMGetBasicBlockTerminator(false_prev) == null) {
        builder.positionAtEnd(false_prev);
        builder.addBranch(exit);
    }

    builder.positionAtEnd(exit);
}

fn functionParam(codegen: *CodeGen, inst: Hir.Index, index: u32) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Param);

    const param_str = try hir.interner.get(data.name);
    const param_ref = c.LLVMGetParam(codegen.builder.function, index);
    c.LLVMSetValueName2(param_ref, param_str.ptr, param_str.len);
    return param_ref;
}

fn dbgValue(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.insts.items(.data)[inst];

    const id = data.op_pl.pl;
    const ident_str = try hir.interner.get(id);
    const ref = codegen.resolveRef(data.op_pl.op);
    // TODO: fix, change dbgValue to an actual export decl
    _ = ident_str;
    // c.LLVMSetValueName2(ref, ident_str.ptr, ident_str.len);
    return ref;
}

fn zext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Extend);

    const ty = hir.resolveType(data.ty);
    const ref = codegen.resolveRef(data.val);
    return codegen.builder.addZext(ty, ref);
}

fn sext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Extend);

    const ty = hir.resolveType(data.ty);
    const ref = codegen.resolveRef(data.val);
    return codegen.builder.addSext(ty, ref);
}

fn fpext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Extend);

    const ty = hir.resolveType(data.ty);
    const ref = codegen.resolveRef(data.val);
    return codegen.builder.addFpext(ty, ref);
}

fn yield(codegen: *CodeGen, inst: Hir.Index, comptime implicit: bool) c.LLVMValueRef {
    const data = codegen.hir.insts.items(.data)[inst];

    if (implicit) {
        return codegen.resolveRef(data.un_tok.operand);
    } else {
        return codegen.resolveRef(data.un_node.operand);
    }
}

fn loop(codegen: *CodeGen, inst: Hir.Index) !void {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Loop);
    var builder = codegen.builder;

    const prev = builder.getInsertBlock();
    const entry_block = builder.appendBlock("loop_entry");
    builder.positionAtEnd(entry_block);
    _ = try codegen.block(data.body);

    const condition_block = builder.appendBlock("loop_cond");
    builder.addBranch(condition_block);
    builder.positionAtEnd(prev);
    builder.addBranch(condition_block);
    builder.positionAtEnd(condition_block);
    const condition_ref = try codegen.block(data.condition);

    const exit_block = builder.appendBlock("loop_exit");
    builder.addCondBranch(condition_ref, entry_block, exit_block);

    builder.positionAtEnd(exit_block);
}

fn call(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Call);

    const addr = codegen.resolveRef(data.addr);
    const args = try codegen.arena.alloc(c.LLVMValueRef, data.args_len);
    defer codegen.arena.free(args);
    const hir_args = hir.extra_data[pl + 1 .. pl + 1 + data.args_len];
    for (hir_args, 0..) |arg, i| {
        args[i] = codegen.resolveRef(@enumFromInt(arg));
    }

    const ty = hir.resolveType(data.addr);
    return codegen.builder.addCall(ty, addr, args);
}

fn ret(codegen: *CodeGen, inst: Hir.Index, comptime implicit: bool) c.LLVMValueRef {
    const hir = codegen.hir;

    if (implicit) {
        const data = hir.insts.items(.data)[inst].un_tok;
        const ref = if (data.operand == Hir.Ref.void_val) null else codegen.resolveRef(data.operand);
        return codegen.builder.addReturn(ref);
    } else {
        const data = hir.insts.items(.data)[inst].un_node;
        const ref = if (data.operand == Hir.Ref.void_val) null else codegen.resolveRef(data.operand);
        return codegen.builder.addReturn(ref);
    }
}
