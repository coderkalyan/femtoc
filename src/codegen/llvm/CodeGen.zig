const std = @import("std");
const Compilation = @import("../../Compilation.zig");
const llvm = @import("llvm.zig");
const Value = @import("../../value.zig").Value;
const Mir = @import("../../Mir.zig");

const Allocator = std.mem.Allocator;
// const Decl = Compilation.Decl;
const CodeGen = @This();

gpa: Allocator,
comp: *const Compilation,
mir: *const Mir,
// TODO: is LLVM thread safe?
module: llvm.Module,
map: std.AutoHashMapUnmanaged(Mir.Index, llvm.Value),
function: llvm.Value,
builder: llvm.Builder,
alloc_block: llvm.c.LLVMBasicBlockRef,

const Error = Allocator.Error || @import("../../interner.zig").Error || error { NotImplemented };
const c = llvm.c;

pub fn generate(codegen: *CodeGen) !void {
    // const data = mir.insts.items(.data)[mir.insts.len - 1];
    // std.debug.print("{}\n", .{mir.insts.items(.tag)[mir.insts.len - 1]});
    const block_inst = @intCast(u32, codegen.mir.insts.len - 1);

    codegen.alloc_block = c.LLVMAppendBasicBlock(codegen.function, "alloc");
    const entry = c.LLVMAppendBasicBlock(codegen.function, "entry");
    c.LLVMPositionBuilderAtEnd(codegen.builder, entry);
    // _ = try codegen.block(data.bin_pl.r);
    _ = try codegen.block(block_inst);
    c.LLVMPositionBuilderAtEnd(codegen.builder, codegen.alloc_block);
    _ = c.LLVMBuildBr(codegen.builder, entry);
}

fn resolveRef(codegen: *CodeGen, mirref: Mir.Ref) llvm.Value {
    if (Mir.refToIndex(mirref)) |index| {
        // TODO: we probably still have forward declaration issues
        const mir = codegen.mir;
        if (mir.insts.items(.tag)[index] == .load_decl) {
            const pl = mir.insts.items(.data)[index].pl;
            const decl = codegen.comp.decls.at(pl);
            std.debug.print("{s}\n", .{decl.name});
            // TODO: figure out this global vs function nonsense
            // return llvm.c.LLVMGetNamedGlobal(codegen.module, decl.name);
            return llvm.c.LLVMGetNamedFunction(codegen.module, decl.name);
        } else {
            return codegen.map.get(index).?;
        }
    } else {
        return switch (mirref) {
            .zero_val => c.LLVMConstInt(c.LLVMInt64Type(), 0, 0),
            .one_val => c.LLVMConstInt(c.LLVMInt64Type(), 1, 0),
            else => unreachable,
        };
    }
}

fn block(codegen: *CodeGen, block_inst: Mir.Index) Error!c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[block_inst];
    const block_data = mir.extraData(data.pl, Mir.Inst.Block);

    const after_block = c.LLVMAppendBasicBlock(codegen.function, "");
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
            else => {
                std.debug.print("{}\n", .{mir.insts.items(.tag)[inst]});
                continue;
            },
        };
        try codegen.map.put(codegen.gpa, inst, ref);
    }

    if (yield_jump) {
        _ = c.LLVMBuildBr(codegen.builder, after_block);
        c.LLVMPositionBuilderAtEnd(codegen.builder, after_block);
    } else {
        c.LLVMDeleteBasicBlock(after_block);
    }

    return yield_val;
}

fn constant(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst].ty_pl;
    const mir_ty = mir.refToType(data.ty);
    switch (mir_ty.kind()) {
        .comptime_uint,
        .comptime_sint,
        .comptime_float => return c.LLVMConstPointerNull(c.LLVMInt32Type()),
        else => {},
    }

    const ty = try llvm.getType(codegen.gpa, mir_ty);
    switch (mir_ty.kind()) {
        .uint => return c.LLVMConstInt(ty, @intCast(c_ulonglong, mir.valToInt(data.pl)), 0),
        .sint => return c.LLVMConstInt(ty, @intCast(c_ulonglong, mir.valToInt(data.pl)), 1),
        .float => return c.LLVMConstReal(ty, mir.valToFloat(data.pl)),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const lref = codegen.resolveRef(data.bin_op.lref);
    const rref = codegen.resolveRef(data.bin_op.rref);
    
    const lty = mir.resolveTy(data.bin_op.lref);
    return switch (lty.kind()) {
        .uint => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(codegen.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(codegen.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(codegen.builder, lref, rref, ""),
            .div => c.LLVMBuildUDiv(codegen.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(codegen.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .sint => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildAdd(codegen.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(codegen.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(codegen.builder, lref, rref, ""),
            .div => c.LLVMBuildSDiv(codegen.builder, lref, rref, ""),
            .mod => return c.LLVMBuildSRem(codegen.builder, lref, rref, ""), // TODO: this is not correct
            else => unreachable,
        },
        .float => switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildFAdd(codegen.builder, lref, rref, ""),
            .sub => c.LLVMBuildFSub(codegen.builder, lref, rref, ""),
            .mul => c.LLVMBuildFMul(codegen.builder, lref, rref, ""),
            .div => c.LLVMBuildFDiv(codegen.builder, lref, rref, ""),
            .mod => c.LLVMBuildFRem(codegen.builder, lref, rref, ""),
            else => unreachable,
        },
        else => unreachable,
    };
}

fn ret(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    
    if (data.un_op == Mir.Ref.void_val) {
        return c.LLVMBuildRetVoid(codegen.builder);
    } else {
        const ref = codegen.resolveRef(data.un_op);
        return c.LLVMBuildRet(codegen.builder, ref);
    }
}

fn alloc(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = try llvm.getType(codegen.gpa, data.ty);
    const bb = llvm.c.LLVMGetInsertBlock(codegen.builder);
    llvm.c.LLVMPositionBuilderAtEnd(codegen.builder, codegen.alloc_block);
    const alloca = c.LLVMBuildAlloca(codegen.builder, ty, "");
    llvm.c.LLVMPositionBuilderAtEnd(codegen.builder, bb);
    return alloca;
}

fn store(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    const addr = codegen.resolveRef(data.bin_op.lref);
    const val = codegen.resolveRef(data.bin_op.rref);
    return c.LLVMBuildStore(codegen.builder, val, addr);
}

fn load(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    const addr = codegen.resolveRef(data.un_op);
    const ty = c.LLVMGetAllocatedType(addr);
    return c.LLVMBuildLoad2(codegen.builder, ty, addr, "");
}

fn cmp(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const mir = codegen.mir;
    const tag = mir.insts.items(.tag)[inst];
    const data = mir.insts.items(.data)[inst];

    const lref = codegen.resolveRef(data.bin_op.lref);
    const rref = codegen.resolveRef(data.bin_op.rref);
    return switch (tag) {
        .cmp_eq => ref: {
            const ty = c.LLVMTypeOf(lref);
            break :ref switch (c.LLVMGetTypeKind(ty)) {
                c.LLVMIntegerTypeKind => c.LLVMBuildICmp(codegen.builder, c.LLVMIntEQ, lref, rref, ""),
                c.LLVMFloatTypeKind => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOEQ, lref, rref, ""),
                else => unreachable,
            };
        },
        .cmp_ne => ref: {
            const ty = c.LLVMTypeOf(lref);
            break :ref switch (c.LLVMGetTypeKind(ty)) {
                c.LLVMIntegerTypeKind => c.LLVMBuildICmp(codegen.builder, c.LLVMIntNE, lref, rref, ""),
                c.LLVMFloatTypeKind => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealONE, lref, rref, ""),
                else => unreachable,
            };
        },

        .cmp_ule => c.LLVMBuildICmp(codegen.builder, c.LLVMIntULE, lref, rref, ""),
        .cmp_uge => c.LLVMBuildICmp(codegen.builder, c.LLVMIntUGE, lref, rref, ""),
        .cmp_ult => c.LLVMBuildICmp(codegen.builder, c.LLVMIntULT, lref, rref, ""),
        .cmp_ugt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntUGT, lref, rref, ""),

        .cmp_sle => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSLE, lref, rref, ""),
        .cmp_sge => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSGE, lref, rref, ""),
        .cmp_slt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSLT, lref, rref, ""),
        .cmp_sgt => c.LLVMBuildICmp(codegen.builder, c.LLVMIntSGT, lref, rref, ""),

        .cmp_fle => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOLE, lref, rref, ""),
        .cmp_fge => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOGE, lref, rref, ""),
        .cmp_flt => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOLT, lref, rref, ""),
        .cmp_fgt => c.LLVMBuildFCmp(codegen.builder, c.LLVMRealOGT, lref, rref, ""),
        else => unreachable,
    };
}

fn branchSingle(codegen: *CodeGen, inst: Mir.Index) Error!void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const condition = codegen.resolveRef(data.op_pl.op);
    const exec_true = c.LLVMAppendBasicBlock(codegen.function, "exec_true");
    const exit = c.LLVMAppendBasicBlock(codegen.function, "exit");
    _ = c.LLVMBuildCondBr(codegen.builder, condition, exec_true, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_true);
    // TODO: branch expressions
    _ = try codegen.block(data.op_pl.pl);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exit);
}

fn branchDouble(codegen: *CodeGen, inst: Mir.Index) Error!void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const condbr = mir.extraData(data.op_pl.pl, Mir.Inst.CondBr);

    const condition = codegen.resolveRef(data.op_pl.op);
    const exec_true = c.LLVMAppendBasicBlock(codegen.function, "exec_true");
    const exec_false = c.LLVMAppendBasicBlock(codegen.function, "exec_false");
    const exit = c.LLVMAppendBasicBlock(codegen.function, "exit");
    _ = c.LLVMBuildCondBr(codegen.builder, condition, exec_true, exec_false);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_true);
    // TODO: branch expressions
    _ = try codegen.block(condbr.exec_true);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_false);
    _ = try codegen.block(condbr.exec_false);
    if (c.LLVMGetBasicBlockTerminator(exec_false) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exit);
}

fn functionParam(codegen: *CodeGen, inst: Mir.Index, index: u32) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const id = data.ty_pl.pl;
    const param_str = try mir.interner.get(id);
    const param_ref = c.LLVMGetParam(codegen.function, index);
    c.LLVMSetValueName2(param_ref, param_str.ptr, param_str.len);
    return param_ref;
}

fn dbgValue(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const id = data.op_pl.pl;
    const ident_str = try mir.interner.get(id);
    const ref = codegen.resolveRef(data.op_pl.op);
    c.LLVMSetValueName2(ref, ident_str.ptr, ident_str.len);
    return ref;
}

fn zext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = try llvm.getType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveRef(data.ty_op.op);
    return c.LLVMBuildZExt(codegen.builder, ref, ty, "");
}

fn sext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = try llvm.getType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveRef(data.ty_op.op);
    return c.LLVMBuildSExt(codegen.builder, ref, ty, "");
}

fn fpext(codegen: *CodeGen, inst: Mir.Index) !c.LLVMValueRef {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];

    const ty = try llvm.getType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveRef(data.ty_op.op);
    return c.LLVMBuildFPExt(codegen.builder, ref, ty, "");
}

fn yield(codegen: *CodeGen, inst: Mir.Index) c.LLVMValueRef {
    const data = codegen.mir.insts.items(.data)[inst];

    return codegen.resolveRef(data.un_op);
}

fn loop(codegen: *CodeGen, inst: Mir.Index) !void {
    const mir = codegen.mir;
    const data = mir.insts.items(.data)[inst];
    const loop_data = mir.extraData(data.pl, Mir.Inst.Loop);
    
    const entry_block = c.LLVMAppendBasicBlock(codegen.function, "loop_entry");
    const condition_block = c.LLVMAppendBasicBlock(codegen.function, "loop_condition");
    _ = c.LLVMBuildBr(codegen.builder, condition_block);

    c.LLVMPositionBuilderAtEnd(codegen.builder, condition_block);
    const condition_ref = try codegen.block(loop_data.condition);
    // const yield_block = c.LLVMGetInsertBlock(codegen.builder);
    // const name = "loop_yield";
    // c.LLVMSetValueName2(c.LLVMBasicBlockAsValue(yield_block), name, name.len);
    const exit_block = c.LLVMAppendBasicBlock(codegen.function, "loop_exit");
    _ = c.LLVMBuildCondBr(codegen.builder, condition_ref, entry_block, exit_block);

    c.LLVMPositionBuilderAtEnd(codegen.builder, entry_block);
    _ = try codegen.block(loop_data.body);
    _ = c.LLVMBuildBr(codegen.builder, condition_block);
    
    c.LLVMPositionBuilderAtEnd(codegen.builder, exit_block);
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
    const llvm_type = try llvm.getType(codegen.gpa, mir.resolveTy(data.op));
    const val = c.LLVMBuildCall2(codegen.builder, llvm_type, addr,
                                 args.ptr, @intCast(c_uint, args.len), "");
    return val;
}
