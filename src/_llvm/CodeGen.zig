const std = @import("std");
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const Hir = @import("../Hir.zig");
const Type = @import("../hir/type.zig").Type;
const Allocator = std.mem.Allocator;
const c = Context.c;

const CodeGen = @This();

arena: Allocator,
builder: *Context.Builder,
hir: *const Hir,
func: *const Value.Function,
// TODO: is LLVM thread safe?
map: std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
global_map: *std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),

const Error = Allocator.Error || @import("../interner.zig").Error || error{NotImplemented};

pub fn generate(codegen: *CodeGen) !void {
    var builder = codegen.builder;

    const entry = builder.appendBlock("entry");
    builder.positionAtEnd(entry);
    _ = try codegen.block(codegen.func.body);
}

fn resolveInst(codegen: *CodeGen, inst: Hir.Index) c.LLVMValueRef {
    const hir = codegen.hir;
    if (hir.insts.items(.tag)[inst] == .load_global) {
        const data = hir.get(inst, .load_global);
        // TODO: we can probably remove global map and just use llvm to look it up
        // via the name
        return codegen.global_map.get(data.operand).?;
    } else {
        if (codegen.map.get(inst) == null)
            std.debug.print("resolving %{}\n", .{inst});
        return codegen.map.get(inst).?;
    }
}

fn block(codegen: *CodeGen, block_inst: Hir.Index) Error!c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.insts.items(.data)[block_inst];
    const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);

    const after_block = codegen.builder.appendBlock("yield.exit");
    var yield_val: c.LLVMValueRef = null;
    var yield_jump: bool = false;

    const insts = hir.block_slices[block_data.head];
    for (insts, 0..) |inst, i| {
        const ref = switch (hir.insts.items(.tag)[inst]) {
            .constant => try codegen.constant(inst),
            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            => |tag| try codegen.binaryOp(inst, tag),
            inline .ret_node, .ret_implicit => |tag| codegen.ret(inst, tag),
            .alloca => try codegen.alloca(inst),
            .load => try codegen.load(inst),
            .store => codegen.store(inst),
            inline .icmp_eq,
            .icmp_ne,
            .icmp_ugt,
            .icmp_uge,
            .icmp_ult,
            .icmp_ule,
            .icmp_sgt,
            .icmp_sge,
            .icmp_slt,
            .icmp_sle,
            .fcmp_gt,
            .fcmp_ge,
            .fcmp_lt,
            .fcmp_le,
            => |tag| try codegen.cmp(inst, tag),
            .cmp_eq,
            .cmp_ne,
            .cmp_le,
            .cmp_ge,
            .cmp_lt,
            .cmp_gt,
            => unreachable,
            .zext => try codegen.zext(inst),
            .sext => try codegen.sext(inst),
            .fpext => try codegen.fpext(inst),
            .param => try codegen.param(inst, @intCast(i)),
            .call => try codegen.call(inst),
            .branch_single => {
                try codegen.branchSingle(inst);
                continue;
            },
            .branch_double => {
                try codegen.branchDouble(inst);
                continue;
            },
            .ty => continue,
            inline .yield_implicit, .yield_node => |tag| {
                yield_val = codegen.yield(inst, tag);
                break;
            },
            .loop => {
                try codegen.loop(inst);
                continue;
            },
            .block => try codegen.block(inst),
            .log_not => codegen.logNot(inst),
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
    const ty = try hir.resolveType(codegen.arena, data.ty);

    var builder = codegen.builder;
    switch (ty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => return null,

        .uint => return builder.addUint(ty, hir.instToInt(inst)),
        .sint => return builder.addSint(ty, hir.instToInt(inst)),
        .float => return builder.addFloat(ty, hir.instToFloat(inst)),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    const lref = codegen.resolveInst(data.lref);
    const rref = codegen.resolveInst(data.rref);
    var builder = codegen.builder;

    // C (clang 17) emits urem for all integer modulo
    // in the long term, we need to decide what behavior we want for
    // negative numbers and 0
    const lty = try hir.resolveType(codegen.arena, data.lref);
    return switch (lty.kind()) {
        .uint => switch (tag) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildUDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(builder.builder, lref, rref, ""),
            else => unreachable,
        },
        .sint => switch (tag) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildSDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(builder.builder, lref, rref, ""),
            else => unreachable,
        },
        .float => switch (tag) {
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
    const data = hir.get(inst, .alloca);
    return codegen.builder.addAlloca(try hir.resolveType(codegen.arena, data.operand));
}

fn load(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .load);
    const pointer_type = try hir.resolveType(codegen.arena, data.operand);
    const pointee_type = pointer_type.extended.cast(Type.Pointer).?.pointee;
    const ptr = codegen.resolveInst(data.operand);
    return codegen.builder.addLoad(ptr, pointee_type);
}

fn store(codegen: *CodeGen, inst: Hir.Index) c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .store);
    const addr = codegen.resolveInst(data.ptr);
    const val = codegen.resolveInst(data.val);
    return codegen.builder.addStore(addr, val);
}

fn cmp(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    const lref = codegen.resolveInst(data.lref);
    const rref = codegen.resolveInst(data.rref);
    return codegen.builder.addCmp(tag, lref, rref);
}

fn branchSingle(codegen: *CodeGen, inst: Hir.Index) Error!void {
    const hir = codegen.hir;
    var builder = codegen.builder;
    const data = hir.get(inst, .branch_single);

    const condition = codegen.resolveInst(data.condition);
    const exec_true = builder.appendBlock("if.true");
    const prev = builder.getInsertBlock();
    builder.positionAtEnd(exec_true);

    // TODO: branch expressions
    _ = try codegen.block(data.exec_true);
    const exit = builder.appendBlock("if.exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null) {
        builder.addBranch(exit);
    }

    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exit);
    builder.positionAtEnd(exit);
}

fn branchDouble(codegen: *CodeGen, inst: Hir.Index) Error!void {
    const hir = codegen.hir;
    var builder = codegen.builder;
    const data = hir.get(inst, .branch_double);

    const condition = codegen.resolveInst(data.condition);

    // TODO: branch expressions
    const prev = builder.getInsertBlock();
    const exec_true = builder.appendBlock("ifelse.true");
    builder.positionAtEnd(exec_true);
    _ = try codegen.block(data.exec_true);

    const false_prev = builder.getInsertBlock();
    const exec_false = builder.appendBlock("ifelse.false");
    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exec_false);
    builder.positionAtEnd(exec_false);
    _ = try codegen.block(data.exec_false);

    const exit = builder.appendBlock("ifelse.exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null)
        builder.addBranch(exit);
    if (c.LLVMGetBasicBlockTerminator(false_prev) == null) {
        builder.positionAtEnd(false_prev);
        builder.addBranch(exit);
    }

    builder.positionAtEnd(exit);
}

fn param(codegen: *CodeGen, inst: Hir.Index, index: u32) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .param);
    const param_str = try hir.interner.get(data.name);
    const param_ref = c.LLVMGetParam(codegen.builder.function, index);
    c.LLVMSetValueName2(param_ref, param_str.ptr, param_str.len);
    return param_ref;
}

fn zext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .zext);
    const ty = try hir.resolveType(codegen.arena, data.ty);
    const ref = codegen.resolveInst(data.val);
    return codegen.builder.addZext(ty, ref);
}

fn sext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .sext);
    const ty = try hir.resolveType(codegen.arena, data.ty);
    const ref = codegen.resolveInst(data.val);
    return codegen.builder.addSext(ty, ref);
}

fn fpext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .fpext);
    const ty = try hir.resolveType(codegen.arena, data.ty);
    const ref = codegen.resolveInst(data.val);
    return codegen.builder.addFpext(ty, ref);
}

fn yield(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    return codegen.resolveInst(data.operand);
}

fn loop(codegen: *CodeGen, inst: Hir.Index) !void {
    const hir = codegen.hir;
    var builder = codegen.builder;
    const data = hir.get(inst, .loop);

    const prev = builder.getInsertBlock();
    const entry_block = builder.appendBlock("loop.entry");
    builder.positionAtEnd(entry_block);
    _ = try codegen.block(data.body);

    const condition_block = builder.appendBlock("loop.cond");
    builder.addBranch(condition_block);
    builder.positionAtEnd(prev);
    builder.addBranch(condition_block);
    builder.positionAtEnd(condition_block);
    const condition_ref = try codegen.block(data.condition);

    const exit_block = builder.appendBlock("loop.exit");
    builder.addCondBranch(condition_ref, entry_block, exit_block);

    builder.positionAtEnd(exit_block);
}

fn call(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Call);

    const addr = codegen.resolveInst(data.ptr);
    const args = try codegen.arena.alloc(c.LLVMValueRef, data.args_end - data.args_start);
    defer codegen.arena.free(args);
    const hir_args = hir.extra_data[data.args_start..data.args_end];
    for (hir_args, 0..) |arg, i| {
        args[i] = codegen.resolveInst(arg);
    }

    const ty = try hir.resolveType(codegen.arena, data.ptr);
    return codegen.builder.addCall(ty, addr, args);
}

fn ret(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);

    const is_none = hir.insts.items(.tag)[data.operand] == .none;
    const ref = if (is_none) null else codegen.resolveInst(data.operand);
    return codegen.builder.addReturn(ref);
}

fn logNot(codegen: *CodeGen, inst: Hir.Index) c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .log_not);
    const ref = codegen.resolveInst(data.operand);
    const u1_type = c.LLVMInt1TypeInContext(codegen.builder.context.context);
    return codegen.builder.addCmp(.icmp_eq, ref, c.LLVMConstInt(u1_type, 0, 0));
}
