const std = @import("std");
const Context = @import("Context.zig");
const Air = @import("../air/Air.zig");
const Type = @import("../air/type.zig").Type;
const InternPool = @import("../InternPool.zig");
const Scope = @import("Scope.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;
const ValueHandle = InternPool.ValueHandle;

const CodeGen = @This();

arena: Allocator,
builder: *Context.Builder,
pool: *InternPool,
air: *const Air,
scratch: std.ArrayListUnmanaged(c.LLVMValueRef),
map: std.AutoHashMapUnmanaged(Air.Index, c.LLVMValueRef),
// demoted_insts: std.AutoHashMapUnmanaged(Air.Index, c.LLVMValueRef),
lazy: std.AutoHashMapUnmanaged(Air.Index, []c.LLVMValueRef),
scope: *Scope,
discopes: std.ArrayListUnmanaged(c.LLVMMetadataRef),

const Error = Allocator.Error || error{ NotImplemented, NoSpaceLeft };

pub fn generate(self: *CodeGen) !void {
    var builder = self.builder;

    const entry = builder.appendBlock("entry");
    var function_scope = Scope.Function.init(entry);
    self.scope = &function_scope.base;

    builder.positionAtEnd(entry);
    _ = try self.block(self.air.toplevel);
}

pub fn deinit(self: *CodeGen) void {
    self.builder.deinit();
    self.scratch.deinit(self.arena);
    self.map.deinit(self.arena);
}

fn resolveInst(self: *CodeGen, inst: Air.Index) c.LLVMValueRef {
    const air = self.air;
    switch (air.instTag(inst)) {
        // dereference through the load decl and return the decl itself
        .load_decl => {
            const load_decl = air.instData(inst).load_decl;
            const decl_index = self.pool.indexToKey(load_decl.ip_index).decl;
            return self.builder.context.resolveDecl(decl_index);
        },
        else => {
            // the instruction should exist
            if (self.map.get(inst)) |ref| {
                return ref;
            } else {
                std.log.err("self: unable to resolve instruction %{}\n", .{inst});
                unreachable;
            }
        },
    }
}

fn elideLazy(self: *CodeGen, inst: Air.Index) Air.Index {
    switch (self.air.instData(inst)) {
        .load_lazy => |lazy| {
            for (self.lazy.get(inst).?) |elide| {
                c.LLVMInstructionEraseFromParent(elide);
            }
            return lazy.ptr;
        },
        else => return inst,
    }
}

fn block(self: *CodeGen, block_inst: Air.Index) Error!c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(block_inst).block;
    const slice = air.extraData(Air.Inst.ExtraSlice, data.insts);
    const insts = air.extraSlice(slice);

    // TODO: move this to block scope struct
    const after_block = self.builder.appendBlock("yield.exit");
    var yield_val: c.LLVMValueRef = null;
    var yield_jump: bool = false;
    var block_scope = Scope.Block.init(self.scope, self.arena);
    defer block_scope.deinit();

    for (insts) |i| {
        const inst: Air.Index = @enumFromInt(i);
        const loc = self.air.instLoc(inst);
        // std.debug.print("{}: [{}:{}]\n", .{ air.instTag(inst), loc.line, loc.col });
        const discope = self.discopes.items[self.discopes.items.len - 1];
        const diloc = c.LLVMDIBuilderCreateDebugLocation(
            self.builder.context.context,
            loc.line,
            loc.col,
            discope,
            null,
        );

        c.LLVMSetCurrentDebugLocation2(self.builder.builder, diloc);
        _ = switch (air.instTag(inst)) {
            .constant => try self.constant(inst),

            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            .bitwise_or,
            .bitwise_and,
            .bitwise_xor,
            .lsl,
            .lsr,
            .asl,
            .asr,
            => |tag| try self.binaryOp(inst, tag),
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
            => |tag| try self.cmp(inst, tag),
            .neg => try self.neg(inst),
            .logical_not => try self.logicalNot(inst),
            .bitwise_inv => unreachable, // TODO

            .call => try self.call(inst),

            .zext => try self.zext(inst),
            .sext => try self.sext(inst),
            .fpext => try self.fpext(inst),
            .reftoptr => try self.reftoptr(inst),
            .ptrtoref => try self.ptrtoref(inst),

            .slice_init => try self.sliceInit(inst),
            .array_init => try self.arrayInit(inst),
            .struct_init => try self.structInit(inst),
            .index_ref => try self.indexRef(inst),
            .index_val => try self.indexVal(inst),
            .slice_ptr_val => try self.slicePtrVal(inst),
            .slice_len_val => try self.sliceLenVal(inst),
            .field_val => try self.fieldVal(inst),

            .alloc, .alloc_mut => try self.alloc(inst),
            .load => try self.load(inst),
            .load_lazy => try self.loadLazy(inst),
            .store => try self.store(inst),

            .branch_single => try self.branchSingle(inst),
            .branch_double => try self.branchDouble(inst),
            .loop_forever => try self.loopForever(inst),
            .loop_while => try self.loopWhile(inst),
            .@"return" => try self.ret(inst),
            .yield => {
                yield_val = try self.yield(inst);
                continue;
            },
            .@"break" => try self.controlBreak(inst),
            .@"continue" => try self.controlContinue(inst),
            .block => try self.block(inst),

            .param => try self.param(inst, @intCast(i)),
            .load_decl => {},

            .dbg_block_begin => try self.dbgBlockBegin(inst),
            .dbg_block_end => try self.dbgBlockEnd(inst),
            .dbg_var_val => try self.dbgVarVal(inst),
            .dbg_var_ptr => try self.dbgVarPtr(inst),
            // .slice_ptr_ref => try self.slicePtrRef(inst),
            // .slice_len_ref => try self.sliceLenRef(inst),
            // .slice_ptr_val => try self.slicePtrVal(inst),
            // .slice_len_val => try self.sliceLenVal(inst),
        };
    }

    if (yield_jump) {
        self.builder.addBranch(after_block);
        self.builder.positionAtEnd(after_block);
    } else {
        self.builder.deleteBlock(after_block);
    }

    return yield_val;
}

fn constant(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const builder = self.builder;
    const data = self.air.instData(inst).constant;
    const tv = self.pool.indexToKey(data).tv;
    // TODO: we should have a better way to either mark as unused or complete discard
    // these intermediate instructions, but for now we just skip them
    switch (self.pool.indexToKey(tv.ty).ty) {
        .comptime_int,
        .comptime_float,
        => return null,
        .void => {
            const ty = c.LLVMVoidTypeInContext(builder.context.context);
            const val = c.LLVMGetUndef(ty);
            try self.map.put(self.arena, inst, val);
            return val;
        },
        .int,
        .float,
        .bool,
        => {
            const val = try builder.context.resolveTv(tv);
            try self.map.put(self.arena, inst, val);
            return val;
        },
        .array => |array| {
            // llvm doesn't like immediate aggregates, so we quietly
            // store this in .data by creating an unnamed decl
            switch (self.pool.indexToType(array.element)) {
                .comptime_int,
                .comptime_float,
                => return null,
                else => {
                    const val = try builder.context.resolveTv(tv);
                    const global = try builder.context.addGlobal("", .{ .array = array });
                    c.LLVMSetGlobalConstant(global, 1);
                    c.LLVMSetInitializer(global, val);
                    c.LLVMSetUnnamedAddress(global, c.LLVMGlobalUnnamedAddr);
                    try self.map.put(self.arena, inst, global);
                    return global;
                },
            }
        },
        else => unreachable, // unimplemented
    }
}

fn binaryOp(self: *CodeGen, inst: Air.Index, comptime tag: Air.Inst.Tag) !c.LLVMValueRef {
    const air = self.air;
    const data = @field(air.instData(inst), @tagName(tag));
    const l = self.resolveInst(data.l);
    const r = self.resolveInst(data.r);
    var builder = self.builder;

    // C (clang 17) emits urem for all integer modulo
    // in the long term, we need to decide what behavior we want for
    // negative numbers and 0
    const lty = self.air.typeOf(data.l);
    const ref = switch (self.pool.indexToKey(lty).ty) {
        .int => |int| switch (int.sign) {
            .unsigned => switch (tag) {
                .add => c.LLVMBuildAdd(builder.builder, l, r, ""),
                .sub => c.LLVMBuildSub(builder.builder, l, r, ""),
                .mul => c.LLVMBuildMul(builder.builder, l, r, ""),
                .div => c.LLVMBuildUDiv(builder.builder, l, r, ""),
                .mod => c.LLVMBuildURem(builder.builder, l, r, ""),
                .lsl, .asl => c.LLVMBuildShl(builder.builder, l, r, ""),
                .lsr => c.LLVMBuildLShr(builder.builder, l, r, ""),
                .asr => c.LLVMBuildAShr(builder.builder, l, r, ""),
                .bitwise_or => c.LLVMBuildOr(builder.builder, l, r, ""),
                .bitwise_and => c.LLVMBuildAnd(builder.builder, l, r, ""),
                .bitwise_xor => c.LLVMBuildXor(builder.builder, l, r, ""),
                else => unreachable,
            },
            .signed => switch (tag) {
                .add => c.LLVMBuildAdd(builder.builder, l, r, ""),
                .sub => c.LLVMBuildSub(builder.builder, l, r, ""),
                .mul => c.LLVMBuildMul(builder.builder, l, r, ""),
                .div => c.LLVMBuildSDiv(builder.builder, l, r, ""),
                .mod => c.LLVMBuildURem(builder.builder, l, r, ""),
                .lsl, .asl => c.LLVMBuildShl(builder.builder, l, r, ""),
                .lsr => c.LLVMBuildLShr(builder.builder, l, r, ""),
                .asr => c.LLVMBuildAShr(builder.builder, l, r, ""),
                .bitwise_or => c.LLVMBuildOr(builder.builder, l, r, ""),
                .bitwise_and => c.LLVMBuildAnd(builder.builder, l, r, ""),
                .bitwise_xor => c.LLVMBuildXor(builder.builder, l, r, ""),
                else => unreachable,
            },
        },
        .float => switch (tag) {
            .add => c.LLVMBuildFAdd(builder.builder, l, r, ""),
            .sub => c.LLVMBuildFSub(builder.builder, l, r, ""),
            .mul => c.LLVMBuildFMul(builder.builder, l, r, ""),
            .div => c.LLVMBuildFDiv(builder.builder, l, r, ""),
            .mod => c.LLVMBuildFRem(builder.builder, l, r, ""),
            else => unreachable,
        },
        .bool => switch (tag) {
            .bitwise_or => c.LLVMBuildOr(builder.builder, l, r, ""),
            .bitwise_and => c.LLVMBuildAnd(builder.builder, l, r, ""),
            .bitwise_xor => c.LLVMBuildXor(builder.builder, l, r, ""),
            else => unreachable,
        },
        else => unreachable,
    };

    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn alloc(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const slot_type = switch (self.air.instData(inst)) {
        inline .alloc, .alloc_mut => |data| data.slot_type,
        else => unreachable,
    };
    const ty = self.pool.indexToKey(slot_type).ty;

    const ref = try self.allocaInner(ty);
    try self.map.put(self.arena, inst, ref);

    // TODO: cache this
    // const name = "llvm.lifetime.start";
    // const id = c.LLVMLookupIntrinsicID(name.ptr, name.len);
    // const context = self.builder.context;
    // const params: []const c.LLVMTypeRef = &.{ c.LLVMIntTypeInContext(context.context, 64), c.LLVMPointerTypeInContext(context.context, 0) };
    // // const decl = c.LLVMGetIntrinsicDeclaration(context.module, id, @constCast(params.ptr), params.len);
    // // _ = params;
    // const decl = c.LLVMGetIntrinsicDeclaration(context.module, id, @constCast(params.ptr), 0);
    // // _ = decl;
    // const decl_type = c.LLVMIntrinsicGetType(context.context, id, @constCast(params.ptr), params.len);
    // const args: []const c.LLVMValueRef = &.{ try self.builder.addUint(Type.u64_type, ty.size(self.pool).?), ref };
    // _ = c.LLVMBuildCall2(self.builder.builder, decl_type, decl, @constCast(args.ptr), @intCast(args.len), "");
    return ref;
}

fn allocaInner(self: *CodeGen, ty: Type) !c.LLVMValueRef {
    const builder = self.builder;
    const prev = builder.getInsertBlock();
    // LLVM prefers allocas to be at the beginning to participate in mem2reg optimization
    const func = self.scope.resolve(.function).cast(Scope.Function).?;
    const insert_loc = if (func.prev_alloca) |loc| c.LLVMGetNextInstruction(loc) else c.LLVMGetFirstInstruction(func.entry);
    if (insert_loc) |loc| {
        c.LLVMPositionBuilder(builder.builder, func.entry, loc);
    } else {
        builder.positionAtEnd(func.entry);
    }

    const ref = try builder.addAlloca(ty);
    func.prev_alloca = ref;
    builder.positionAtEnd(prev);
    return ref;
}

fn load(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).load;
    const ptr = self.resolveInst(data.ptr);
    const ptr_type = self.pool.indexToType(air.typeOf(data.ptr)).ref;
    const load_type = self.pool.indexToType(ptr_type.pointee);
    const ref = switch (load_type) {
        // shhh! these are aggregates, we only pretend to load them
        // what we actually do is copy them into a new aggregate, since
        // we're emulating loading their value into a temporary (but
        // the temporary is actually an alloca)
        // TODO: in the future, we should try to elide as many of these
        // copies as we can
        .array, .slice, .@"struct" => ref: {
            const alloca = try self.allocaInner(load_type);
            _ = try self.builder.addMemcpy(load_type, alloca, ptr);
            break :ref alloca;
        },
        else => try self.builder.addLoad(ptr, load_type),
    };
    // const ref = try self.builder.addLoad(ptr, load_type);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn loadLazy(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).load_lazy;
    const ptr = self.resolveInst(data.ptr);
    const ptr_type = self.pool.indexToType(air.typeOf(data.ptr)).ref;
    const load_type = self.pool.indexToType(ptr_type.pointee);
    const ref = switch (load_type) {
        // shhh! these are aggregates, we only pretend to load them
        // what we actually do is copy them into a new aggregate, since
        // we're emulating loading their value into a temporary (but
        // the temporary is actually an alloca)
        // TODO: in the future, we should try to elide as many of these
        // copies as we can
        .array, .slice, .@"struct" => ref: {
            const alloca = try self.allocaInner(load_type);
            const memcpy = try self.builder.addMemcpy(load_type, alloca, ptr);

            const insts = try self.arena.alloc(c.LLVMValueRef, 2);
            insts[0] = alloca;
            insts[1] = memcpy;
            try self.lazy.put(self.arena, inst, insts);

            break :ref alloca;
        },
        else => try self.builder.addLoad(ptr, load_type),
    };
    // const ref = try self.builder.addLoad(ptr, load_type);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn store(self: *CodeGen, inst: Air.Index) !void {
    const air = self.air;
    const builder = self.builder;
    const data = air.instData(inst).store;
    const addr = self.resolveInst(data.ptr);
    var val = self.resolveInst(data.val);
    // special behavior for aggregates to avoid storing the entire value at once
    // LLVM prefers initializing each element/field separately, and we can also
    // perform other optimizations
    const val_type = self.pool.indexToType(air.typeOf(data.val));
    switch (val_type) {
        .array, .@"struct" => _ = try builder.addMemcpy(val_type, addr, self.resolveInst(self.elideLazy(data.val))),
        .slice => |slice| {
            // unroll a memcpy
            var indices = .{
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // first element (ptr)
            };
            var gep = try builder.addGetElementPtr(val_type, val, &indices);
            // TODO: should this be mutable or not
            const ptr = try builder.addLoad(gep, .{ .pointer = .{ .pointee = slice.element, .mutable = true } });
            gep = try builder.addGetElementPtr(val_type, addr, &indices);
            _ = builder.addStore(gep, ptr);

            indices = .{
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 1), // second element (len)
            };
            gep = try builder.addGetElementPtr(val_type, val, &indices);
            const len = try builder.addLoad(gep, Type.u64_type);
            gep = try builder.addGetElementPtr(val_type, addr, &indices);
            _ = builder.addStore(gep, len);
        },
        else => _ = builder.addStore(addr, val),
    }
}

fn cmp(self: *CodeGen, inst: Air.Index, comptime tag: Air.Inst.Tag) !c.LLVMValueRef {
    const data = @field(self.air.instData(inst), @tagName(tag));
    const l = self.resolveInst(data.l);
    const r = self.resolveInst(data.r);
    const ref = self.builder.addCmp(tag, l, r);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

// lowers to a (0 - op)
fn neg(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const data = self.air.instData(inst).neg;
    const operand = self.resolveInst(data);
    const ty = self.pool.indexToType(self.air.typeOf(data));
    const zero = try self.builder.addUint(ty, 0);
    const sub = c.LLVMBuildSub(self.builder.builder, zero, operand, "");
    try self.map.put(self.arena, inst, sub);
    return sub;
}

// lowers to a u1 (op xor 1)
fn logicalNot(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const data = self.air.instData(inst).logical_not;
    const operand = self.resolveInst(data);
    const one = try self.builder.addUint(Type.bool_type, 1);
    const not = c.LLVMBuildXor(self.builder.builder, operand, one, "");
    try self.map.put(self.arena, inst, not);
    return not;
}

fn branchSingle(self: *CodeGen, inst: Air.Index) Error!c.LLVMValueRef {
    var builder = self.builder;
    const data = self.air.instData(inst).branch_single;

    const condition = self.resolveInst(data.cond);
    const exec_true = builder.appendBlock("if.true");
    const prev = builder.getInsertBlock();
    builder.positionAtEnd(exec_true);

    _ = try self.block(data.exec_true);
    const exit = builder.appendBlock("if.exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null) {
        builder.addBranch(exit);
    }

    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exit);
    builder.positionAtEnd(exit);
    return undefined;
}

fn branchDouble(self: *CodeGen, inst: Air.Index) Error!c.LLVMValueRef {
    const air = self.air;
    var builder = self.builder;
    const branch_double = air.instData(inst).branch_double;
    const data = air.extraData(Air.Inst.BranchDouble, branch_double.pl);

    const condition = self.resolveInst(branch_double.cond);

    const prev = builder.getInsertBlock();
    const exec_true = builder.appendBlock("ifelse.true");
    builder.positionAtEnd(exec_true);
    const yield_true = try self.block(data.exec_true);

    const false_prev = builder.getInsertBlock();
    const exec_false = builder.appendBlock("ifelse.false");
    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exec_false);
    builder.positionAtEnd(exec_false);
    const yield_false = try self.block(data.exec_false);

    const exit = builder.appendBlock("ifelse.exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null)
        builder.addBranch(exit);
    if (c.LLVMGetBasicBlockTerminator(false_prev) == null) {
        builder.positionAtEnd(false_prev);
        builder.addBranch(exit);
    }

    builder.positionAtEnd(exit);
    if ((yield_true == null) or (yield_false == null)) return null;

    const yield_type = self.pool.indexToKey(air.typeOf(data.exec_true)).ty;
    const phi = try builder.addPhi(yield_type);
    var values = .{ yield_true, yield_false };
    var blocks = .{ exec_true, exec_false };
    c.LLVMAddIncoming(phi, @ptrCast(&values), @ptrCast(&blocks), 2);
    try self.map.put(self.arena, inst, phi);
    return phi;
}

fn param(self: *CodeGen, inst: Air.Index, index: u32) !c.LLVMValueRef {
    const data = self.air.instData(inst).param;
    const param_str = self.pool.getString(data.name).?;
    const ref = c.LLVMGetParam(self.builder.function, index);
    c.LLVMSetValueName2(ref, param_str.ptr, param_str.len);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn zext(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).zext;
    const ty = self.pool.indexToKey(data.ty).ty;
    const operand = self.resolveInst(data.operand);
    const ref = try self.builder.addZext(ty, operand);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn sext(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).sext;
    const ty = self.pool.indexToKey(data.ty).ty;
    const operand = self.resolveInst(data.operand);
    const ref = try self.builder.addSext(ty, operand);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn fpext(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).fpext;
    const ty = self.pool.indexToKey(data.ty).ty;
    const operand = self.resolveInst(data.operand);
    const ref = try self.builder.addFpext(ty, operand);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn reftoptr(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).reftoptr;
    const operand = self.resolveInst(data.operand);
    try self.map.put(self.arena, inst, operand);
    return operand;
}

fn ptrtoref(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).ptrtoref;
    const operand = self.resolveInst(data.operand);
    try self.map.put(self.arena, inst, operand);
    return operand;
}

fn yield(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    // TODO: this doesn't actually work with yields in the middle
    const data = self.air.instData(inst).yield;
    return self.resolveInst(data);
}

fn loopWhile(self: *CodeGen, inst: Air.Index) !void {
    // the canonical form of a loop in LLVM is a do-while
    // where the body is placed first, and then the condition, which
    // results in a branch back to the body or to the exit
    // finally, a jump is placed at the top (entry) that jumps unconditionally
    // to the branch to create "while" behavior rather than "do" while
    var builder = self.builder;
    const data = self.air.instData(inst).loop_while;

    const prev_block = builder.getInsertBlock();
    const entry_block = builder.appendBlock("loop.entry");
    const condition_block = builder.appendBlock("loop.cond");
    const exit_block = builder.appendBlock("loop.exit");
    var loop_scope = Scope.Loop.init(self.scope, condition_block, exit_block);
    self.scope = &loop_scope.base;
    defer self.scope = loop_scope.parent;

    builder.positionAtEnd(prev_block);
    builder.addBranch(condition_block);

    builder.positionAtEnd(entry_block);
    _ = try self.block(data.body);
    builder.addBranch(condition_block);
    // purely aesthetic
    c.LLVMMoveBasicBlockAfter(condition_block, builder.getInsertBlock());

    builder.positionAtEnd(condition_block);
    const condition_ref = try self.block(data.cond);
    builder.addCondBranch(condition_ref, entry_block, exit_block);
    // purely aesthetic
    c.LLVMMoveBasicBlockAfter(exit_block, builder.getInsertBlock());

    builder.positionAtEnd(exit_block);
}

fn loopForever(self: *CodeGen, inst: Air.Index) !void {
    // the forever loop can be generated much more simply, since there isn't a
    // condition to check. we can therefore ignore the structure above in loopWhile
    // and simply emit a single basic block that unconditionally branches back to itself
    var builder = self.builder;
    const data = self.air.instData(inst).loop_forever;

    const prev_block = builder.getInsertBlock();
    const entry_block = builder.appendBlock("loop.entry");
    const exit_block = builder.appendBlock("loop.exit");
    var loop_scope = Scope.Loop.init(self.scope, entry_block, exit_block);
    self.scope = &loop_scope.base;
    defer self.scope = loop_scope.parent;

    builder.positionAtEnd(prev_block);
    builder.addBranch(entry_block);

    builder.positionAtEnd(entry_block);
    _ = try self.block(data.body);
    builder.addBranch(entry_block);
    // purely aesthetic
    c.LLVMMoveBasicBlockAfter(exit_block, builder.getInsertBlock());

    builder.positionAtEnd(exit_block);
}

fn controlBreak(self: *CodeGen, inst: Air.Index) !void {
    _ = inst;
    const loop_scope = self.scope.resolve(.loop).cast(Scope.Loop).?;
    self.builder.addBranch(loop_scope.exit);
}

fn controlContinue(self: *CodeGen, inst: Air.Index) !void {
    _ = inst;
    const loop_scope = self.scope.resolve(.loop).cast(Scope.Loop).?;
    self.builder.addBranch(loop_scope.entry);
}

fn call(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const call_inst = air.instData(inst).call;
    const slice = air.extraData(Air.Inst.ExtraSlice, call_inst.args);
    const air_args = air.extraSlice(slice);
    const function = self.resolveInst(call_inst.function);

    const scratch_top = self.scratch.items.len;
    defer self.scratch.shrinkRetainingCapacity(scratch_top);
    try self.scratch.ensureUnusedCapacity(self.arena, air_args.len);
    for (air_args) |air_arg| {
        const arg = self.resolveInst(@enumFromInt(air_arg));
        self.scratch.appendAssumeCapacity(arg);
    }

    const args = self.scratch.items[scratch_top..];
    // const ty = self.pool.indexToKey(air.typeOf(call_inst.function)).ty;
    const pointer_type = self.pool.indexToType(air.typeOf(call_inst.function)).pointer;
    const function_type = self.pool.indexToType(pointer_type.pointee);
    const ref = try self.builder.addCall(function_type, function, args);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn ret(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const data = air.instData(inst).@"return";
    const ty = self.pool.indexToKey(air.typeOf(data)).ty;
    const val = switch (ty) {
        .void => null,
        else => self.resolveInst(data),
    };
    const ref = self.builder.addReturn(val);
    try self.map.put(self.arena, inst, ref);
    return ref;
}

fn indexRef(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const index_ref = air.instData(inst).index_ref;
    const data = air.extraData(Air.Inst.IndexRef, index_ref.pl);

    const base = self.resolveInst(data.base);
    const index = self.resolveInst(data.index);

    const base_type = self.pool.indexToType(air.typeOf(data.base)).ref;
    const pointee = self.pool.indexToType(base_type.pointee);
    switch (pointee) {
        .array => {
            // since arrays are actually stored in memory (even though this is value semantics)
            // we can ignore the "ref" to the base and just emit a GEP
            const deref = try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0);
            var indices = .{ deref, index };
            const gep = try builder.addGetElementPtr(pointee, base, &indices);
            try self.map.put(self.arena, inst, gep);
            return gep;
        },
        .many_pointer => unreachable, // TODO
        else => unreachable,
    }
}

fn indexVal(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const data = air.instData(inst).index_val;

    var base = self.resolveInst(self.elideLazy(data.base));
    const index = self.resolveInst(data.index);
    const base_type = self.pool.indexToKey(air.typeOf(data.base)).ty;
    switch (base_type) {
        .array => |array_type| {
            // since arrays are actually stored in memory (even though this is value semantics)
            // we have an extra level of indirection
            // hence we emit a GEP + load
            const element_type = self.pool.indexToKey(array_type.element).ty;
            const deref = try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0);
            var indices = .{ deref, index };
            const gep = try builder.addGetElementPtr(base_type, base, &indices);
            const access = try builder.addLoad(gep, element_type);
            try self.map.put(self.arena, inst, access);
            return access;
        },
        .slice => |slice_type| {
            // theres a lot of indirection here:
            // the slice object itself (which doesn't own the underlying memory), while using value semantics,
            // is still stored in memory since its an aggregate
            // so first we gep + load the ptr field from the slice
            // then, we use that ptr as the base to gep + load the element
            const element_type = self.pool.indexToKey(slice_type.element).ty;
            var indices = .{
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
                try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // first element (ptr)
            };
            const ptr_gep = try builder.addGetElementPtr(base_type, base, &indices);
            // TODO: should this be mutable or not
            const ptr = try builder.addLoad(ptr_gep, .{ .pointer = .{ .pointee = slice_type.element, .mutable = true } });
            var gep2_indices = .{index};
            const gep = try builder.addGetElementPtr(element_type, ptr, &gep2_indices);
            const access = try builder.addLoad(gep, element_type);
            try self.map.put(self.arena, inst, access);
            return access;
        },
        .many_pointer => unreachable, // TODO
        else => unreachable,
    }
}

fn slicePtrVal(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const data = air.instData(inst).slice_ptr_val;

    const base = self.resolveInst(data.base);
    const base_type = self.pool.indexToType(air.typeOf(data.base));
    const ptr_type = self.pool.indexToType(data.ty);
    const indices = .{
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // first element (ptr)
    };
    const gep = try builder.addGetElementPtr(base_type, base, &indices);
    const access = try builder.addLoad(gep, ptr_type);
    try self.map.put(self.arena, inst, access);
    return access;
}

fn sliceLenVal(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const data = air.instData(inst).slice_len_val;

    const base = self.resolveInst(data.base);
    const base_type = self.pool.indexToType(air.typeOf(data.base));
    const len_type = self.pool.indexToType(data.ty);
    const indices = .{
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 1), // first element (len)
    };
    const gep = try builder.addGetElementPtr(base_type, base, &indices);
    const access = try builder.addLoad(gep, len_type);
    try self.map.put(self.arena, inst, access);
    return access;
}

fn fieldVal(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const data = air.instData(inst).field_val;

    const base = self.resolveInst(self.elideLazy(data.base));
    const base_type = self.pool.indexToType(air.typeOf(data.base));
    const field_type = self.pool.indexToType(air.typeOf(inst));

    const field_map_index = self.pool.indexToKey(base_type.@"struct".names).field_map;
    const field_map = self.pool.fields.at(@intFromEnum(field_map_index));
    const slot_index = field_map.get(data.name).?;
    const indices = .{
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, slot_index), // ith field
    };
    const gep = try builder.addGetElementPtr(base_type, base, &indices);
    const access = try builder.addLoad(gep, field_type);
    try self.map.put(self.arena, inst, access);
    return access;
}

fn sliceInit(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const slice_init = air.instData(inst).slice_init;
    const data = air.extraData(Air.Inst.SliceInit, slice_init.pl);

    const ptr = self.resolveInst(data.ptr);
    const len = self.resolveInst(data.len);
    const slice_type = self.pool.indexToKey(air.typeOf(inst)).ty;
    // aggregate, so should be stored on the stack
    const alloca = try self.allocaInner(slice_type);

    // now insert both values in using 2x (gep + store)
    // const ptr_type = self.pool.indexToKey(slice_type.slice.element).ty;
    var indices = .{
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // first element (ptr)
    };
    var gep = try builder.addGetElementPtr(slice_type, alloca, &indices);
    _ = builder.addStore(gep, ptr);
    indices = .{
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
        try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 1), // second element (len)
    };
    gep = try builder.addGetElementPtr(slice_type, alloca, &indices);
    _ = builder.addStore(gep, len);

    try self.map.put(self.arena, inst, alloca);
    return alloca;
}

fn arrayInit(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const array_init = air.instData(inst).array_init;
    const slice = air.extraData(Air.Inst.ExtraSlice, array_init.elements);
    const elements = air.extraSlice(slice);

    // aggregate, so should be stored on the stack
    const array_type = self.pool.indexToType(air.typeOf(inst));
    const alloca = try self.allocaInner(array_type);

    // insert each element with a gep + store
    for (elements, 0..) |element, i| {
        const val = self.resolveInst(@enumFromInt(element));
        const indices = .{
            try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
            try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, i), // ith element
        };
        const gep = try builder.addGetElementPtr(array_type, alloca, &indices);
        c.LLVMSetIsInBounds(gep, @intFromBool(true));
        _ = builder.addStore(gep, val);
    }

    try self.map.put(self.arena, inst, alloca);
    return alloca;
}

fn structInit(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const builder = self.builder;
    const struct_init = air.instData(inst).struct_init;
    const slice = air.extraData(Air.Inst.ExtraSlice, struct_init.fields);
    const fields = air.extraSlice(slice);

    // aggregate, so should be stored on the stack
    const struct_type = self.pool.indexToType(air.typeOf(inst));
    const alloca = try self.allocaInner(struct_type);

    // insert each field with a gep + store - ensured to be in order
    for (fields, 0..) |field, i| {
        const val = self.resolveInst(@enumFromInt(field));
        const indices = .{
            try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, 0), // deref
            try builder.addUint(.{ .int = .{ .sign = .unsigned, .width = 32 } }, i), // ith field
        };
        const gep = try builder.addGetElementPtr(struct_type, alloca, &indices);
        c.LLVMSetIsInBounds(gep, @intFromBool(true));
        _ = builder.addStore(gep, val);
    }

    try self.map.put(self.arena, inst, alloca);
    return alloca;
}
//
// fn slicePtrRef(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
//     const air = self.air;
//     const data = air.get(inst, .slice_ptr_ref);
//
//     const ty = try air.resolveType(self.arena, data.operand);
//     const slice_type = ty.extended.cast(Type.Pointer).?.pointee;
//     const slice = self.resolveInst(data.operand);
//
//     const scratch_top = self.scratch.items.len;
//     defer self.scratch.shrinkRetainingCapacity(scratch_top);
//     const indices = try self.scratch.addManyAsSlice(self.arena, 2);
//     indices[0] = try self.builder.addUint(Type.Common.u32_type, 0);
//     indices[1] = try self.builder.addUint(Type.Common.u32_type, 0);
//
//     const gep = try self.builder.addGetElementPtr(slice_type, slice, indices);
//     c.LLVMSetIsInBounds(gep, 1);
//     return gep;
// }
//
// fn sliceLenRef(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
//     const air = self.air;
//     const data = air.get(inst, .slice_len_ref);
//
//     const ty = try air.resolveType(self.arena, data.operand);
//     const slice_type = ty.extended.cast(Type.Pointer).?.pointee;
//     const slice = self.resolveInst(data.operand);
//
//     const scratch_top = self.scratch.items.len;
//     defer self.scratch.shrinkRetainingCapacity(scratch_top);
//     const indices = try self.scratch.addManyAsSlice(self.arena, 2);
//     indices[0] = try self.builder.addUint(Type.Common.u32_type, 0);
//     indices[1] = try self.builder.addUint(Type.Common.u32_type, 1);
//
//     const gep = try self.builder.addGetElementPtr(slice_type, slice, indices);
//     c.LLVMSetIsInBounds(gep, 1);
//     return gep;
// }
//

fn dbgBlockBegin(self: *CodeGen, inst: Air.Index) !void {
    const air = self.air;
    const di = self.builder.context.di;
    const loc = air.instLoc(inst);

    const parent = self.discopes.items[self.discopes.items.len - 1];
    const discope = c.LLVMDIBuilderCreateLexicalBlock(
        di.builder,
        parent,
        di.file,
        loc.line,
        loc.col,
    );
    try self.discopes.append(self.arena, discope);
}

fn dbgBlockEnd(self: *CodeGen, inst: Air.Index) !void {
    _ = inst;
    _ = self.discopes.pop();
}

fn dbgVarVal(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const di = self.builder.context.di;
    const dbg = air.instData(inst).dbg_var_val;
    const loc = air.instLoc(inst);

    const name = self.pool.getString(dbg.name).?;
    const val_type = self.pool.indexToType(air.typeOf(dbg.val));
    const val = self.resolveInst(dbg.val);
    const discope = self.discopes.items[self.discopes.items.len - 1];
    const variable = c.LLVMDIBuilderCreateAutoVariable(
        di.builder,
        discope,
        name.ptr,
        name.len,
        di.file,
        loc.line,
        try di.resolveType(val_type),
        @intFromBool(false),
        c.LLVMDIFlagZero,
        @intCast(val_type.alignment(self.pool).?), // TODO: no clue if this is correct
    );
    const diloc = c.LLVMDIBuilderCreateDebugLocation(
        self.builder.context.context,
        loc.line,
        loc.col,
        discope,
        null,
    );
    const dival = c.LLVMDIBuilderInsertDbgValueAtEnd(
        di.builder,
        val,
        variable,
        c.LLVMDIBuilderCreateExpression(di.builder, null, 0),
        diloc,
        self.builder.getInsertBlock(),
    );

    try self.map.put(self.arena, inst, dival);
    return dival;
}

fn dbgVarPtr(self: *CodeGen, inst: Air.Index) !c.LLVMValueRef {
    const air = self.air;
    const di = self.builder.context.di;
    const dbg = air.instData(inst).dbg_var_ptr;
    const loc = air.instLoc(inst);

    const name = self.pool.getString(dbg.name).?;
    const alloc_type = self.pool.indexToType(air.typeOf(dbg.ptr)).ref;
    const slot_type = self.pool.indexToType(alloc_type.pointee);
    const alloca = self.resolveInst(dbg.ptr);
    const alignment = c.LLVMGetAlignment(alloca);
    const discope = self.discopes.items[self.discopes.items.len - 1];
    const variable = c.LLVMDIBuilderCreateAutoVariable(
        di.builder,
        discope,
        name.ptr,
        name.len,
        di.file,
        loc.line,
        try di.resolveType(slot_type),
        @intFromBool(false),
        c.LLVMDIFlagZero,
        alignment,
    );
    const diloc = c.LLVMDIBuilderCreateDebugLocation(
        self.builder.context.context,
        loc.line,
        loc.col,
        discope,
        null,
    );
    const declare = c.LLVMDIBuilderInsertDeclareAtEnd(
        di.builder,
        alloca,
        variable,
        c.LLVMDIBuilderCreateExpression(di.builder, null, 0),
        diloc,
        self.builder.getInsertBlock(),
    );

    try self.map.put(self.arena, inst, declare);
    return declare;
}
