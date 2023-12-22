const std = @import("std");
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const Hir = @import("../Hir.zig");
const Type = @import("../hir/type.zig").Type;
const InternPool = @import("../InternPool.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;
const ValueHandle = InternPool.ValueHandle;

const CodeGen = @This();

arena: Allocator,
builder: *Context.Builder,
hir: *const Hir,
func: *const Value.Function,
scratch: std.ArrayListUnmanaged(c.LLVMValueRef),
// TODO: is LLVM thread safe?
map: std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
global_map: *std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
value_map: std.AutoHashMapUnmanaged(ValueHandle, c.LLVMValueRef),

const Error = Allocator.Error || @import("../interner.zig").Error || error{NotImplemented};

pub fn generate(codegen: *CodeGen) !void {
    var builder = codegen.builder;

    const entry = builder.appendBlock("entry");
    builder.positionAtEnd(entry);
    _ = try codegen.block(codegen.func.body);
}

pub fn deinit(codegen: *CodeGen) void {
    codegen.builder.deinit();
    codegen.scratch.deinit(codegen.arena);
}

fn resolveInst(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    switch (hir.insts.items(.tag)[inst]) {
        .load_global => {
            const load_global = hir.get(inst, .load_global);
            // TODO: we can probably remove global map and just use llvm to look it up
            // via the name
            // but should we?
            return codegen.global_map.get(load_global.operand).?;
        },
        // .constant => {
        //     // constants aren't generated until someone uses them, so
        //     // take the time to emit them now and then return
        //     // this is actually out of date
        //     const ref = try codegen.constant(inst);
        //     try codegen.map.put(codegen.arena, inst, ref);
        //     return ref;
        // },
        else => {
            // the instruction should exist
            if (codegen.map.get(inst)) |ref| {
                return ref;
            } else {
                std.log.err("codegen: unable to resolve instruction %{}\n", .{inst});
                unreachable;
            }
        },
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
            .bitwise_or,
            .bitwise_and,
            .bitwise_xor,
            .lsl,
            .lsr,
            .asl,
            .asr,
            => |tag| try codegen.binaryOp(inst, tag),
            inline .ret_node, .ret_implicit => |tag| try codegen.ret(inst, tag),
            .alloca => try codegen.alloca(inst),
            .load => try codegen.load(inst),
            .store => try codegen.store(inst),
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
            .push => unreachable,
            .zext => try codegen.zext(inst),
            .sext => try codegen.sext(inst),
            .fpext => try codegen.fpext(inst),
            .param => try codegen.param(inst, @intCast(i)),
            .call => try codegen.call(inst),
            .index_ref => try codegen.indexRef(inst),
            .index_val => try codegen.indexVal(inst),
            .slice_ptr_ref => try codegen.slicePtrRef(inst),
            .slice_len_ref => try codegen.sliceLenRef(inst),
            .slice_ptr_val => try codegen.slicePtrVal(inst),
            .slice_len_val => try codegen.sliceLenVal(inst),
            .branch_single => {
                try codegen.branchSingle(inst);
                continue;
            },
            .branch_double => try codegen.branchDouble(inst),
            .ty, .load_global, .array => continue,
            // .array_init => try codegen.arrayInit(inst),
            .array_init => continue,
            .slice_init => continue,
            inline .yield_implicit, .yield_node => |tag| {
                yield_val = try codegen.yield(inst, tag);
                break;
            },
            .loop => {
                try codegen.loop(inst);
                continue;
            },
            .block => try codegen.block(inst),
            .none => continue,
            else => {
                std.log.err("codegen: unexpected inst: {}", .{hir.insts.items(.tag)[inst]});
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

    const ref = try codegen.constantInner(ty, hir.pool.getValue(data.val));
    try codegen.value_map.put(codegen.arena, data.val, ref);
    return ref;
}

fn constantInner(codegen: *CodeGen, ty: Type, value: Value) !c.LLVMValueRef {
    const hir = codegen.hir;
    var builder = codegen.builder;
    switch (ty.kind()) {
        .comptime_uint,
        .comptime_sint,
        .comptime_float,
        .comptime_array,
        => return null,
        .uint => return builder.addUint(ty, value.integer),
        .sint => return builder.addSint(ty, value.integer),
        .float => return builder.addFloat(ty, @bitCast(value.float)),
        .array => {
            const element_type = ty.extended.cast(Type.Array).?.element;
            const src_elements = value.array.elements;

            const scratch_top = codegen.scratch.items.len;
            defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
            try codegen.scratch.ensureUnusedCapacity(codegen.arena, src_elements.len);
            for (src_elements) |element| {
                const element_value = hir.pool.getValue(element);
                const dest_element = try codegen.constantInner(element_type, element_value);
                codegen.scratch.appendAssumeCapacity(dest_element);
            }

            const dest_elements = codegen.scratch.items[scratch_top..];
            const llvm_type = try builder.convertType(ty);
            const llvm_array = try builder.addConstArray(ty, dest_elements);
            const global = builder.context.addGlobal(".data", llvm_type);
            c.LLVMSetInitializer(global, llvm_array);
            c.LLVMSetGlobalConstant(global, 1);
            c.LLVMSetLinkage(global, c.LLVMPrivateLinkage);
            c.LLVMSetUnnamedAddr(global, 1);
            return global;
        },
        .string => {
            const string = value.string;
            const literal = try hir.interner.get(string);
            const llvm_string = try builder.context.addConstString(literal);
            const llvm_type = c.LLVMTypeOf(llvm_string);
            const global = builder.context.addGlobal(".data", llvm_type);
            c.LLVMSetInitializer(global, llvm_string);
            c.LLVMSetGlobalConstant(global, 1);
            c.LLVMSetLinkage(global, c.LLVMPrivateLinkage);
            c.LLVMSetUnnamedAddr(global, 1);
            return global;
        },
        .slice => {
            // generate a slice value by starting with an undef
            // and inserting 2 values into it (ptr and len)
            const slice = value.slice;
            const llvm_type = try codegen.builder.convertType(ty);
            const undef = c.LLVMGetUndef(llvm_type);
            const ptr_value = codegen.value_map.get(slice.ptr).?;
            const len_type = Type.Common.u64_type;
            const len = hir.pool.getValue(slice.len);
            const len_value = try codegen.constantInner(len_type, len);
            const ptr = try codegen.builder.addInsertValue(undef, ptr_value, 0);
            return codegen.builder.addInsertValue(ptr, len_value, 1);
        },
        // switch (value) {
        //     .string => {
        //         const string = value.string;
        //         const literal = try hir.interner.get(string);
        //         const llvm_string = try builder.context.addConstString(literal);
        //         const llvm_type = c.LLVMTypeOf(llvm_string);
        //         const global = builder.context.addGlobal(".str", llvm_type);
        //         c.LLVMSetInitializer(global, llvm_string);
        //         c.LLVMSetGlobalConstant(global, 1);
        //         c.LLVMSetLinkage(global, c.LLVMPrivateLinkage);
        //         c.LLVMSetUnnamedAddr(global, 1);
        //         return global;
        //     },
        //     .slice => {
        //         const slice = value.slice;
        //         var elements: [2]c.LLVMValueRef = [1]c.LLVMValueRef{undefined} ** 2;
        //         const len_type = Type.Common.u64_type;
        //         const len = hir.pool.getValue(slice.len);
        //         elements[0] = codegen.value_map.get(slice.ptr).?;
        //         elements[1] = try codegen.constantInner(len_type, len);
        //         const llvm_slice = try builder.context.addConstStruct(&elements);
        //         const llvm_type = try builder.convertType(ty);
        //         const global = builder.context.addGlobal(".slice", llvm_type);
        //         c.LLVMSetInitializer(global, llvm_slice);
        //         c.LLVMSetGlobalConstant(global, 1);
        //         c.LLVMSetLinkage(global, c.LLVMPrivateLinkage);
        //         c.LLVMSetUnnamedAddr(global, 1);
        //         return global;
        //     },
        //     else => unreachable,
        // }
        // },
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    const lref = try codegen.resolveInst(data.lref);
    const rref = try codegen.resolveInst(data.rref);
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
            .lsl, .asl => return c.LLVMBuildShl(builder.builder, lref, rref, ""),
            .lsr => return c.LLVMBuildLShr(builder.builder, lref, rref, ""),
            .asr => return c.LLVMBuildAShr(builder.builder, lref, rref, ""),
            .bitwise_or => return c.LLVMBuildOr(builder.builder, lref, rref, ""),
            .bitwise_and => return c.LLVMBuildAnd(builder.builder, lref, rref, ""),
            .bitwise_xor => return c.LLVMBuildXor(builder.builder, lref, rref, ""),
            else => unreachable,
        },
        .sint => switch (tag) {
            .add => c.LLVMBuildAdd(builder.builder, lref, rref, ""),
            .sub => c.LLVMBuildSub(builder.builder, lref, rref, ""),
            .mul => c.LLVMBuildMul(builder.builder, lref, rref, ""),
            .div => c.LLVMBuildSDiv(builder.builder, lref, rref, ""),
            .mod => return c.LLVMBuildURem(builder.builder, lref, rref, ""),
            .lsl, .asl => return c.LLVMBuildShl(builder.builder, lref, rref, ""),
            .lsr => return c.LLVMBuildLShr(builder.builder, lref, rref, ""),
            .asr => return c.LLVMBuildAShr(builder.builder, lref, rref, ""),
            .bitwise_or => return c.LLVMBuildOr(builder.builder, lref, rref, ""),
            .bitwise_and => return c.LLVMBuildAnd(builder.builder, lref, rref, ""),
            .bitwise_xor => return c.LLVMBuildXor(builder.builder, lref, rref, ""),
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
    const ptr = try codegen.resolveInst(data.operand);
    return codegen.builder.addLoad(ptr, pointee_type);
}

fn store(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .store);
    // TODO: better way to check for comptime data
    const addr = try codegen.resolveInst(data.ptr);
    const ptr_inner_type = try hir.resolveType(codegen.arena, data.ptr);
    const ptr_type = ptr_inner_type.extended.cast(Type.Pointer).?.pointee;
    // const ty = try hir.resolveType(codegen.arena, data.val);
    // we lower different "stores" differently to avoid
    // creating a large store to an aggregate type
    switch (ptr_type.kind()) {
        .array => {
            // arrays can either be initialized from a comptime known Value
            // or runtime known .array_init instruction
            // for the former, we prefer to emit a memcpy from a static global
            // into the ptr
            // for the latter, we generate a series of insertvalue instructions
            switch (hir.insts.items(.tag)[data.val]) {
                // to initialize an array from a set of comptime values, the best
                // solution is to emit an unnamed constant global in the data section
                // (done by resolveInst above) and memcpy from it to the array pointer
                .constant => {
                    var val = try codegen.resolveInst(data.val);
                    return codegen.builder.addMemcpy(ptr_type, addr, val);
                },
                // to initialize an array from a set of runtime values, we loop
                // over each index in the array and emit a GEP + store
                // to avoid doing a large store to the entire array
                .array_init => {
                    const array_init = hir.get(data.val, .array_init);
                    const start = array_init.elements_start;
                    const end = array_init.elements_end;
                    const src_elements = hir.extra_data[start..end];

                    const scratch_top = codegen.scratch.items.len;
                    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
                    const indices = try codegen.scratch.addManyAsSlice(codegen.arena, 2);

                    for (src_elements, 0..) |src_element, i| {
                        indices[0] = try codegen.builder.addUint(Type.Common.u32_type, 0);
                        indices[1] = try codegen.builder.addUint(Type.Common.u32_type, i);
                        const element = try codegen.resolveInst(src_element);
                        const gep = try codegen.builder.addGetElementPtr(ptr_type, addr, indices);
                        c.LLVMSetIsInBounds(gep, 1);
                        _ = codegen.builder.addStore(gep, element);
                    }

                    return null;
                },
                else => unreachable,
            }
        },
        .slice => {
            // slices are initialized by adding two GEP + stores, even if the data
            // is comptime known, because it doesn't make sense to emit a 64/128 bit
            // "wide pointer" constant global and then memcpy from it
            switch (hir.insts.items(.tag)[data.val]) {
                .constant => {
                    const slice_constant = hir.get(data.val, .constant);
                    const slice = hir.pool.values.items(.data)[slice_constant.val].slice;

                    const scratch_top = codegen.scratch.items.len;
                    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
                    const indices = try codegen.scratch.addManyAsSlice(codegen.arena, 2);
                    indices[0] = try codegen.builder.addUint(Type.Common.u32_type, 0);

                    // ptr
                    const ptr = codegen.value_map.get(slice.ptr).?;
                    indices[1] = try codegen.builder.addUint(Type.Common.u32_type, 0);
                    const gep1 = try codegen.builder.addGetElementPtr(ptr_type, addr, indices);
                    c.LLVMSetIsInBounds(gep1, 1);
                    _ = codegen.builder.addStore(gep1, ptr);

                    // len
                    const slice_len = hir.pool.getValue(slice.len);
                    const len = try codegen.constantInner(Type.Common.u64_type, slice_len);
                    indices[1] = try codegen.builder.addUint(Type.Common.u32_type, 1);
                    const gep2 = try codegen.builder.addGetElementPtr(ptr_type, addr, indices);
                    c.LLVMSetIsInBounds(gep2, 1);
                    _ = codegen.builder.addStore(gep2, len);

                    return null;
                },
                .slice_init => {
                    const slice_init = hir.get(data.val, .slice_init);
                    const ptr_value = try codegen.resolveInst(slice_init.ptr);
                    const len_value = try codegen.resolveInst(slice_init.len);

                    // generate a slice value by starting with an undef
                    // and inserting 2 values into it (ptr and len)
                    const ty = try hir.resolveType(codegen.arena, data.val);
                    const llvm_type = try codegen.builder.convertType(ty);
                    const undef = c.LLVMGetUndef(llvm_type);
                    const ptr = try codegen.builder.addInsertValue(undef, ptr_value, 0);
                    return codegen.builder.addInsertValue(ptr, len_value, 1);
                },
                else => unreachable,
            }
        },
        else => {
            var val = try codegen.resolveInst(data.val);
            return codegen.builder.addStore(addr, val);
        },
    }
}

fn cmp(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    const lref = try codegen.resolveInst(data.lref);
    const rref = try codegen.resolveInst(data.rref);
    return codegen.builder.addCmp(tag, lref, rref);
}

fn branchSingle(codegen: *CodeGen, inst: Hir.Index) Error!void {
    const hir = codegen.hir;
    var builder = codegen.builder;
    const data = hir.get(inst, .branch_single);

    const condition = try codegen.resolveInst(data.condition);
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

fn branchDouble(codegen: *CodeGen, inst: Hir.Index) Error!c.LLVMValueRef {
    const hir = codegen.hir;
    var builder = codegen.builder;
    const data = hir.get(inst, .branch_double);

    const condition = try codegen.resolveInst(data.condition);

    // TODO: branch expressions
    const prev = builder.getInsertBlock();
    const exec_true = builder.appendBlock("ifelse.true");
    builder.positionAtEnd(exec_true);
    const yield_true = try codegen.block(data.exec_true);

    const false_prev = builder.getInsertBlock();
    const exec_false = builder.appendBlock("ifelse.false");
    builder.positionAtEnd(prev);
    builder.addCondBranch(condition, exec_true, exec_false);
    builder.positionAtEnd(exec_false);
    const yield_false = try codegen.block(data.exec_false);

    const exit = builder.appendBlock("ifelse.exit");
    if (c.LLVMGetBasicBlockTerminator(builder.getInsertBlock()) == null)
        builder.addBranch(exit);
    if (c.LLVMGetBasicBlockTerminator(false_prev) == null) {
        builder.positionAtEnd(false_prev);
        builder.addBranch(exit);
    }

    builder.positionAtEnd(exit);
    if ((yield_true == null) or (yield_false == null)) return null;

    const phi_type = try hir.resolveType(codegen.arena, data.exec_true);
    const phi = try builder.addPhi(phi_type);
    var incoming_values: [2]c.LLVMValueRef = [1]c.LLVMValueRef{undefined} ** 2;
    var incoming_blocks: [2]c.LLVMBasicBlockRef = [1]c.LLVMBasicBlockRef{undefined} ** 2;
    incoming_values[0] = yield_true;
    incoming_values[1] = yield_false;
    incoming_blocks[0] = exec_true;
    incoming_blocks[1] = exec_false;
    c.LLVMAddIncoming(phi, (&incoming_values).ptr, (&incoming_blocks).ptr, incoming_values.len);
    return phi;
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
    const ref = try codegen.resolveInst(data.val);
    return codegen.builder.addZext(ty, ref);
}

fn sext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .sext);
    const ty = try hir.resolveType(codegen.arena, data.ty);
    const ref = try codegen.resolveInst(data.val);
    return codegen.builder.addSext(ty, ref);
}

fn fpext(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .fpext);
    const ty = try hir.resolveType(codegen.arena, data.ty);
    const ref = try codegen.resolveInst(data.val);
    return codegen.builder.addFpext(ty, ref);
}

fn yield(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);
    return try codegen.resolveInst(data.operand);
}

fn loop(codegen: *CodeGen, inst: Hir.Index) !void {
    // the canonical form of a loop in LLVM is a do-while
    // where the body is placed first, and then the condition, which
    // results in a branch back to the body or to the exit
    // finally, a jump is placed at the top (entry) that jumps unconditionally
    // to the branch to create "while" behavior rather than "do" while
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

    const addr = try codegen.resolveInst(data.ptr);
    const hir_args = hir.extra_data[data.args_start..data.args_end];

    const scratch_top = codegen.scratch.items.len;
    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
    try codegen.scratch.ensureUnusedCapacity(codegen.arena, hir_args.len);
    for (hir_args) |arg| {
        codegen.scratch.appendAssumeCapacity(try codegen.resolveInst(arg));
    }

    const args = codegen.scratch.items[scratch_top..];
    const ty = try hir.resolveType(codegen.arena, data.ptr);
    return codegen.builder.addCall(ty, addr, args);
}

fn ret(codegen: *CodeGen, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, tag);

    const is_none = hir.insts.items(.tag)[data.operand] == .none;
    const ref = if (is_none) null else try codegen.resolveInst(data.operand);
    return codegen.builder.addReturn(ref);
}

fn indexRef(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .index_ref);

    const ty = try hir.resolveType(codegen.arena, data.array);
    const pointer_type = ty.extended.cast(Type.Pointer).?;
    const array_type = pointer_type.pointee;
    const array = try codegen.resolveInst(data.array);
    const access_index = try codegen.resolveInst(data.index);
    var indices: [2]c.LLVMValueRef = [1]c.LLVMValueRef{undefined} ** 2;
    indices[0] = try codegen.builder.addUint(Type.Common.u64_type, 0);
    indices[1] = access_index;
    const gep = try codegen.builder.addGetElementPtr(array_type, array, &indices);
    if (hir.insts.items(.tag)[data.index] == .constant) {
        // comptime known, so inbounds
        // TODO: better check and also make sure this is correct
        // and actually inbound
        c.LLVMSetIsInBounds(gep, 1);
    }
    return gep;
}

fn indexVal(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .index_val);

    const array_type = try hir.resolveType(codegen.arena, data.array);
    const array = try codegen.resolveInst(data.array);
    const element_type = array_type.extended.cast(Type.Array).?.element;
    const access_index = try codegen.resolveInst(data.index);

    const scratch_top = codegen.scratch.items.len;
    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
    const indices = try codegen.scratch.addManyAsSlice(codegen.arena, 2);
    indices[0] = try codegen.builder.addUint(Type.Common.u32_type, 0);
    indices[1] = access_index;
    const gep = try codegen.builder.addGetElementPtr(array_type, array, indices);
    if (hir.insts.items(.tag)[data.index] == .constant) {
        // comptime known, so inbounds
        // TODO: better check and also make sure this is correct
        // and actually inbound
        c.LLVMSetIsInBounds(gep, 1);
    }
    return codegen.builder.addLoad(gep, element_type);
}

fn slicePtrRef(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .slice_ptr_ref);

    const ty = try hir.resolveType(codegen.arena, data.operand);
    const slice_type = ty.extended.cast(Type.Pointer).?.pointee;
    const slice = try codegen.resolveInst(data.operand);

    const scratch_top = codegen.scratch.items.len;
    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
    const indices = try codegen.scratch.addManyAsSlice(codegen.arena, 2);
    indices[0] = try codegen.builder.addUint(Type.Common.u32_type, 0);
    indices[1] = try codegen.builder.addUint(Type.Common.u32_type, 0);

    const gep = try codegen.builder.addGetElementPtr(slice_type, slice, indices);
    c.LLVMSetIsInBounds(gep, 1);
    return gep;
}

fn sliceLenRef(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .slice_len_ref);

    const ty = try hir.resolveType(codegen.arena, data.operand);
    const slice_type = ty.extended.cast(Type.Pointer).?.pointee;
    const slice = try codegen.resolveInst(data.operand);

    const scratch_top = codegen.scratch.items.len;
    defer codegen.scratch.shrinkRetainingCapacity(scratch_top);
    const indices = try codegen.scratch.addManyAsSlice(codegen.arena, 2);
    indices[0] = try codegen.builder.addUint(Type.Common.u32_type, 0);
    indices[1] = try codegen.builder.addUint(Type.Common.u32_type, 1);

    const gep = try codegen.builder.addGetElementPtr(slice_type, slice, indices);
    c.LLVMSetIsInBounds(gep, 1);
    return gep;
}

fn slicePtrVal(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .slice_ptr_val);

    const slice = try codegen.resolveInst(data.operand);
    return codegen.builder.addExtractValue(slice, 0);
}

fn sliceLenVal(codegen: *CodeGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = codegen.hir;
    const data = hir.get(inst, .slice_ptr_val);

    const slice = try codegen.resolveInst(data.operand);
    return codegen.builder.addExtractValue(slice, 1);
}
