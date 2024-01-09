const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Air = @import("../air/Air.zig");
const Type = @import("../air/type.zig").Type;
const TypedValue = @import("../air/TypedValue.zig");
const Allocator = std.mem.Allocator;
const Decl = Air.Decl;

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
});

const Context = @This();

arena: Allocator,
context: c.LLVMContextRef,
module: c.LLVMModuleRef,
pool: *InternPool,
decl_map: std.AutoHashMapUnmanaged(InternPool.DeclIndex, c.LLVMValueRef),

pub const Error = error{
    VerificationError,
};

pub fn init(arena: Allocator, name: [:0]const u8, pool: *InternPool) Context {
    const context = c.LLVMContextCreate();
    const module = c.LLVMModuleCreateWithNameInContext(name.ptr, context);

    return .{
        .arena = arena,
        .context = context,
        .module = module,
        .pool = pool,
        .decl_map = .{},
    };
}

pub fn deinit(context: *Context) void {
    c.LLVMDisposeModule(context.module);
    c.LLVMContextDispose(context.context);
    context.decl_map.deinit(context.arena);
}

// prints to stderr
pub fn dump(context: *Context) void {
    c.LLVMDumpModule(context.module);
}

pub fn printToFile(context: *Context, filename: []const u8) !void {
    var err: [*c]u8 = @ptrFromInt(0);
    defer c.LLVMDisposeMessage(err);
    const c_str = try std.fmt.allocPrintZ(context.arena, "{s}", .{filename});
    defer context.arena.free(c_str);

    const result = c.LLVMPrintModuleToFile(context.module, c_str.ptr, &err);
    if (result != 0) {
        std.log.err("codegen: failed to print module to file {s}", .{filename});
        std.os.exit(1);
    }
}

pub fn verify(context: *Context) void {
    var err: [*c]u8 = @ptrFromInt(0);
    const result = c.LLVMVerifyModule(context.module, c.LLVMPrintMessageAction, &err);
    defer c.LLVMDisposeMessage(err);
    if (result != 0) {
        std.log.err("codegen: failed to verify module: {s}", .{err});
        return Error.VerificationError;
    }
}

// converts an internal Air type to a (uniqued) LLVM type reference
pub fn resolveType(context: *Context, ty: Type) !c.LLVMTypeRef {
    return switch (ty) {
        .void => c.LLVMVoidTypeInContext(context.context),
        .comptime_int => unreachable,
        .comptime_float => unreachable,
        .ref => unreachable,
        .int => |int| c.LLVMIntTypeInContext(context.context, int.width),
        .float => |float| switch (float.width) {
            32 => c.LLVMFloatTypeInContext(context.context),
            64 => c.LLVMDoubleTypeInContext(context.context),
            else => unreachable,
        },
        .bool => c.LLVMIntTypeInContext(context.context, 1),
        .function => |function| {
            const src_params = context.pool.extra.items[function.params.start..function.params.end];
            const params = try context.arena.alloc(c.LLVMTypeRef, src_params.len);
            defer context.arena.free(params);

            for (src_params, 0..) |param, i| {
                const param_ty = context.pool.indexToKey(@enumFromInt(param)).ty;
                params[i] = try context.resolveType(param_ty);
            }

            const return_type = ty: {
                const src = context.pool.indexToKey(function.@"return").ty;
                break :ty try context.resolveType(src);
            };
            return c.LLVMFunctionType(return_type, params.ptr, @intCast(params.len), 0);
        },
        .pointer, .many_pointer => return c.LLVMPointerTypeInContext(context.context, 0),
        .array => |array| {
            const element_type = try context.resolveType(context.pool.indexToKey(array.element).ty);
            return c.LLVMArrayType(element_type, array.count);
        },
        .@"struct" => |st| {
            const src_fields = context.pool.extra.items[st.fields.start..st.fields.end];
            const fields = try context.arena.alloc(c.LLVMTypeRef, src_fields.len);
            defer context.arena.free(fields);

            for (src_fields, 0..) |field, i| {
                const field_type = context.pool.indexToType(@enumFromInt(field));
                fields[i] = try context.resolveType(field_type);
            }

            return c.LLVMStructTypeInContext(context.context, fields.ptr, @intCast(fields.len), @intFromBool(false));
        },
        .slice => {
            // wide pointer consisting of pointer and usize len
            const ptr = c.LLVMPointerTypeInContext(context.context, 0);
            const len = c.LLVMInt64TypeInContext(context.context);
            var fields = .{ ptr, len };
            return c.LLVMStructTypeInContext(context.context, @ptrCast(&fields), 2, @intFromBool(false));
        },
    };
}

pub fn resolveTv(context: *Context, tv: TypedValue) !c.LLVMValueRef {
    const ty = context.pool.indexToKey(tv.ty).ty;
    const llvm_type = try context.resolveType(ty);
    return switch (ty) {
        .void,
        .comptime_int,
        .comptime_float,
        .function,
        => unreachable,
        .int => |int| c.LLVMConstInt(llvm_type, @intCast(tv.val.integer), @intFromBool(int.sign == .signed)),
        .bool => c.LLVMConstInt(llvm_type, @intCast(tv.val.integer), @intFromBool(false)),
        .float => c.LLVMConstReal(llvm_type, tv.val.float),
        .ref,
        .pointer,
        .many_pointer,
        .slice,
        .@"struct",
        => unreachable, // TODO
        .array => {
            switch (tv.val) {
                .array => |array| {
                    const src_elements = context.pool.extra.items[array.start..array.end];
                    var elements = try context.arena.alloc(c.LLVMValueRef, src_elements.len);
                    for (src_elements, 0..) |src_element, i| {
                        const element_tv = context.pool.indexToKey(@enumFromInt(src_element)).tv;
                        elements[i] = try context.resolveTv(element_tv);
                    }

                    return c.LLVMConstArray(llvm_type, elements.ptr, @intCast(elements.len));
                },
                .string => |string| {
                    const bytes = context.pool.getString(string).?;
                    return c.LLVMConstStringInContext(context.context, bytes.ptr, @intCast(bytes.len), @intFromBool(false));
                },
                else => unreachable,
            }
        },
    };
}

// pub fn generateDecl(context: *Context, decl: *const Decl) !c.LLVMValueRef {
pub fn generateDecl(context: *Context, decl_index: InternPool.DeclIndex) !c.LLVMValueRef {
    const pool = context.pool;
    const decl = pool.decls.at(@intFromEnum(decl_index));
    const name = switch (decl.name) {
        .named => |name| pool.getString(name).?,
        .unnamed => "",
    };

    const ty = pool.indexToKey(decl.ty).ty;
    const llvm_decl = switch (ty) {
        .void, .comptime_int, .comptime_float => unreachable,
        .function => try context.addFunction(name, ty),
        else => decl: {
            const global = try context.addGlobal(name, ty);
            c.LLVMSetGlobalConstant(global, @intFromBool(!decl.mutable));
            if (decl.initializer) |initializer| {
                const initializer_tv = pool.indexToKey(initializer).tv;
                const value = try context.resolveTv(initializer_tv);
                c.LLVMSetInitializer(global, value);
            }
            break :decl global;
        },
    };

    switch (decl.linkage) {
        .internal => c.LLVMSetLinkage(llvm_decl, c.LLVMInternalLinkage),
        .external => c.LLVMSetLinkage(llvm_decl, c.LLVMExternalLinkage),
    }

    switch (decl.name) {
        .unnamed => c.LLVMSetUnnamedAddress(llvm_decl, c.LLVMGlobalUnnamedAddr),
        else => {},
    }

    try context.decl_map.put(context.arena, decl_index, llvm_decl);
    return llvm_decl;
}

pub fn addFunction(context: *Context, name: [:0]const u8, ty: Type) !c.LLVMValueRef {
    const llvm_type = try context.resolveType(ty);
    return c.LLVMAddFunction(context.module, name.ptr, llvm_type);
}

pub fn addGlobal(context: *Context, name: [:0]const u8, ty: Type) !c.LLVMValueRef {
    const llvm_type = try context.resolveType(ty);
    return c.LLVMAddGlobal(context.module, llvm_type, name.ptr);
}

// given a decl (by index into the intern pool), returns an LLVMValueRef to that decl
// works on both globals and functions
// pub fn resolveDecl(context: *Context, decl: *const Decl) c.LLVMValueRef {
//     // TODO: what happens when the name is unavailable
//     const name = context.pool.getString(decl.name.?).?;
//     const ty = context.pool.indexToKey(decl.ty).ty;
//     if (@as(std.meta.Tag(Type), ty) == .function) {
//         return c.LLVMGetNamedFunction(context.module, name);
//     } else {
//         return c.LLVMGetNamedGlobal(context.module, name);
//     }
// }

pub fn resolveDecl(context: *Context, decl_index: InternPool.DeclIndex) c.LLVMValueRef {
    return context.decl_map.get(decl_index).?;
    // const name = context.pool.getString(decl.name.?).?;
    // const ty = context.pool.indexToKey(decl.ty).ty;
    // if (@as(std.meta.Tag(Type), ty) == .function) {
    //     return c.LLVMGetNamedFunction(context.module, name);
    // } else {
    //     return c.LLVMGetNamedGlobal(context.module, name);
    // }
}

pub fn addConstString(context: *Context, string: []const u8) !c.LLVMValueRef {
    return c.LLVMConstStringInContext(context.context, string.ptr, @intCast(string.len), 0);
}

pub fn addConstStruct(context: *Context, vals: []c.LLVMValueRef) !c.LLVMValueRef {
    return c.LLVMConstStructInContext(context.context, vals.ptr, @intCast(vals.len), 0);
}

// pub fn lookupIntrinsic(context: *Context, name: []const u8) c.LLVMValueRef {
//     const id = c.LLVMLookupIntrinsicID(name.ptr, name.len);
//     const decl = c.LLVMGetIntrinsicDeclaration(context.module, id,)
// }

pub const Builder = struct {
    arena: Allocator,
    context: *Context,
    function: c.LLVMValueRef,
    builder: c.LLVMBuilderRef,

    pub fn init(context: *Context, function: c.LLVMValueRef) Builder {
        return .{
            .arena = context.arena,
            .context = context,
            .function = function,
            .builder = c.LLVMCreateBuilderInContext(context.context),
        };
    }

    pub fn deinit(builder: *Builder) void {
        c.LLVMDisposeBuilder(builder.builder);
    }

    pub fn appendBlock(builder: *Builder, name: [:0]const u8) c.LLVMBasicBlockRef {
        return c.LLVMAppendBasicBlock(builder.function, name.ptr);
    }

    pub fn deleteBlock(_: *Builder, block: c.LLVMBasicBlockRef) void {
        c.LLVMDeleteBasicBlock(block);
    }

    pub fn getInsertBlock(builder: *Builder) c.LLVMBasicBlockRef {
        return c.LLVMGetInsertBlock(builder.builder);
    }

    pub fn positionAtEnd(builder: *Builder, block: c.LLVMBasicBlockRef) void {
        c.LLVMPositionBuilderAtEnd(builder.builder, block);
    }

    pub fn addUint(builder: *Builder, ty: Type, val: u64) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(val), 0);
    }

    pub fn addSint(builder: *Builder, ty: Type, val: u64) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(val), 1);
    }

    pub fn addFloat(builder: *Builder, ty: Type, val: f64) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMConstReal(llvm_type, val);
    }

    pub fn addConstArray(builder: *Builder, ty: Type, vals: []c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMConstArray(llvm_type, vals.ptr, @intCast(vals.len));
    }

    // memory
    pub fn addAlloca(builder: *Builder, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildAlloca(builder.builder, llvm_type, "");
    }

    pub fn addLoad(builder: *Builder, addr: c.LLVMValueRef, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildLoad2(builder.builder, llvm_type, addr, "");
    }

    pub fn addStore(builder: *Builder, addr: c.LLVMValueRef, val: c.LLVMValueRef) c.LLVMValueRef {
        return c.LLVMBuildStore(builder.builder, val, addr);
    }

    // operators and arithmetic
    pub const BinOp = enum {
        add,
        sub,
        umul,
        udiv,
        urem,
        smul,
        sdiv,
        srem,
        sl,
        lsr,
        asr,
        bitwise_or,
        bitwise_and,
        bitwise_xor,

        fadd,
        fsub,
        fmul,
        fdiv,
        frem,

        cmp_ieq,
        cmp_ine,

        cmp_ule,
        cmp_uge,
        cmp_ult,
        cmp_ugt,

        cmp_sle,
        cmp_sge,
        cmp_slt,
        cmp_sgt,

        cmp_feq,
        cmp_fne,

        cmp_fle,
        cmp_fge,
        cmp_flt,
        cmp_fgt,
    };

    pub fn chooseBinary(builder: *Builder, tag: Air.Inst.Tag, ty: Type) BinOp {
        _ = builder;
        return switch (ty) {
            .int => |int| switch (int.sign) {
                .unsigned => switch (tag) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .umul,
                    .div => .udiv,
                    .mod => .umod,
                    .lsl, .asl => .sl,
                    .lsr => .lsr,
                    .asr => .asr,
                    .bitwise_or => .bitwise_or,
                    .bitwise_and => .bitwise_and,
                    .bitwise_xor => .bitwise_xor,
                    else => unreachable,
                },
                .signed => switch (tag) {
                    .add => .add,
                    .sub => .sub,
                    .mul => .smul,
                    .div => .sdiv,
                    .mod => .smod,
                    .lsl, .asl => .sl,
                    .lsr => .lsr,
                    .asr => .asr,
                    .bitwise_or => .bitwise_or,
                    .bitwise_and => .bitwise_and,
                    .bitwise_xor => .bitwise_xor,
                    else => unreachable,
                },
            },
            .float => switch (tag) {
                .add => .fadd,
                .sub => .fsub,
                .mul => .fmul,
                .div => .fdiv,
                .mod => .fmod,
                else => unreachable,
            },
        };
    }

    pub fn addBinary(builder: *Builder, op: BinOp, l: c.LLVMValueRef, r: c.LLVMValueRef) c.LLVMValueRef {
        return switch (op) {
            .cmp_ieq => c.LLVMBuildICmp(builder.builder, c.LLVMIntEQ, l, r, ""),
            .cmp_ine => c.LLVMBuildICmp(builder.builder, c.LLVMIntNE, l, r, ""),

            .cmp_ule => c.LLVMBuildICmp(builder.builder, c.LLVMIntULE, l, r, ""),
            .cmp_uge => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGE, l, r, ""),
            .cmp_ult => c.LLVMBuildICmp(builder.builder, c.LLVMIntULT, l, r, ""),
            .cmp_ugt => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGT, l, r, ""),

            .cmp_sle => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLE, l, r, ""),
            .cmp_sge => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGE, l, r, ""),
            .cmp_slt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLT, l, r, ""),
            .cmp_sgt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGT, l, r, ""),

            .cmp_fle => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLE, l, r, ""),
            .cmp_fge => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGE, l, r, ""),
            .cmp_flt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLT, l, r, ""),
            .cmp_fgt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGT, l, r, ""),
            else => unreachable,
        };
    }

    pub fn addCmp(builder: *Builder, comptime tag: std.meta.Tag(Air.Inst), lref: c.LLVMValueRef, rref: c.LLVMValueRef) c.LLVMValueRef {
        return switch (tag) {
            .icmp_eq => c.LLVMBuildICmp(builder.builder, c.LLVMIntEQ, lref, rref, ""),
            .icmp_ne => c.LLVMBuildICmp(builder.builder, c.LLVMIntNE, lref, rref, ""),

            .icmp_ule => c.LLVMBuildICmp(builder.builder, c.LLVMIntULE, lref, rref, ""),
            .icmp_uge => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGE, lref, rref, ""),
            .icmp_ult => c.LLVMBuildICmp(builder.builder, c.LLVMIntULT, lref, rref, ""),
            .icmp_ugt => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGT, lref, rref, ""),

            .icmp_sle => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLE, lref, rref, ""),
            .icmp_sge => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGE, lref, rref, ""),
            .icmp_slt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLT, lref, rref, ""),
            .icmp_sgt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGT, lref, rref, ""),

            .fcmp_le => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLE, lref, rref, ""),
            .fcmp_ge => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGE, lref, rref, ""),
            .fcmp_lt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLT, lref, rref, ""),
            .fcmp_gt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGT, lref, rref, ""),

            else => unreachable,
        };
    }

    // control flow
    pub fn addBranch(builder: *Builder, bb: c.LLVMBasicBlockRef) void {
        _ = c.LLVMBuildBr(builder.builder, bb);
    }

    pub fn addCondBranch(builder: *Builder, cond: c.LLVMValueRef, a: c.LLVMBasicBlockRef, b: c.LLVMBasicBlockRef) void {
        _ = c.LLVMBuildCondBr(builder.builder, cond, a, b);
    }

    pub fn addCall(builder: *Builder, ty: Type, addr: c.LLVMValueRef, args: []c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildCall2(builder.builder, llvm_type, addr, args.ptr, @intCast(args.len), "");
    }

    pub fn addReturn(builder: *Builder, val: c.LLVMValueRef) c.LLVMValueRef {
        if (val) |ref| {
            return c.LLVMBuildRet(builder.builder, ref);
        } else {
            return c.LLVMBuildRetVoid(builder.builder);
        }
    }

    // casting
    pub fn addZext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildZExt(builder.builder, val, llvm_type, "");
    }

    pub fn addSext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildSExt(builder.builder, val, llvm_type, "");
    }

    pub fn addFpext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildFPExt(builder.builder, val, llvm_type, "");
    }

    pub fn addGetElementPtr(builder: *Builder, ty: Type, ptr: c.LLVMValueRef, indices: []const c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildGEP2(builder.builder, llvm_type, ptr, @constCast(indices.ptr), @intCast(indices.len), "");
    }

    pub fn addPhi(builder: *Builder, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.resolveType(ty);
        return c.LLVMBuildPhi(builder.builder, llvm_type, "");
    }

    pub fn addMemcpy(builder: *Builder, ty: Type, dest: c.LLVMValueRef, src: c.LLVMValueRef) !c.LLVMValueRef {
        const u32_type: Type = .{ .int = .{ .sign = .unsigned, .width = 32 } };
        const size = try builder.addUint(u32_type, ty.size(builder.context.pool).?);
        const dest_align = c.LLVMGetAlignment(dest);
        const src_align = c.LLVMGetAlignment(src);
        return c.LLVMBuildMemCpy(builder.builder, dest, dest_align, src, src_align, size);
    }

    pub fn addInsertValue(builder: *Builder, agg: c.LLVMValueRef, element: c.LLVMValueRef, index: u32) !c.LLVMValueRef {
        return c.LLVMBuildInsertValue(builder.builder, agg, element, index, "");
    }

    pub fn addExtractValue(builder: *Builder, agg: c.LLVMValueRef, index: u32) !c.LLVMValueRef {
        return c.LLVMBuildExtractValue(builder.builder, agg, index, "");
    }

    pub inline fn resolveType(builder: *Builder, ty: Type) !c.LLVMTypeRef {
        return builder.context.resolveType(ty);
    }
};
