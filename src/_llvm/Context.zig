const std = @import("std");
const Hir = @import("../Hir.zig");
const Type = @import("../hir/type.zig").Type;

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
});

const Allocator = std.mem.Allocator;
const Context = @This();

arena: Allocator,
context: c.LLVMContextRef,
module: c.LLVMModuleRef,

pub const Error = error{
    VerificationError,
};

pub fn init(arena: Allocator, name: [:0]const u8) Context {
    const context = c.LLVMContextCreate();
    const module = c.LLVMModuleCreateWithNameInContext(name.ptr, context);

    return .{
        .arena = arena,
        .context = context,
        .module = module,
    };
}

pub fn deinit(context: *Context) void {
    c.LLVMDisposeModule(context.module);
    c.LLVMContextDispose(context.context);
}

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

// converts an internal HIR type to a (uniqued) LLVM type reference
pub fn convertType(context: *Context, ty: Type) !c.LLVMTypeRef {
    return switch (ty.kind()) {
        .void => c.LLVMVoidTypeInContext(context.context),
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint, .sint => c.LLVMIntTypeInContext(context.context, ty.basic.width),
        .float => switch (ty.basic.width) {
            32 => c.LLVMFloatTypeInContext(context.context),
            64 => c.LLVMDoubleTypeInContext(context.context),
            else => unreachable,
        },
        .function => {
            const function = ty.extended.cast(Type.Function).?;
            const params = try context.arena.alloc(c.LLVMTypeRef, function.param_types.len);
            defer context.arena.free(params);

            for (function.param_types, 0..) |param, i| {
                params[i] = try context.convertType(param);
            }

            const return_type = try context.convertType(function.return_type);
            return c.LLVMFunctionType(return_type, params.ptr, @intCast(params.len), 0);
        },
        .pointer => return c.LLVMPointerTypeInContext(context.context, 0),
        .structure => unreachable,
    };
}

pub fn addFunction(context: *Context, name: [:0]const u8, ty: c.LLVMTypeRef) c.LLVMValueRef {
    return c.LLVMAddFunction(context.module, name.ptr, ty);
}

pub fn getNamedFunction(context: *Context, name: [:0]const u8) c.LLVMValueRef {
    return c.LLVMGetNamedFunction(context.module, name.ptr);
}

pub fn addGlobal(context: *Context, name: [:0]const u8, ty: c.LLVMTypeRef) c.LLVMValueRef {
    return c.LLVMAddGlobal(context.module, ty, name.ptr);
}

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
        const llvm_type = try builder.convertType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(val), 0);
    }

    pub fn addSint(builder: *Builder, ty: Type, val: u64) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(val), 1);
    }

    pub fn addFloat(builder: *Builder, ty: Type, val: f64) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMConstReal(llvm_type, val);
    }

    // memory
    pub fn addAlloca(builder: *Builder, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMBuildAlloca(builder.builder, llvm_type, "");
    }

    pub fn addLoad(builder: *Builder, addr: c.LLVMValueRef, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMBuildLoad2(builder.builder, llvm_type, addr, "");
    }

    pub fn addStore(builder: *Builder, addr: c.LLVMValueRef, val: c.LLVMValueRef) c.LLVMValueRef {
        return c.LLVMBuildStore(builder.builder, val, addr);
    }

    // operators and arithmetic
    pub const Cmp = enum {
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

    pub fn addCmp(builder: *Builder, comptime tag: Hir.Inst.Tag, lref: c.LLVMValueRef, rref: c.LLVMValueRef) c.LLVMValueRef {
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

            .cmp_eq,
            .cmp_ne,
            .cmp_gt,
            .cmp_ge,
            .cmp_lt,
            .cmp_le,
            => @compileError("can't lower untyped comparison"),

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
        const llvm_type = try builder.convertType(ty);
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
        const llvm_type = try builder.convertType(ty);
        return c.LLVMBuildZExt(builder.builder, val, llvm_type, "");
    }

    pub fn addSext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMBuildSExt(builder.builder, val, llvm_type, "");
    }

    pub fn addFpext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.convertType(ty);
        return c.LLVMBuildFPExt(builder.builder, val, llvm_type, "");
    }

    // basic types are guaranteed not to allocate, so they won't throw errors
    // pub fn getBasicType(builder: *Builder, ty: Type) c.LLVMTypeRef {
    //     return llvm.getBasicType(builder.context, ty);
    // }

    pub inline fn convertType(builder: *Builder, ty: Type) !c.LLVMTypeRef {
        return builder.context.convertType(ty);
    }
};
