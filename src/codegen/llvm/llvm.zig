const std = @import("std");
const Mir = @import("../../Mir.zig");
const typing = @import("../../typing.zig");

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/Disassembler.h");
});

const Allocator = std.mem.Allocator;

pub const Module = c.LLVMModuleRef;
pub const Builder = c.LLVMBuilderRef;
pub const Value = c.LLVMValueRef;
pub const Type = c.LLVMTypeRef;

pub const addFunction = c.LLVMAddFunction;
pub const createModule = c.LLVMModuleCreateWithName;
pub const destroyModule = c.LLVMDisposeModule;
pub const verifyModule = c.LLVMVerifyModule;
pub const dumpModule = c.LLVMDumpModule;

pub const createBuilder = c.LLVMCreateBuilder;
pub const destroyBuilder = c.LLVMDisposeBuilder;

pub fn getType(arena: Allocator, ty: typing.Type) !Type {
    return switch (ty.kind()) {
        .void => c.LLVMVoidType(),
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint, .sint => c.LLVMIntType(ty.basic.width),
        .float => switch (ty.basic.width) {
            32 => c.LLVMFloatType(),
            64 => c.LLVMDoubleType(),
            else => unreachable,
        },
        .function => {
            const function = ty.extended.cast(typing.Type.Function).?;

            const params = try arena.alloc(c.LLVMTypeRef, function.param_types.len);
            defer arena.free(params);
            for (function.param_types) |param, i| {
                params[i] = try getType(arena, param);
            }
            const return_type = try getType(arena, function.return_type);

            return c.LLVMFunctionType(return_type, params.ptr, @intCast(c_uint, params.len), 0);
        },
        else => unreachable,
    };
}


