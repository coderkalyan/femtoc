const std = @import("std");
const Allocator = std.mem.Allocator;
const Mir = @import("Mir.zig");
const Compilation = @import("MirGen.zig").Compilation;
const Type = @import("typing.zig").Type;

const c = @cImport({
    @cInclude("llvm-c/Core.h");
});

pub const Backend = struct {
    gpa: Allocator,
    compilation: *const Compilation,
    module: c.LLVMModuleRef,
};

gpa: Allocator,
arena: Allocator,
backend: *Backend,
parameter_types: std.ArrayListUnmanaged(c.LLVMTypeRef),
return_type: c.LLVMTypeRef,
builder: c.LLVMBuilderRef,

const CodeGen = @This();

pub fn generate(gpa: Allocator, compilation: *const Compilation) !void {
    const module = c.LLVMModuleCreateWithName("femto_main");
    defer c.LLVMDisposeModule(module);

    var backend = Backend {
        .gpa = gpa,
        .compilation = compilation,
        .module = module,
    };

    generateGlobals(&backend, &compilation.global);

    // for (compilation.mir) |mir| {
    //     generateFromMir(&backend, &mir);
    // }
}

fn getLlvmType(ty: Type) c.LLVMTypeRef {
    if (ty.isTag()) {
        return switch (ty.tag) {
            .void => c.LLVMVoidType(),
            .comptime_uint, .comptime_sint => unreachable,
            .u1 => c.LLVMInt1Type(),
            .i8, .u8 => c.LLVMInt8Type(),
            .i16, .u16 => c.LLVMInt16Type(),
            .i32, .u32 => c.LLVMInt32Type(),
            .i64, .u64 => c.LLVMInt64Type(),
            .comptime_float => unreachable,
            .f32 => c.LLVMFloatType(),
            .f64 => c.LLVMDoubleType(),
        };
    } else {
        // TODO
        unreachable;
    }
}

fn generateGlobals(backend: *Backend, global: *const Mir) void {
    _ = backend;
    _ = global;
}

fn generateFromMir(backend: *Backend, mir: *const Mir) void {
    var arena = std.heap.ArenaAllocator.init(backend.gpa);
    defer arena.deinit();

    const builder = c.LLVMCreateBuilder();
    defer c.LLVMDisposeBuilder(builder);

    const function = mir.insts.items(.data)[mir.insts.len - 1];
    var codegen = CodeGen {
        .gpa = backend.gpa,
        .arena = arena.allocator(),
        .backend = backend,
        .parameter_types = .{},
        .return_type = getLlvmType(function.ty_pl.ty),
        .builder = builder,
    };

    const function_type = c.LLVMFunctionType(codegen.return_type,
                                             codegen.parameter_types.items.ptr,
                                             @intCast(c_uint, codegen.parameter_types.items.len),
                                             0);
    _ = function_type;

    // const function_ref = c.LLVMAddFunction()
    // const entry = c.LLVMAppendBasicBlock()
    // c.LLVMPositionBuilderAtEnd(builder, entry);
    codegen.block(function.ty_pl.pl);
}

fn block(codegen: *CodeGen, inst: Mir.Index) void {
    _ = codegen;
    _ = inst;
}
