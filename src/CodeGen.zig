const std = @import("std");
const Allocator = std.mem.Allocator;
const Mir = @import("Mir.zig");
const Compilation = @import("MirGen.zig").Compilation;
const Type = @import("typing.zig").Type;

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/Disassembler.h");
});

pub const Backend = struct {
    gpa: Allocator,
    compilation: *const Compilation,
    module: c.LLVMModuleRef,
    map: std.AutoHashMapUnmanaged(u32, c.LLVMValueRef),
};

gpa: Allocator,
arena: Allocator,
backend: *Backend,
builder: c.LLVMBuilderRef,
function: c.LLVMValueRef,
map: std.AutoHashMapUnmanaged(u32, c.LLVMValueRef),

const CodeGen = @This();

pub fn generate(gpa: Allocator, compilation: *const Compilation) !void {
    const module = c.LLVMModuleCreateWithName("femto_main");
    defer c.LLVMDisposeModule(module);

    var backend = Backend {
        .gpa = gpa,
        .compilation = compilation,
        .module = module,
        .map = .{},
    };

    try generateGlobals(&backend, &compilation.global);
    for (compilation.mir) |mir| {
        try generateFunctionBody(&backend, &mir);
    }

    var err = @intToPtr([*c]u8, 0);
    const result = c.LLVMVerifyModule(backend.module, c.LLVMPrintMessageAction, &err);
    if (result != 0) {
        std.debug.print("analysis failed\n", .{});
        c.LLVMDisposeMessage(err);
    }

    if (c.LLVMWriteBitcodeToFile(backend.module, "arithmetic.bc") != 0) {
        std.debug.print("writing bitcode failed\n", .{});
    }

    // if (c.LLVMPrintModuleToFile(backend.module, "arithmetic.out", &err) != 0) {
    //     std.debug.print("printing module failed\n", .{});
    //     c.LLVMDisposeMessage(err);
    // }
    c.LLVMDumpModule(backend.module);
}

fn getLlvmType(gpa: Allocator, ty: Type) !c.LLVMTypeRef {
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
        switch (ty.payload.kind) {
            .function => {
                const function = ty.payload.cast(Type.Function).?;
                const return_ty = try getLlvmType(gpa, function.return_ty);

                const params = try gpa.alloc(c.LLVMTypeRef, function.params.len);
                for (function.params) |param, i| {
                    params[i] = try getLlvmType(gpa, param);
                }

                return c.LLVMFunctionType(return_ty, params.ptr, @intCast(c_uint, params.len), 0);
            },
            else => unreachable,
        }
    }
}

fn resolveRef(codegen: *CodeGen, mirref: Mir.Ref) c.LLVMValueRef {
    const index = Mir.refToIndex(mirref).?;
    if (codegen.map.get(index)) |ref| return ref;
    return codegen.backend.map.get(index).?;
}

fn generateGlobals(backend: *Backend, global: *const Mir) !void {
    const data = global.insts.items(.data)[global.insts.len - 1];
    const block_data = global.extraData(data.pl, Mir.Inst.Block);

    var extra_index: u32 = 0;
    while (extra_index < block_data.insts_len) : (extra_index += 1) {
        const inst_index = global.extra[data.pl + 1 + extra_index];
        try generateGlobalInst(backend, global, inst_index);
    }
}

fn generateGlobalInst(backend: *Backend, global: *const Mir, inst: u32) !void {
    switch (global.insts.items(.tag)[inst]) {
        .proto => try generateFunctionProto(backend, global, inst),
        else => std.debug.print("{}", .{global.insts.items(.tag)[inst]}),
    }
}

fn generateFunctionProto(backend: *Backend, global: *const Mir, inst: u32) !void {
    const data = global.insts.items(.data)[inst];

    const function_ty = try getLlvmType(backend.gpa, data.ty_pl.ty);
    var name = [_]u8{'a', 0};
    name[0] += @intCast(u8, data.ty_pl.pl);
    const function_ref = c.LLVMAddFunction(backend.module, &name, function_ty);
    try backend.map.put(backend.gpa, inst, function_ref);
}

fn generateFunctionBody(backend: *Backend, mir: *const Mir) !void {
    var arena = std.heap.ArenaAllocator.init(backend.gpa);
    defer arena.deinit();

    const builder = c.LLVMCreateBuilder();
    defer c.LLVMDisposeBuilder(builder);

    const data = mir.insts.items(.data)[mir.insts.len - 1];

    var codegen = CodeGen {
        .gpa = backend.gpa,
        .arena = arena.allocator(),
        .backend = backend,
        .builder = builder,
        .function = backend.map.get(data.bin_pl.l).?,
        .map = .{},
    };

    const entry = c.LLVMAppendBasicBlock(codegen.function, "entry");
    c.LLVMPositionBuilderAtEnd(codegen.builder, entry);
    try codegen.block(mir, data.bin_pl.r);

    // std.debug.print("{any}\n", .{c.LLVMGetEntryBasicBlock(codegen.function)});
    // std.debug.print("{any}\n", .{c.LLVMGetFirstInstruction(entry)});
}

fn block(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !void {
    const data = mir.insts.items(.data)[inst];
    const block_data = mir.extraData(data.pl, Mir.Inst.Block);

    var extra_index: u32 = 0;
    while (extra_index < block_data.insts_len) : (extra_index += 1) {
        const inst_index = mir.extra[data.pl + 1 + extra_index];
        const ref = switch (mir.insts.items(.tag)[inst_index]) {
            .constant => try codegen.constant(mir, inst_index),
            .add, .sub, .mul, .div, .mod => try codegen.binaryOp(mir, inst_index),
            .ret => try codegen.ret(mir, inst_index),
            else => {
                std.debug.print("{}\n", .{mir.insts.items(.tag)[inst_index]});
                unreachable;
                // break :ref c.LLVMConstPointerNull(c.LLVMInt32Type());
            }
        };
        std.debug.print("{any}\n", .{c.LLVMGetInstructionOpcode(ref)});
        try codegen.map.put(codegen.gpa, inst_index, ref);
    }
}

fn constant(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    const value = mir.values[data.ty_pl.pl];
    // TODO: make sure we will only use this with tagged types
    switch (data.ty_pl.ty.tag) {
        .comptime_uint, .comptime_sint => return c.LLVMConstPointerNull(c.LLVMInt32Type()),
        .comptime_float => return c.LLVMConstPointerNull(c.LLVMInt32Type()),
        else => {},
    }

    const ty = try getLlvmType(codegen.gpa, data.ty_pl.ty);
    switch (data.ty_pl.ty.tag) {
        .u1, .u8, .u16, .u32, .u64 => return c.LLVMConstInt(ty, @intCast(c_ulonglong, value.uint), 0),
        .i8, .i16, .i32, .i64 => return c.LLVMConstInt(ty, @intCast(c_ulonglong, value.sint), 1),
        .f32, .f64 => return c.LLVMConstReal(ty, value.float),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    const lref = codegen.resolveRef(data.bin_op.lref);
    const rref = codegen.resolveRef(data.bin_op.rref);
    
    // TODO: take care of signed properly
    switch (mir.insts.items(.tag)[inst]) {
        .add => return c.LLVMBuildAdd(codegen.builder, lref, rref, "add"),
        .sub => return c.LLVMBuildSub(codegen.builder, lref, rref, "sub"),
        .mul => return c.LLVMBuildMul(codegen.builder, lref, rref, "mul"),
        .div => {
            return switch (c.LLVMGetValueKind(lref)) {
                c.LLVMConstantIntValueKind => c.LLVMBuildSDiv(codegen.builder, lref, rref, "div"),
                c.LLVMConstantFPValueKind => c.LLVMBuildFDiv(codegen.builder, lref, rref, "div"),
                else => unreachable,
            };
        },
        .mod => return c.LLVMBuildSRem(codegen.builder, lref, rref, "mod"),
        else => unreachable,
    }
}

fn ret(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    
    if (data.un_op == Mir.Ref.void_val) {
        return c.LLVMBuildRetVoid(codegen.builder);
    } else {
        const ref = codegen.resolveRef(data.un_op);
        return c.LLVMBuildRet(codegen.builder, ref);
    }
}
