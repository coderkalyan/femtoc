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

const Error = Allocator.Error || @import("interner.zig").Error || error { NotImplemented };

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
            const function = ty.extended.cast(Type.Function).?;
            const return_ty = try getLlvmType(gpa, function.return_ty);

            const params = try gpa.alloc(c.LLVMTypeRef, function.params.len);
            for (function.params) |param, i| {
                params[i] = try getLlvmType(gpa, param);
            }

            return c.LLVMFunctionType(return_ty, params.ptr, @intCast(c_uint, params.len), 0);
        },
        else => unreachable,
    };
}

fn resolveLocalRef(codegen: *CodeGen, mirref: Mir.Ref) c.LLVMValueRef {
    if (Mir.refToIndex(mirref)) |index| {
        return codegen.map.get(index).?;
    } else {
        return switch (mirref) {
            .zero_val => c.LLVMConstInt(c.LLVMInt64Type(), 0, 0),
            .one_val => c.LLVMConstInt(c.LLVMInt64Type(), 1, 0),
            else => unreachable,
        };
    }
}

fn resolveGlobalRef(backend: *Backend, mirref: Mir.Ref) c.LLVMValueRef {
    const index = Mir.refToIndex(mirref).?;
    return backend.map.get(index).?;
}

fn generateGlobals(backend: *Backend, global: *const Mir) !void {
    const data = global.insts.items(.data)[global.insts.len - 1];
    const block_data = global.extraData(data.pl, Mir.Inst.Block);

    var extra_index: u32 = 0;
    while (extra_index < block_data.insts_len) : (extra_index += 1) {
        const inst_index = global.extra[data.pl + 1 + extra_index];
        const ref = try generateGlobalInst(backend, global, inst_index);
        try backend.map.put(backend.gpa, inst_index, ref);
    }
}

fn generateGlobalInst(backend: *Backend, global: *const Mir, inst: u32) !c.LLVMValueRef {
    return switch (global.insts.items(.tag)[inst]) {
        .proto => generateFunctionProto(backend, global, inst),
        .constant => try constant(backend.gpa, global, inst),
        .alloc => try generateGlobal(backend, global, inst),
        .store => generateInitializer(backend, global, inst),
        else => ref: {
            std.debug.print("{}\n", .{global.insts.items(.tag)[inst]});
            break :ref c.LLVMConstPointerNull(c.LLVMInt32Type());
        },
    };
}

fn generateGlobal(backend: *Backend, global: *const Mir, inst: u32) !c.LLVMValueRef {
    const data = global.insts.items(.data)[inst];

    const ty = try getLlvmType(backend.gpa, data.ty);
    const ref = c.LLVMAddGlobal(backend.module, ty, "");
    return ref;
}

fn generateInitializer(backend: *Backend, global: *const Mir, inst: u32) c.LLVMValueRef {
    const data = global.insts.items(.data)[inst];

    const addr = resolveGlobalRef(backend, data.bin_op.lref);
    const val = resolveGlobalRef(backend, data.bin_op.rref);
    c.LLVMSetInitializer(addr, val);
    return val;
}

fn generateFunctionProto(backend: *Backend, global: *const Mir, inst: u32) !c.LLVMValueRef {
    const data = global.insts.items(.data)[inst];

    const function_ty = try getLlvmType(backend.gpa, global.refToType(data.ty_pl.ty));
    var name = [_]u8{'a', 0};
    name[0] += @intCast(u8, data.ty_pl.pl);
    const function_ref = c.LLVMAddFunction(backend.module, &name, function_ty);
    try backend.map.put(backend.gpa, inst, function_ref);
    
    return function_ref;
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
}

fn block(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) Error!void {
    const data = mir.insts.items(.data)[inst];
    const block_data = mir.extraData(data.pl, Mir.Inst.Block);

    var extra_index: u32 = 0;
    while (extra_index < block_data.insts_len) : (extra_index += 1) {
        const inst_index = mir.extra[data.pl + 1 + extra_index];
        const ref = switch (mir.insts.items(.tag)[inst_index]) {
            .constant => try constant(codegen.gpa, mir, inst_index),
            .add, .sub, .mul, .div, .mod => try codegen.binaryOp(mir, inst_index),
            .ret => codegen.ret(mir, inst_index),
            .alloc => try codegen.alloc(mir, inst_index),
            .load => codegen.load(mir, inst_index),
            .store => codegen.store(mir, inst_index),
            .cmp_eq, .cmp_ne,
            .cmp_ule, .cmp_uge, .cmp_ult, .cmp_ugt,
            .cmp_sle, .cmp_sge, .cmp_slt, .cmp_sgt,
            .cmp_fle, .cmp_fge, .cmp_flt, .cmp_fgt => codegen.cmp(mir, inst_index),
            .zext => try codegen.zext(mir, inst_index),
            .sext => try codegen.sext(mir, inst_index),
            .fpext => try codegen.fpext(mir, inst_index),
            .param => try codegen.functionParam(mir, inst_index, extra_index),
            .branch_single => {
                try codegen.branchSingle(mir, inst_index);
                continue;
            },
            .branch_double => {
                try codegen.branchDouble(mir, inst_index);
                continue;
            },
            .dbg_value => try codegen.dbgValue(mir, inst_index),
            .ty => continue,
            else => {
                std.debug.print("{}\n", .{mir.insts.items(.tag)[inst_index]});
                continue;
            },
        };
        try codegen.map.put(codegen.gpa, inst_index, ref);
    }
}

fn constant(gpa: Allocator, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    const value = mir.values[data.ty_pl.pl];
    const mir_ty = mir.refToType(data.ty_pl.ty);
    switch (mir_ty.kind()) {
        .comptime_uint,
        .comptime_sint,
        .comptime_float => return c.LLVMConstPointerNull(c.LLVMInt32Type()),
        else => {},
    }

    const ty = try getLlvmType(gpa, mir_ty);
    switch (mir_ty.kind()) {
        .uint => return c.LLVMConstInt(ty, @intCast(c_ulonglong, value.int), 0),
        .sint => return c.LLVMConstInt(ty, @intCast(c_ulonglong, value.int), 1),
        .float => return c.LLVMConstReal(ty, value.float),
        else => unreachable,
    }
}

fn binaryOp(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    // TODO: handle global refs
    const lref = codegen.resolveLocalRef(data.bin_op.lref);
    const rref = codegen.resolveLocalRef(data.bin_op.rref);
    
    if (c.LLVMGetTypeKind(c.LLVMTypeOf(lref)) != c.LLVMIntegerTypeKind) {
        // floats
        return switch (mir.insts.items(.tag)[inst]) {
            .add => c.LLVMBuildFAdd(codegen.builder, lref, rref, ""),
            .sub => c.LLVMBuildFSub(codegen.builder, lref, rref, ""),
            .mul => c.LLVMBuildFMul(codegen.builder, lref, rref, ""),
            .div => c.LLVMBuildFDiv(codegen.builder, lref, rref, ""),
            .mod => return c.LLVMBuildFRem(codegen.builder, lref, rref, ""),
            else => unreachable,
        };
    } else {
        // TODO: take care of signed properly
        return switch (mir.insts.items(.tag)[inst]) {
            .add => return c.LLVMBuildAdd(codegen.builder, lref, rref, ""),
            .sub => return c.LLVMBuildSub(codegen.builder, lref, rref, ""),
            .mul => return c.LLVMBuildMul(codegen.builder, lref, rref, ""),
            .div => {
                const lty = try mir.resolveTy(data.bin_op.lref);
                const rty = try mir.resolveTy(data.bin_op.rref);
                if (lty.intSign() or rty.intSign()) {
                    return c.LLVMBuildSDiv(codegen.builder, lref, rref, "");
                } else {
                    return c.LLVMBuildUDiv(codegen.builder, lref, rref, "");
                }
            },
            .mod => return c.LLVMBuildSRem(codegen.builder, lref, rref, ""),
            else => unreachable,
        };
    }
}

fn ret(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];
    
    if (data.un_op == Mir.Ref.void_val) {
        return c.LLVMBuildRetVoid(codegen.builder);
    } else {
        const ref = codegen.resolveLocalRef(data.un_op);
        return c.LLVMBuildRet(codegen.builder, ref);
    }
}

fn alloc(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const ty = try getLlvmType(codegen.gpa, data.ty);
    return c.LLVMBuildAlloca(codegen.builder, ty, "");
}

fn store(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const addr = codegen.resolveLocalRef(data.bin_op.lref);
    const val = codegen.resolveLocalRef(data.bin_op.rref);
    return c.LLVMBuildStore(codegen.builder, val, addr);
}

fn load(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const addr = codegen.resolveLocalRef(data.un_op);
    const ty = c.LLVMGetAllocatedType(addr);
    return c.LLVMBuildLoad2(codegen.builder, ty, addr, "");
}

fn cmp(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) c.LLVMValueRef {
    const tag = mir.insts.items(.tag)[inst];
    const data = mir.insts.items(.data)[inst];

    const lref = codegen.resolveLocalRef(data.bin_op.lref);
    const rref = codegen.resolveLocalRef(data.bin_op.rref);
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

fn branchSingle(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) Error!void {
    const data = mir.insts.items(.data)[inst];

    const condition = codegen.resolveLocalRef(data.op_pl.op);
    const exec_true = c.LLVMAppendBasicBlock(codegen.function, "exec_true");
    const exit = c.LLVMAppendBasicBlock(codegen.function, "exit");
    _ = c.LLVMBuildCondBr(codegen.builder, condition, exec_true, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_true);
    try codegen.block(mir, data.op_pl.pl);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exit);
}

fn branchDouble(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) Error!void {
    const data = mir.insts.items(.data)[inst];
    const condbr = mir.extraData(data.op_pl.pl, Mir.Inst.CondBr);

    const condition = codegen.resolveLocalRef(data.op_pl.op);
    const exec_true = c.LLVMAppendBasicBlock(codegen.function, "exec_true");
    const exec_false = c.LLVMAppendBasicBlock(codegen.function, "exec_false");
    const exit = c.LLVMAppendBasicBlock(codegen.function, "exit");
    _ = c.LLVMBuildCondBr(codegen.builder, condition, exec_true, exec_false);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_true);
    try codegen.block(mir, condbr.exec_true);
    if (c.LLVMGetBasicBlockTerminator(exec_true) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exec_false);
    try codegen.block(mir, condbr.exec_false);
    if (c.LLVMGetBasicBlockTerminator(exec_false) == null)
        _ = c.LLVMBuildBr(codegen.builder, exit);

    c.LLVMPositionBuilderAtEnd(codegen.builder, exit);
}

fn functionParam(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index, index: u32) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const id = data.ty_pl.pl;
    const param_str = try mir.interner.get(id);
    const param_ref = c.LLVMGetParam(codegen.function, index);
    c.LLVMSetValueName2(param_ref, param_str.ptr, param_str.len);
    return param_ref;
}

fn dbgValue(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const id = data.op_pl.pl;
    const ident_str = try mir.interner.get(id);
    const ref = codegen.resolveLocalRef(data.op_pl.op);
    c.LLVMSetValueName2(ref, ident_str.ptr, ident_str.len);
    return ref;
}

fn zext(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const ty = try getLlvmType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveLocalRef(data.ty_op.op);
    return c.LLVMBuildZExt(codegen.builder, ref, ty, "");
}

fn sext(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const ty = try getLlvmType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveLocalRef(data.ty_op.op);
    return c.LLVMBuildSExt(codegen.builder, ref, ty, "");
}

fn fpext(codegen: *CodeGen, mir: *const Mir, inst: Mir.Index) !c.LLVMValueRef {
    const data = mir.insts.items(.data)[inst];

    const ty = try getLlvmType(codegen.gpa, mir.refToType(data.ty_op.ty));
    const ref = codegen.resolveLocalRef(data.ty_op.op);
    return c.LLVMBuildFPExt(codegen.builder, ref, ty, "");
}
