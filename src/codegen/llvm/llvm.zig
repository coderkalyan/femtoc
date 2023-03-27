const std = @import("std");
const Mir = @import("../../Mir.zig");
const typing = @import("../../typing.zig");
const DeclGen = @import("DeclGen.zig");
const CodeGen = @import("CodeGen.zig");
const Compilation = @import("../../Compilation.zig");
const Decl = Compilation.Decl;

pub const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/BitWriter.h");
    @cInclude("llvm-c/Disassembler.h");
});

const Allocator = std.mem.Allocator;
const llvm = @This();

pub const Type = typing.Type;
pub const Value = c.LLVMValueRef;

pub const verifyModule = c.LLVMVerifyModule;
pub const dumpModule = c.LLVMDumpModule;

pub const Error = error {
    WriteBitcodeFailed,
    WriteIRFailed,
    VerificationFailed,
};

pub const Context = struct {
    context: c.LLVMContextRef,

    pub fn create() Context {
        return .{ .context = c.LLVMContextCreate() };
    }

    pub fn destroy(context: Context) void {
        c.LLVMContextDispose(context.context);
    }
};

pub const Module = struct {
    module: c.LLVMModuleRef,

    pub fn create(context: Context, name: [:0]const u8) Module {
        return .{
            .module = c.LLVMModuleCreateWithNameInContext(name.ptr, context.context),
        };
    }

    pub fn destroy(module: Module) void {
        c.LLVMDisposeModule(module.module);
    }

    pub fn print(module: Module) void {
        c.LLVMDumpModule(module.module);
    }

    pub fn writeBitcode(module: Module, name: [:0]const u8) !void {
        const result = c.LLVMWriteBitcodeToFile(module.module, name.ptr);
        if (result != 0) return Error.WriteBitcodeFailed;
    }

    pub fn writeIR(module: Module, name: [:0]const u8) !void {
        var err = @intToPtr([*c]u8, 0);
        const result = c.LLVMPrintModuleToFile(module.module, name.ptr, &err);
        if (result != 0) {
            std.debug.print("{s}\n", .{err});
            c.LLVMDisposeMessage(err);
            return Error.WriteIRFailed;
        }
    }

    pub fn verify(module: Module) !void {
        var err = @intToPtr([*c]u8, 0);
        const result = c.LLVMVerifyModule(module.module, c.LLVMPrintMessageAction, &err);
        if (result != 0) {
            std.debug.print("{s}\n", .{err});
            return Error.VerificationFailed;
        }
    }
};

pub const Backend = struct {
    gpa: Allocator,
    comp: *const Compilation,
    context: Context,
    module: Module,
    // TODO: change to Decl.Index
    globals: std.AutoHashMapUnmanaged(*Decl, c.LLVMValueRef),

    pub fn create(gpa: Allocator, comp: *const Compilation, name: [:0]const u8) Backend {
        const context = Context.create();
        return .{
            .gpa = gpa,
            .comp = comp,
            .context = context,
            .module = Module.create(context, name),
            .globals = .{},
        };
    }

    pub fn destroy(backend: *Backend) void {
        backend.globals.deinit(backend.gpa);
        backend.module.destroy();
        backend.context.destroy();
    }

    pub fn updateDecl(backend: *Backend, decl: *Decl) !void {
        var dg = DeclGen {
            .gpa = backend.gpa,
            .decl = decl,
            .backend = backend,
        };
        try dg.generate();
    }

    pub fn generateBody(backend: *Backend, decl: *Decl, mir: *const Mir) !void {
        const function = c.LLVMGetNamedFunction(backend.module.module, decl.name);
        const builder = Builder.create(backend, function);
        defer builder.destroy();

        var codegen = CodeGen {
            .gpa = backend.gpa,
            .mir = mir,
            .comp = backend.comp,
            .map = .{},
            .builder = builder,
            .alloc_block = null,
        };
        try codegen.generate();
    }
};

pub const Builder = struct {
    gpa: Allocator,
    backend: *Backend,
    context: c.LLVMContextRef,
    function: c.LLVMValueRef,
    builder: c.LLVMBuilderRef,

    pub fn create(backend: *Backend, function: c.LLVMValueRef) Builder {
        return .{ 
            .gpa = backend.gpa,
            .backend = backend,
            .context = backend.context.context,
            .function = function,
            .builder = c.LLVMCreateBuilderInContext(backend.context.context),
        };
    }

    pub fn destroy(builder: *const Builder) void {
        c.LLVMDisposeBuilder(builder.builder);
    }

    pub fn appendBlock(builder: *Builder, name: [:0]const u8) c.LLVMBasicBlockRef {
        return c.LLVMAppendBasicBlock(builder.function, name.ptr);
    }

    pub fn deleteBlock(builder: *Builder, bb: c.LLVMBasicBlockRef) void {
        _ = builder;
        c.LLVMDeleteBasicBlock(bb);
    }

    pub fn getInsertBlock(builder: *Builder) c.LLVMBasicBlockRef {
        return c.LLVMGetInsertBlock(builder.builder);
    }

    pub fn positionAtEnd(builder: *Builder, bb: c.LLVMBasicBlockRef) void {
        c.LLVMPositionBuilderAtEnd(builder.builder, bb);
    }

    // constant values
    pub fn addUint(builder: *Builder, ty: Type, val: u64) c.LLVMValueRef {
        const llvm_type = builder.getBasicType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(c_ulonglong, val), 0);
    }

    pub fn addSint(builder: *Builder, ty: Type, val: u64) c.LLVMValueRef {
        const llvm_type = builder.getBasicType(ty);
        return c.LLVMConstInt(llvm_type, @intCast(c_ulonglong, val), 1);
    }

    pub fn addFloat(builder: *Builder, ty: Type, val: f64) c.LLVMValueRef {
        const llvm_type = builder.getBasicType(ty);
        return c.LLVMConstReal(llvm_type, val);
    }

    // memory
    pub fn addAlloca(builder: *Builder, ty: Type) !c.LLVMValueRef {
        const llvm_type = try builder.getType(ty);
        return c.LLVMBuildAlloca(builder.builder, llvm_type, "");
    }

    pub fn addLoad(builder: *Builder, addr: c.LLVMValueRef) c.LLVMValueRef {
        const llvm_type = c.LLVMGetAllocatedType(addr);
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

    pub fn addCmp(builder: *Builder, kind: Cmp,
                  lref: c.LLVMValueRef, rref: c.LLVMValueRef) c.LLVMValueRef {
        return switch (kind) {
            .cmp_ieq => c.LLVMBuildICmp(builder.builder, c.LLVMIntEQ, lref, rref, ""),
            .cmp_ine => c.LLVMBuildICmp(builder.builder, c.LLVMIntNE, lref, rref, ""),

            .cmp_ule => c.LLVMBuildICmp(builder.builder, c.LLVMIntULE, lref, rref, ""),
            .cmp_uge => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGE, lref, rref, ""),
            .cmp_ult => c.LLVMBuildICmp(builder.builder, c.LLVMIntULT, lref, rref, ""),
            .cmp_ugt => c.LLVMBuildICmp(builder.builder, c.LLVMIntUGT, lref, rref, ""),

            .cmp_sle => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLE, lref, rref, ""),
            .cmp_sge => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGE, lref, rref, ""),
            .cmp_slt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSLT, lref, rref, ""),
            .cmp_sgt => c.LLVMBuildICmp(builder.builder, c.LLVMIntSGT, lref, rref, ""),

            .cmp_feq => c.LLVMBuildICmp(builder.builder, c.LLVMRealOEQ, lref, rref, ""),
            .cmp_fne => c.LLVMBuildICmp(builder.builder, c.LLVMRealONE, lref, rref, ""),

            .cmp_fle => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLE, lref, rref, ""),
            .cmp_fge => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGE, lref, rref, ""),
            .cmp_flt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOLT, lref, rref, ""),
            .cmp_fgt => c.LLVMBuildFCmp(builder.builder, c.LLVMRealOGT, lref, rref, ""),
        };
    }

    // control flow
    pub fn addBranch(builder: *Builder, bb: c.LLVMBasicBlockRef) void {
        _ = c.LLVMBuildBr(builder.builder, bb);
    }

    pub fn addCondBranch(builder: *Builder, cond: c.LLVMValueRef,
                         a: c.LLVMBasicBlockRef, b: c.LLVMBasicBlockRef) void {
        _ = c.LLVMBuildCondBr(builder.builder, cond, a, b);
    }

    pub fn addCall(builder: *Builder, ty: Type,
                   addr: c.LLVMValueRef, args: []c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.getType(ty);
        return c.LLVMBuildCall2(builder.builder, llvm_type, addr,
                                args.ptr, @intCast(c_uint, args.len), "");
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
        const llvm_type = try builder.getType(ty);
        return c.LLVMBuildZExt(builder.builder, val, llvm_type, "");
    }

    pub fn addSext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.getType(ty);
        return c.LLVMBuildSExt(builder.builder, val, llvm_type, "");
    }

    pub fn addFpext(builder: *Builder, ty: Type, val: c.LLVMValueRef) !c.LLVMValueRef {
        const llvm_type = try builder.getType(ty);
        return c.LLVMBuildFPExt(builder.builder, val, llvm_type, "");
    }

    // basic types are guaranteed not to allocate, so they won't throw errors
    pub fn getBasicType(builder: *Builder, ty: Type) c.LLVMTypeRef {
        return llvm.getBasicType(builder.context, ty);
    }

    pub fn getType(builder: *Builder, ty: Type) !c.LLVMTypeRef {
        return llvm.getType(builder.gpa, builder.context, ty);
    }
};

pub fn getBasicType(context: c.LLVMContextRef, ty: typing.Type) c.LLVMTypeRef {
    return switch (ty.kind()) {
        .void => c.LLVMVoidTypeInContext(context),
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint, .sint => c.LLVMIntTypeInContext(context, ty.basic.width),
        .float => switch (ty.basic.width) {
            32 => c.LLVMFloatTypeInContext(context),
            64 => c.LLVMDoubleTypeInContext(context),
            else => unreachable,
        },
        else => unreachable,
    };
}

pub fn getType(gpa: Allocator, context: c.LLVMContextRef, ty: typing.Type) !c.LLVMTypeRef {
    return switch (ty.kind()) {
        .void => c.LLVMVoidTypeInContext(context),
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint, .sint => c.LLVMIntTypeInContext(context, ty.basic.width),
        .float => switch (ty.basic.width) {
            32 => c.LLVMFloatTypeInContext(context),
            64 => c.LLVMDoubleTypeInContext(context),
            else => unreachable,
        },
        .function => {
            const function = ty.extended.cast(typing.Type.Function).?;

            // TODO: use arena
            const params = try gpa.alloc(c.LLVMTypeRef, function.param_types.len);
            defer gpa.free(params);
            for (function.param_types) |param, i| {
                params[i] = try getType(gpa, context, param);
            }
            const return_type = try getType(gpa, context, function.return_type);

            return c.LLVMFunctionType(return_type,
            params.ptr, @intCast(c_uint, params.len), 0);
        },
        else => unreachable,
    };
}
