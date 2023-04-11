const std = @import("std");
const Compilation = @import("../../Compilation.zig");
const llvm = @import("llvm.zig");
const Value = @import("../../value.zig").Value;

const Allocator = std.mem.Allocator;
const Decl = Compilation.Decl;
const DeclGen = @This();

gpa: Allocator,
// TODO: make const, change ownership of backend
// comp: *const Compilation,
decl_index: Decl.Index,
// TODO: is LLVM thread safe?
backend: *llvm.Backend,

pub fn generate(dg: *DeclGen) !void {
    var comp = dg.backend.comp;
    const decl = comp.declPtr(dg.decl_index);
    if (dg.backend.globals.get(decl)) |val| {
        // already exists, update
        llvm.c.LLVMSetValueName2(val, decl.name, std.mem.len(decl.name));
        const is_exported = comp.export_decls.contains(dg.decl_index);
        const linkage = if (is_exported) llvm.c.LLVMExternalLinkage else llvm.c.LLVMInternalLinkage;
        llvm.c.LLVMSetLinkage(val, @intCast(c_uint, linkage));
    } else {
        // create the decl
        // TODO: switch from value kind to decl kind (need to create that enum)
        const val = switch (decl.val.kind()) {
            .function => try dg.function(),
            else => try dg.global(),
            // .zero,
            // .one,
            // .u32,
            // .u64,
            // .f64 => try dg.,
            // .reference => try dg.reference(),
            // else => unreachable,
            // else => return,
        };
        try dg.backend.globals.put(dg.gpa, decl, val);
    }
}

fn function(dg: *DeclGen) !llvm.c.LLVMValueRef {
    const comp = dg.backend.comp;
    const decl = comp.declPtr(dg.decl_index);
    // const function_val = decl.val.payload.cast(Value.Payload.Function).?;
    // const function_decl = function_val.func;
    // _ = function_decl;

    const llvm_type = try llvm.getType(dg.gpa, dg.backend.context.context, decl.ty);
    const func = llvm.c.LLVMAddFunction(dg.backend.module.module, decl.name, llvm_type);
    return func;
}

fn global(dg: *DeclGen) !llvm.c.LLVMValueRef {
    const comp = dg.backend.comp;
    const decl = comp.declPtr(dg.decl_index);
    const llvm_type = try llvm.getType(dg.gpa, dg.backend.context.context, decl.ty);
    const g = llvm.c.LLVMAddGlobal(dg.backend.module.module, llvm_type, decl.name);
    llvm.c.LLVMSetGlobalConstant(g, @boolToInt(!decl.mut));

    const val = switch (decl.ty.kind()) {
        .uint => llvm.c.LLVMConstInt(llvm_type, @intCast(c_ulonglong, decl.val.toInt()), 0),
        .sint => llvm.c.LLVMConstInt(llvm_type, @intCast(c_ulonglong, decl.val.toInt()), 1),
        .float => llvm.c.LLVMConstReal(llvm_type, decl.val.toFloat()),
        else => unreachable,
    };
    llvm.c.LLVMSetInitializer(g, val);
    return g;
}
