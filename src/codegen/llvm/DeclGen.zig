const std = @import("std");
const Compilation = @import("../../Compilation.zig");
const llvm = @import("llvm.zig");
const Value = @import("../../value.zig").Value;

const Allocator = std.mem.Allocator;
const Decl = Compilation.Decl;
const DeclGen = @This();

gpa: Allocator,
// TODO: make const, change ownership of backend
// comp: *Compilation,
decl: *Decl,
// TODO: is LLVM thread safe?
backend: *llvm.Backend,

pub fn generate(dg: *DeclGen) !void {
    const decl = dg.decl;
    if (dg.backend.globals.get(decl)) |val| {
        // already exists, update
        // currently, only thing to update is the name
        llvm.c.LLVMSetValueName2(val, decl.name, std.mem.len(decl.name));
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
    const decl = dg.decl;
    // const function_val = decl.val.payload.cast(Value.Payload.Function).?;
    // const function_decl = function_val.func;
    // _ = function_decl;

    const llvm_type = try llvm.getType(dg.gpa, dg.backend.context.context, decl.ty);
    const func = llvm.c.LLVMAddFunction(dg.backend.module.module, decl.name, llvm_type);
    return func;
}

fn global(dg: *DeclGen) !llvm.c.LLVMValueRef {
    const decl = dg.decl;
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
// fn reference(dg: *DeclGen) !void {
//     const decl = dg.decl;
//     const ref_val = decl.val.payload.cast(Value.Payload.Reference).?;
//     const ref = dg.comp.backend.globals.get(dg.comp.decls.at(ref_val.ref)).?;
//     const llvm_type = llvm.c.LLVMTypeOf(ref);
//     // const llvm_type = try llvm.getType(dg.gpa, decl.ty);
//     // TODO: use Alias2
//     // const val = llvm.c.LLVMAddAlias(dg.module, llvm_type, ref, decl.name);
//     const val = llvm.c.LLVMAddGlobal(dg.module, llvm_type, decl.name);
//     llvm.c.LLVMSetInitializer(val, ref);
//     try dg.comp.backend.globals.put(dg.gpa, decl, val);
// }
