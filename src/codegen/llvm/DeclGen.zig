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
        const val = switch (decl.val.kind()) {
            .function => try dg.function(),
            // .reference => try dg.reference(),
            // else => unreachable,
            else => return,
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
