const std = @import("std");
const Compilation = @import("../../Compilation.zig");
const llvm = @import("llvm.zig");
const Value = @import("../../value.zig").Value;

const Allocator = std.mem.Allocator;
const Decl = Compilation.Decl;
const DeclGen = @This();

gpa: Allocator,
comp: *const Compilation,
decl: *Decl,
// TODO: is LLVM thread safe?
module: llvm.Module,

pub fn generate(dg: *DeclGen) !void {
    const decl = dg.decl;
    switch (decl.ty.kind()) {
        .function => {
            try dg.function();
        },
        else => std.debug.assert(false),
    }
}

fn function(dg: *DeclGen) !void {
    const decl = dg.decl;
    const function_val = decl.val.payload.cast(Value.Payload.Function).?;
    const function_decl = function_val.func;
    std.debug.print("{s} {}\n", .{decl.name, function_decl.hir_inst});

    const llvm_type = try llvm.getType(dg.gpa, decl.ty);
    const func = llvm.addFunction(dg.module, decl.name, llvm_type);
    _ = func;
}
