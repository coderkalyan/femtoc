const std = @import("std");
const Hir = @import("../Hir.zig");
const BackendInterface = @import("../codegen/Backend.zig");
const DeclGen = @import("DeclGen.zig");
const CodeGen = @import("CodeGen.zig");
const DeclInfo = @import("../hir/DeclInfo.zig");
const Context = @import("Context.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;

const Backend = @This();

gpa: Allocator,
arena: Allocator,
context: Context,

pub fn init(gpa: Allocator, arena: Allocator) Backend {
    return .{
        .gpa = gpa,
        .arena = arena,
        .context = Context.init(arena, "femto_main"),
    };
}

pub fn deinit(self: *Backend) void {
    self.context.deinit();
}

pub fn generate(self: *Backend, hir: *const Hir) !void {
    const module_pl = hir.insts.items(.data)[hir.module_index].pl_node.pl;
    const module = hir.extraData(module_pl, Hir.Inst.Module);

    var codegens = std.ArrayList(CodeGen).init(self.arena);
    defer codegens.deinit();
    var global_map = std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef){};
    defer global_map.deinit(self.arena);

    var extra_index: u32 = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const id = hir.extra_data[base + extra_index];
        const inst = hir.extra_data[base + extra_index + 1];

        const declinfo = try DeclInfo.generate(hir, self.gpa, inst, id);
        var declgen = DeclGen{
            .gpa = self.gpa,
            .arena = self.arena,
            .context = &self.context,
            .hir = hir,
            .name = id,
            .inline_block = inst,
            .global_map = &global_map,
            .decl_info = declinfo,
        };
        const val = try declgen.generate(&codegens);
        try global_map.put(self.arena, id, val);
    }

    var i: u32 = 0;
    while (i < codegens.items.len) : (i += 1) {
        const codegen = &codegens.items[i];
        defer self.gpa.destroy(codegen.builder);
        defer codegen.builder.deinit();
        try codegen.generate();
    }
}

pub fn dumpToStdout(self: *Backend) !void {
    self.context.dump();
}

pub fn printToFile(self: *Backend, filename: []const u8) !void {
    try self.context.printToFile(filename);
}
