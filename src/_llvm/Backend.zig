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
map: std.AutoHashMapUnmanaged(u32, c.LLVMValueRef),
iface: BackendInterface,

pub fn init(gpa: Allocator) Backend {
    return .{
        .gpa = gpa,
        .map = .{},
        .iface = .{ .generateFn = generate },
    };
}

fn generate(iface: *BackendInterface, hir: *const Hir) !void {
    const self = @fieldParentPtr(Backend, "iface", iface);
    const module_pl = hir.insts.items(.data)[hir.module_index].pl_node.pl;
    const module = hir.extraData(module_pl, Hir.Inst.Module);

    var arena = std.heap.ArenaAllocator.init(self.gpa);
    defer arena.deinit();
    var context = Context.init(arena.allocator(), "femto_main");
    defer context.deinit();
    // var bodies = std.ArrayList(Hir.Index).init(arena.allocator());
    // defer bodies.deinit();
    var codegens = std.ArrayList(CodeGen).init(arena.allocator());
    defer codegens.deinit();

    var extra_index: u32 = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const id = hir.extra_data[base + extra_index];
        const inst = hir.extra_data[base + extra_index + 1];

        const declinfo = try DeclInfo.generate(hir, self.gpa, inst, id);
        var declgen = DeclGen{
            .gpa = self.gpa,
            .arena = arena.allocator(),
            .context = &context,
            .hir = hir,
            .name = id,
            .inline_block = inst,
            .map = .{},
            .global_map = &self.map,
            .decl_info = declinfo,
        };
        const val = try declgen.generate(&codegens);
        try self.map.put(arena.allocator(), id, val);
    }

    var i: u32 = 0;
    while (i < codegens.items.len) : (i += 1) {
        const codegen = &codegens.items[i];
        defer self.gpa.destroy(codegen.builder);
        defer codegen.builder.deinit();
        try codegen.generate();
    }
    context.dump();
}
