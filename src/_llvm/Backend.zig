const std = @import("std");
const Hir = @import("../Hir.zig");
const BackendInterface = @import("../codegen/Backend.zig");
const DeclGen = @import("DeclGen.zig");
const Context = @import("Context.zig");
const Allocator = std.mem.Allocator;

const Backend = @This();

iface: BackendInterface,
gpa: Allocator,

pub fn init(gpa: Allocator) Backend {
    return .{
        .gpa = gpa,
        .iface = .{ .generateFn = generate },
    };
}

fn generate(iface: *BackendInterface, hir: *const Hir) void {
    const self = @fieldParentPtr(Backend, "iface", iface);
    const module_pl = hir.insts.items(.data)[hir.module_index].pl_node.pl;
    const module = hir.extraData(module_pl, Hir.Inst.Module);

    var arena = std.heap.ArenaAllocator.init(self.gpa);
    defer arena.deinit();
    var context = Context.init(arena.allocator(), "femto_main");
    defer context.deinit();

    var extra_index: u32 = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const id = hir.extra_data[base + extra_index];
        const inst = hir.extra_data[base + extra_index + 1];

        var declgen = DeclGen{
            .arena = arena.allocator(),
            .context = &context,
            .hir = hir,
            .name = id,
            .inline_block = inst,
        };
        declgen.generate() catch unreachable;
    }

    context.dump();
}
