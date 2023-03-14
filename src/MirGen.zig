const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Analyzer = @import("Analyzer.zig");
const MirMap = @import("MirMap.zig");
const Module = @import("Module.zig");

const math = std.math;

const Allocator = std.mem.Allocator;
const MirGen = @This();
const Value = Mir.Value;
const TypedValue = Mir.TypedValue;
const Error = error { TypeError, NotImplemented };

gpa: Allocator,
arena: Allocator,

hir: *const Hir,
map: MirMap,
global: ?Mir,
mir: std.ArrayListUnmanaged(Mir),

pub const Compilation = struct {
    global: Mir,
    mir: []const Mir,
};

pub fn generate(gpa: Allocator, hir: *const Hir) !Compilation {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var mirgen = MirGen {
        .gpa = gpa,
        .arena = arena.allocator(),
        .hir = hir,
        .map = MirMap.init(null),
        .global = null,
        .mir = .{},
    };
    const global = try mirgen.walkModule();

    return Compilation {
        .global = global,
        .mir = mirgen.mir.toOwnedSlice(gpa),
    };
}

pub fn walkModule(mg: *MirGen) !Mir {
    var analyzer = Analyzer {
        .mg = mg,
        .map = MirMap.init(&mg.map),
        .hir = mg.hir,
        .gpa = mg.gpa,
        .arena = mg.arena,
        .instructions = .{},
        .extra = .{},
        .values = .{},
        .scratch = .{},
        .errors = .{},
        .interner = &mg.hir.interner,
    };

    _ = try analyzer.analyzeModule(@intCast(u32, mg.hir.insts.len - 1));

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = analyzer.interner,
    };
}
