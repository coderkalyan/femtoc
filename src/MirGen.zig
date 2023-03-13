const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Analyzer = @import("Analyzer.zig");
const MirMap = @import("MirMap.zig");
const Module = @import("Module.zig");
const Compilation = @import("Compilation.zig");

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

pub fn addInst(mg: *MirGen, inst: Mir.Inst) !Mir.Index {
    const result = @intCast(Mir.Index, mg.insts.len);
    try mg.insts.append(mg.gpa, inst);
    return result;
}

pub fn addExtra(mg: *MirGen, extra: anytype) !u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try mg.extra.ensureUnusedCapacity(mg.gpa, fields.len);
    const len = @intCast(u32, mg.extra.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.field_type == u32);
        mg.extra.appendAssumeCapacity(@field(extra, field.name));
    }

    return len;
}

pub fn addValue(mg: *MirGen, val: Value) !u32 {
    const len = @intCast(u32, mg.values.items.len);
    try mg.values.append(mg.gpa, val);
    return len;
}

pub fn resolveTy(mg: *MirGen, ref: Mir.Ref) !Type {
    if (Mir.refToIndex(ref)) |index| {
        return switch (mg.insts.items(.tag)[index]) {
            .constant => mg.insts.items(.data)[index].ty_pl.ty,
            .add, .sub,
            .mul, .div, .mod => mg.resolveTy(mg.insts.items(.data)[index].bin_op.lref),
            .eq, .neq, .geq,
            .leq, .gt, .lt => mg.resolveTy(mg.insts.items(.data)[index].bin_op.lref),
            .alloc => mg.insts.items(.data)[index].ty,
            else => Error.NotImplemented,
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => .{ .tag = .comptime_uint },
            .void_val => .{ .tag = .void },
            else => Error.NotImplemented,
        };
    }
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

    try analyzer.analyzeModule(@intCast(u32, mg.hir.insts.len - 1));

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = analyzer.interner,
    };
}
