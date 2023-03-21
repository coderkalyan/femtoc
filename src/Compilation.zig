const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Value = @import("value.zig").Value;
const Driver = @import("Driver.zig");

pub const Decl = struct {
    name: [*:0]const u8,
    ty: Type,
    val: Value,
};

pub const Function = struct {
    decl: Decl.Index,
    hir_inst: Hir.Index,
};

const Compilation = @This();

// decls: std.SegmentedList(Decl),
global: Mir,
mir: []const Mir,
config: *Driver.Configuration,
