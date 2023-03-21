const std = @import("std");
const Hir = @import("Hir.zig");
const Type = @import("typing.zig").Type;
const Value = @import("value.zig").Value;

pub const Decl = struct {
    name: [*:0]const u8,
    ty: Type,
    val: Value,
};

pub const Function = struct {
    decl: Decl.Index,
    hir_inst: Hir.Index,
};

pub const Compilation = struct {
    decls: std.SegmentedList(Decl),
};
