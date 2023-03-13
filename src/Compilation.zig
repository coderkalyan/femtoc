const std = @import("std");
const Mir = @import("Mir.zig");
const Allocator = std.mem.Allocator;

global: Mir,
mir: []const Mir,

const Compilation = @This();
