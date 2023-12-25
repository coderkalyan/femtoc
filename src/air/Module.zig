const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Module = @This();

name: ?InternPool.Index,
decls: []InternPool.Index,
