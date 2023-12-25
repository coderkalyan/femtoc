const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Decl = @This();

name: ?InternPool.Index,
ty: InternPool.Index,
initializer: ?InternPool.Index,
mutable: bool,
linkage: Linkage,

pub const Linkage = enum {
    internal,
    external,
};
