const std = @import("std");
const Hir = @import("../Hir.zig");

const Backend = @This();
const Error = error{
    OutOfMemory,
    InternTableFull,
    InvalidId,
    NotImplemented,
};

generateFn: *const fn (*Backend, hir: *const Hir) Error!void,

pub fn generate(backend: *Backend, hir: *const Hir) !void {
    try backend.generateFn(backend, hir);
}
