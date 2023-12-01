const std = @import("std");
const Hir = @import("../Hir.zig");

const Backend = @This();

generateFn: *const fn (*Backend, hir: *const Hir) void,

pub fn generate(backend: *Backend, hir: *const Hir) void {
    backend.generateFn(backend, hir);
}
