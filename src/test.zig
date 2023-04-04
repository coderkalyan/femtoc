const std = @import("std");

pub const alu = @import("alu.zig");
pub const coercion = @import("coercion.zig");
pub const scope = @import("scope.zig");
pub const typing = @import("typing.zig");
pub const value = @import("value.zig");
pub const lex = @import("lex.zig");
pub const integerLiteral = @import("integerLiteral.zig");

test {
    std.testing.refAllDecls(@This());
}
