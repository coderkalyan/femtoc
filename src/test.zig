const std = @import("std");

// pub const alu = @import("alu.zig");
// pub const scope = @import("scope.zig");
// pub const typing = @import("typing.zig");
// pub const value = @import("value.zig");
pub const lex = @import("lex.zig");
pub const parser_test = @import("tests/parser_test.zig");

test {
    std.testing.refAllDecls(@This());
}
