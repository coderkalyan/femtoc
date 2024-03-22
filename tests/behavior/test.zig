const std = @import("std");
pub const bool_logic = @import("bool_logic.zig");
pub const factorial = @import("factorial.zig");

test {
    std.testing.refAllDecls(@This());
}
