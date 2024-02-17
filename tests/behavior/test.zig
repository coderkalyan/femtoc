const std = @import("std");
pub const bool_logic = @import("bool_logic.zig");

test {
    std.testing.refAllDecls(@This());
}
