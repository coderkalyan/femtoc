const std = @import("std");

export fn expect_true(predicate: bool) void {
    std.testing.expect(predicate);
}

export fn debug_println(message: [*]u8, len: u64) void {
    std.debug.print("{s}\n", .{message[0..len]});
}

extern fn test_main() void;

test "test runner" {
    test_main();
}
