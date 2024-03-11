const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

extern fn factorial_rec(n: u32) u32;
extern fn factorial_iter(n: u32) u32;

test "recursive factorial" {
    try expectEqual(factorial_rec(0), 1);
    try expectEqual(factorial_rec(1), 1);
    try expectEqual(factorial_rec(2), 2);
    try expectEqual(factorial_rec(3), 6);
    try expectEqual(factorial_rec(4), 24);
    try expectEqual(factorial_rec(5), 120);
    try expectEqual(factorial_rec(10), 3628800);
}

test "iterative factorial" {
    try expectEqual(factorial_iter(0), 1);
    try expectEqual(factorial_iter(1), 1);
    try expectEqual(factorial_iter(2), 2);
    try expectEqual(factorial_iter(3), 6);
    try expectEqual(factorial_iter(4), 24);
    try expectEqual(factorial_iter(5), 120);
    try expectEqual(factorial_iter(10), 3628800);
}
