const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

var dont_touch: bool = true;

// flags side effect if called
export fn skip() bool {
    dont_touch = false;
    return true;
}

extern fn bool_not(a: bool) bool;
extern fn bool_or(a: bool, b: bool) bool;
extern fn bool_and(a: bool, b: bool) bool;
extern fn bool_xor(a: bool, b: bool) bool;
extern fn bool_implies(a: bool, b: bool) bool;
extern fn bool_or_short() void;
extern fn bool_and_short() void;
extern fn bool_implies_short() void;

test "bool inversion" {
    try expectEqual(bool_not(true), false);
    try expectEqual(bool_not(false), true);
}

test "bool or" {
    try expectEqual(bool_or(true, true), true);
    try expectEqual(bool_or(true, false), true);
    try expectEqual(bool_or(false, true), true);
    try expectEqual(bool_or(false, false), false);

    dont_touch = true;
    bool_or_short();
    try expect(dont_touch);
}

test "bool and" {
    try expectEqual(bool_and(true, true), true);
    try expectEqual(bool_and(true, false), false);
    try expectEqual(bool_and(false, true), false);
    try expectEqual(bool_and(false, false), false);

    dont_touch = true;
    bool_and_short();
    try expect(dont_touch);
}

test "bool xor" {
    try expectEqual(bool_xor(true, true), false);
    try expectEqual(bool_xor(true, false), true);
    try expectEqual(bool_xor(false, true), true);
    try expectEqual(bool_xor(false, false), false);
}

test "bool implies" {
    try expectEqual(bool_implies(true, true), true);
    try expectEqual(bool_implies(true, false), false);
    try expectEqual(bool_implies(false, true), true);
    try expectEqual(bool_implies(false, false), true);

    dont_touch = true;
    bool_implies_short();
    try expect(dont_touch);
}
