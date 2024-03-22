const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

var touched: bool = false;

const u16_max = std.math.maxInt(u16);
const u32_max = std.math.maxInt(u32);

// flags side effect if called
export fn touch() void {
    touched = true;
}

extern fn arith_sum(n: u32) u32;
extern fn loop_zero_iter() void;
extern fn loop_zero_iter_func() void;
extern fn loop_zero_while() void;
extern fn loop_zero_while_func() void;
extern fn loop_once_iter() u32;
extern fn loop_once_while() u32;
extern fn count_lt(start: u16, end: u16) u16;
extern fn count_le(start: u16, end: u16) u16;
extern fn count_ge_dec(start: u16, end: u16) u16;
extern fn break_simple(n: u32) u32;
extern fn break_nested_if(n: u32) u32;

test "arithmetic sum" {
    var i: u32 = 1;
    var sum: u32 = 0;
    while (i <= 10) : (i += 1) {
        sum += i;
        try expectEqual(arith_sum(i), sum);
    }
}

test "loop zero times" {
    touched = false;
    loop_zero_iter();
    try expect(!touched);

    touched = false;
    loop_zero_iter_func();
    try expect(!touched);

    touched = false;
    loop_zero_while();
    try expect(!touched);

    touched = false;
    loop_zero_while_func();
    try expect(!touched);
}

test "loop once" {
    touched = false;
    try expectEqual(loop_once_iter(), 1);
    try expect(touched);

    touched = false;
    try expectEqual(loop_once_while(), 1);
    try expect(touched);
}

test "loop count" {
    var i: u16 = undefined;

    // start == end
    i = 0;
    while (i <= 10) : (i += 1) {
        try expectEqual(count_lt(i, i), 0);
        try expectEqual(count_le(i, i), 1);
        if (i != 0) try expectEqual(count_ge_dec(i, i), 0);
    }

    // one loop count
    i = 0;
    while (i <= 10) : (i += 1) {
        try expectEqual(count_lt(i, i + 1), 1);
        try expectEqual(count_le(i, i), 1);
        if (i != 0) try expectEqual(count_ge_dec(i + 1, i), 1);
    }

    // u16 max
    try expectEqual(count_lt(0, u16_max), u16_max);
    try expectEqual(count_le(0, u16_max - 1), u16_max);
    try expectEqual(count_ge_dec(u16_max, 1), u16_max - 1);
}

test "loop break" {
    try expectEqual(break_simple(1), 1);
    try expectEqual(break_simple(123), 123);
    try expectEqual(break_simple(u32_max), 10000);

    try expectEqual(break_nested_if(1), 1);
    try expectEqual(break_nested_if(2), 10000);
    try expectEqual(break_nested_if(123), 123);
    try expectEqual(break_nested_if(100000), 10000);
}
