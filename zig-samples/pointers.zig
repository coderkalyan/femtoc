const std = @import("std");

// Type your code here, or load an example.
export fn foo() void {
    const v1: u32 = 1;
    // const v2: u32 = 2;
    // var v3: u32 = 3;
    // var v4: u32 = 4;
    const v5 = v1;
    // const v6 = v3;

    const p1 = &v1;
    const p2 = &v1;
    const p3 = &v5;
    // const p2 = &v2;
    // const p3 = &v3;
    // const p4 = &v4;
    // const p5 = &v5;
    // const p6 = &v6;

    // std.debug.print("{}\n", .{p1.*});
    std.debug.print("{} == {}: {}\n", .{ p1, p2, p1 == p2 });
    std.debug.print("{} == {}: {}\n", .{ p1, p3, p1 == p3 });
    // _ = p1;
    // _ = p2;
    // _ = p3;
    // _ = p4;
    // _ = p5;
    // _ = p6;
}

pub fn main() void {
    foo();
}
