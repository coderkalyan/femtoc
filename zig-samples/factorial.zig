var x: i32 = 99;
var y: i32 = 101;

export fn factorial(n: i32) i32 {
    var z: i32 = 0;
    z = x + y;
    if (n <= 1) {
        return y;
    } else {
        return n * factorial(n - x);
    }
}

// pub fn main() void {
//     const fact = factorial(10);
//     _ = fact;
// }
pub fn panic(msg: []const u8, error_return_trace: ?*@import("std").builtin.StackTrace, foo: ?usize) noreturn {
    _ = msg;
    _ = error_return_trace;
    _ = foo;
    @breakpoint();
    unreachable;
}
