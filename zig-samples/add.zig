export fn add(a: u32, b: u64) u64 {
    return a + b;
}

export fn caller() void {
    const a = 5;
    const b = 6;
    return add(a, b);
}
// export fn branch(a: u32, b: u64) u64 {
//     if (a > 5) {
//         return a + b;
//     } else {
//         return a - b;
//     }
// }

pub fn panic(msg: []const u8, error_return_trace: ?*@import("std").builtin.StackTrace, foo: ?usize) noreturn {
    _ = msg;
    _ = error_return_trace;
    _ = foo;
    @breakpoint();
    unreachable;
}
