export fn fibonacci(n: i32) i32 {
    if (n == 0) {
        return 1;
    } else if (n == 1) {
        return 1;
    }

    var a = 1;
    var b = 1;

    var i = 2;
    while (i <= n) : (i += 1) {
        const tmp = a;
        a = b;
        b = tmp + b;
    }
}

pub fn main() void {
    const fib = fibonacci(5);
    _ = fib;
}
