export fn factorial(n: i32) i32 {
    if (n <= 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

// pub fn main() void {
//     const fact = factorial(10);
//     _ = fact;
// }
