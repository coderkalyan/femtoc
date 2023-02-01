const str = "Hello, world!";

export fn main() void {
    const a = 5;
    const b = 1;
    const c = a + b;
    _ = c;
}

export fn func(x: i32, y: i32, z: i32) i32 {
    return x * y + z;
}
