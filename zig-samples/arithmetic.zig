pub fn main() void {
    const x: i32 = 5;
    const y: i32 = 1;
    const z = x + y;

    const modulo = (z * z) % 20;
    _ = modulo;

    const a: f32 = 10.0;
    const b: f32 = 4.0;
    const c = a / b;
    _ = c;
}
