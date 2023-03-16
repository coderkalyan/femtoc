const std = @import("std");

// sign extends the value `val` to 64 bits based
// on the value in the `bit` place
// pub fn sext(val: u64, bit: u8) u64 {

// }

pub const Error = error {
    Overflow,
};

const u64_max: u64 = std.math.maxInt(u64);
const i64_max: u64 = int_neg(std.math.maxInt(i64));
const u64_min: u64 = std.math.minInt(u64);
const i64_min: u64 = int_neg(std.math.minInt(i64));

inline fn BIT(val: u64) u8 {
    return @boolToInt(val > 0);
}

inline fn ALL(val: u8) u64 {
    if (val == 0) return 0;
    return 0xffffffffffffffff;
}

pub fn negate(a: u64) u64 {
    var negative: u64 = 0;
    _ = @addWithOverflow(u64, ~a, 1, &negative);
    return negative;
}

pub inline fn isNegative(a: u64) bool {
    return (a & (1 << 63)) > 0;
}
pub fn demoteUnsigned(a: u64) !u64 {
    if (isNegative(a)) return error.Overflow;
    return a;
}

pub fn promoteSigned(a: u64) !u64 {
    if (isNegative(a)) return error.Overflow;
    return a;
}

pub fn uadd(a: u64, b: u64) !u64 {
    comptime var i = 0;
    var sum: u64 = 0;
    var carry: u8 = 0;
    inline while (i < 64) : (i += 1) {
        const bit = comptime (1 << i);
        const bita = BIT(a & bit);
        const bitb = BIT(b & bit);
        if ((bita ^ bitb ^ carry) > 0) sum |= bit;
        carry = BIT((bita & bitb) | (bita & carry) | (bitb & carry));
    }

    if (carry > 0) return error.Overflow;
    return sum;
}

pub fn sadd(a: u64, b: u64) !u64 {
    comptime var i = 0;
    var sum: u64 = 0;
    var carry: u8 = 0;
    inline while (i < 64) : (i += 1) {
        const bit = comptime (1 << i);
        const bita = BIT(a & bit);
        const bitb = BIT(b & bit);
        if ((bita ^ bitb ^ carry) > 0) sum |= bit;
        carry = BIT((bita & bitb) | (bita & carry) | (bitb & carry));
    }

    const msba = a & (1 << 63);
    const msbb = b & (1 << 63);
    const msbs = sum & (1 << 63);
    if ((msba == msbb) and (msba != msbs)) return error.Overflow;
    return sum;
}

pub fn radd(a: u64, b: u64) u64 {
    comptime var i = 0;
    var sum: u64 = 0;
    var carry: u8 = 0;
    inline while (i < 64) : (i += 1) {
        const bit = comptime (1 << i);
        const bita = BIT(a & bit);
        const bitb = BIT(b & bit);
        if ((bita ^ bitb ^ carry) > 0) sum |= bit;
        carry = BIT((bita & bitb) | (bita & carry) | (bitb & carry));
    }

    return sum;
}

pub fn umul(a: u64, b: u64) !u64 {
    var product: u64 = 0;
    comptime var i = 0;
    inline while (i < 64) : (i += 1) {
        const bit = comptime (1 << i);
        const partial = try asl(ALL(BIT(a & bit)) & b, i);
        product = try uadd(product, partial);
    }

    return product;
}

pub fn smul(a: u64, b: u64) !u64 {
    var product: u64 = 0;
    comptime var i = 0;
    inline while (i < 64) : (i += 1) {
        const bit = comptime (1 << i);
        const partial = lsl(ALL(BIT(a & bit)) & b, i);
        product = radd(product, partial);
    }

    const msba = a & (1 << 63);
    const msbb = b & (1 << 63);
    const msbp = product & (1 << 63);
    if ((msba == msbb) and (msbp > 0)) return error.Overflow;
    if ((a != 0) and (b != 0) and (msba != msbb) and (msbp == 0)) return error.Overflow;
    return product;
}

pub fn udiv(a: u64, b: u64) !u64 {
    if (b == 0) return error.DivZero;
    var quotient: u64 = 0;
    var dividend = a;
    var divisor = b;
    var lsb: i32 = 0;
    while (true) : (lsb += 1) {
        divisor = asl(divisor, 1) catch break;
    }

    while (lsb >= 0) : (lsb -= 1) {
        if (dividend >= divisor) {
            dividend = radd(dividend, negate(divisor));
            quotient |= @intCast(u64, 1) << @intCast(u6, lsb);
        }
        divisor = try lsr(divisor, 1);
    }

    return quotient;
}

pub fn sdiv(a: u64, b: u64) !u64 {
    var sign: u8 = 0;
    const x = if (isNegative(a)) val: {
        sign ^= 1;
        break :val try smul(a, comptime int_neg(-1));
    } else a;
    const y = if (isNegative(b)) val: {
        sign ^= 1;
        break :val try smul(b, comptime int_neg(-1));
    } else b;

    const result = try udiv(x, y);
    if (sign > 0) return try smul(result, comptime int_neg(-1));
    return result;
}

pub fn lsl(a: u64, b: u64) u64 {
    var count: u64 = 0;
    var result = a;
    while (count < b) : (count += 1) {
        _ = @shlWithOverflow(u64, result, 1, &result);
    }

    return result;
}

pub inline fn asl(a: u64, b: u64) !u64 {
    var count: u64 = 0;
    var result = a;
    while (count < b) : (count += 1) {
        if ((result & (1 << 63)) > 0) return error.Overflow;
        result <<= 1;
    }

    return result;
}

pub fn lsr(a: u64, b: u64) !u64 {
    if (b > std.math.maxInt(u6)) return error.Overflow;
    if (b < 1) return a;
    var i: u64 = 1;
    const msb_mask: u64 = (1 << 63);
    var result = (a >> 1) & ~msb_mask;
    while (i < b) : (i += 1) {
        result >>= 1;
    }

    return result;
}

pub fn asr(a: u64, b: u64) !u64 {
    if (b > std.math.maxInt(u6)) return error.Overflow;
    return @bitCast(u64, @bitCast(i64, a) >> @intCast(u6, b));
}

inline fn int_neg(val: i64) u64 {
    return @bitCast(u64, val);
}

test "negate" {
    try std.testing.expectEqual(negate(1), int_neg(-1));
    try std.testing.expectEqual(negate(int_neg(-1)), 1);
    try std.testing.expectEqual(negate(5), int_neg(-5));
    try std.testing.expectEqual(negate(int_neg(-5)), 5);
    try std.testing.expectEqual(negate(0), 0);
}

test "promotion" {
    try std.testing.expectEqual(demoteUnsigned(0), 0);
    try std.testing.expectEqual(demoteUnsigned(1), 1);
    try std.testing.expectEqual(demoteUnsigned(10), 10);
    try std.testing.expectEqual(demoteUnsigned(i64_max), i64_max);
    try std.testing.expectError(error.Overflow, demoteUnsigned(u64_max));
    try std.testing.expectError(error.Overflow, demoteUnsigned(int_neg(-1)));
    try std.testing.expectError(error.Overflow, demoteUnsigned(int_neg(-10)));

    try std.testing.expectEqual(promoteSigned(0), 0);
    try std.testing.expectEqual(promoteSigned(1), 1);
    try std.testing.expectEqual(promoteSigned(10), 10);
    try std.testing.expectEqual(promoteSigned(i64_max), i64_max);
    try std.testing.expectError(error.Overflow, promoteSigned(u64_max));
    try std.testing.expectError(error.Overflow, promoteSigned(int_neg(-1)));
    try std.testing.expectError(error.Overflow, promoteSigned(int_neg(-10)));
}

test "add" {
    try std.testing.expectEqual(uadd(2, 3), 5);
    try std.testing.expectEqual(uadd(0, 0), 0);
    try std.testing.expectEqual(uadd(0, 1), 1);
    try std.testing.expectEqual(uadd(0b111, 0b111), 0b111 + 0b111);
    try std.testing.expectEqual(uadd(0b0001101010, 0b00001101100), 214);
    try std.testing.expectEqual(uadd(0b1111001000, 0b11110010000), 2904);
    try std.testing.expectEqual(uadd(u64_max - 1, 1), u64_max);
    try std.testing.expectError(error.Overflow, uadd(u64_max, 1));

    try std.testing.expectEqual(sadd(int_neg(-2), 5), 3);
    try std.testing.expectEqual(sadd(int_neg(-2), int_neg(-3)), int_neg(-5));
    try std.testing.expectEqual(sadd(int_neg(-1), 1), 0);
    try std.testing.expectError(error.Overflow, sadd(i64_max, 1));
}

test "multiply" {
    try std.testing.expectEqual(umul(2, 3), 6);
    try std.testing.expectEqual(umul(1, 1), 1);
    try std.testing.expectEqual(umul(0, 5), 0);
    try std.testing.expectEqual(umul(5, 0), 0);
    try std.testing.expectEqual(umul(123, 456), 123 * 456);
    try std.testing.expectError(error.Overflow, umul(u64_max, 2));

    try std.testing.expectEqual(smul(2, 3), 6);
    try std.testing.expectEqual(smul(1, 1), 1);
    try std.testing.expectEqual(smul(0, 5), 0);
    try std.testing.expectEqual(smul(5, 0), 0);
    try std.testing.expectEqual(smul(int_neg(-5), 0), 0);
    try std.testing.expectEqual(smul(int_neg(-2), int_neg(-3)), 6);
    try std.testing.expectEqual(smul(int_neg(-2), 3), int_neg(-6));
    try std.testing.expectEqual(smul(2, int_neg(-3)), int_neg(-6));
    try std.testing.expectEqual(smul(int_neg(-6), 7), int_neg(-42));
    try std.testing.expectEqual(smul(7, int_neg(-6)), int_neg(-42));
    try std.testing.expectError(error.Overflow, smul(i64_min, int_neg(-1)));
}

test "divide" {
    try std.testing.expectEqual(udiv(10, 5), 2);
    try std.testing.expectEqual(udiv(10, 2), 5);
    try std.testing.expectEqual(udiv(64, 4), 16);
    try std.testing.expectEqual(udiv(5, 10), 0);
    try std.testing.expectEqual(udiv(0, 10), 0);
    try std.testing.expectEqual(udiv(0, 1), 0);
    try std.testing.expectEqual(udiv(0b10101, 0b11), 0b111);
    try std.testing.expectEqual(udiv(u64_max, 1), u64_max);
    try std.testing.expectEqual(udiv(u64_max, 2), u64_max / 2);
    try std.testing.expectEqual(udiv(0, u64_max), 0);
    try std.testing.expectError(error.DivZero, udiv(1, 0));
    try std.testing.expectError(error.DivZero, udiv(10, 0));
    try std.testing.expectError(error.DivZero, udiv(64, 0));

    try std.testing.expectEqual(sdiv(10, 5), 2);
    try std.testing.expectEqual(sdiv(10, 2), 5);
    try std.testing.expectEqual(sdiv(64, 4), 16);
    try std.testing.expectEqual(sdiv(5, 10), 0);
    try std.testing.expectEqual(sdiv(0, 10), 0);
    try std.testing.expectEqual(sdiv(0, 1), 0);
    try std.testing.expectEqual(sdiv(0b10101, 0b11), 0b111);
    try std.testing.expectEqual(sdiv(u64_max, 1), u64_max);
    try std.testing.expectEqual(sdiv(int_neg(-10), 5), int_neg(-2));
    try std.testing.expectEqual(sdiv(10, int_neg(-2)), int_neg(-5));
    try std.testing.expectEqual(sdiv(64, int_neg(-4)), int_neg(-16));
    try std.testing.expectEqual(sdiv(int_neg(-5), 10), 0);
    try std.testing.expectEqual(sdiv(5, int_neg(-10)), 0);
    try std.testing.expectEqual(sdiv(0, int_neg(-10)), 0);
    try std.testing.expectEqual(sdiv(0, int_neg(-1)), 0);
    try std.testing.expectError(error.DivZero, sdiv(1, 0));
    try std.testing.expectError(error.DivZero, sdiv(10, 0));
    try std.testing.expectError(error.DivZero, sdiv(64, 0));
    try std.testing.expectError(error.Overflow, sdiv(i64_min, int_neg(-1)));
    try std.testing.expectError(error.Overflow, sdiv(int_neg(-1), i64_min));
}

test "shift" {
    try std.testing.expectEqual(lsl(1, 8), 1 << 8);
    try std.testing.expectEqual(lsl(1, 0), 1 << 0);
    try std.testing.expectEqual(lsl(0b111, 8), 0b111 << 8);

    try std.testing.expectEqual(asl(1, 8), 1 << 8);
    try std.testing.expectEqual(asl(1, 0), 1 << 0);
    try std.testing.expectEqual(asl(0b111, 8), 0b111 << 8);
    try std.testing.expectError(error.Overflow, asl(u64_max, 1));

    try std.testing.expectEqual(lsr(1, 8), 1 >> 8);
    try std.testing.expectEqual(lsr(1, 0), 1 >> 0);
    try std.testing.expectEqual(lsr(0b111, 1), 0b11);
    try std.testing.expectEqual(lsr(u64_max, 2), u64_max >> 2);
    try std.testing.expectEqual(lsr(int_neg(-2), 1), int_neg(-2) >> 1);
    try std.testing.expectError(error.Overflow, lsr(500, 65));

    try std.testing.expectEqual(asr(1, 8), 1 >> 8);
    try std.testing.expectEqual(asr(1, 0), 1 >> 0);
    try std.testing.expectEqual(asr(0b111, 1), 0b11);
    try std.testing.expectEqual(asr(u64_max, 2), @bitCast(u64, @bitCast(i64, u64_max) >> 2));
    try std.testing.expectEqual(asr(int_neg(-2), 1), @bitCast(u64, @intCast(i64, -2) >> 1));
    try std.testing.expectError(error.Overflow, asr(500, 65));
}
