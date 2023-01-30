const std = @import("std");

pub const ParseError = error { InvalidCharacter };

const State = enum(u8) {
    start,
    point_left,
    point_right,
    exponent,
};

pub fn parseFloat(source: []const u8) !f64 {
    var state: State = .start;
    var base: u64 = 0;
    var exponent: struct { sign: i8, value: u8 } = .{ .sign = 0, .value = 0 };
    var i: usize = 0;
    var point_len: u8 = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];
        switch (state) {
            .start => {
                switch (c) {
                    '0'...'9' => {
                        state = .point_left;
                        base = c - '0';
                    },
                    '.' => state = .point_right,
                    else => return ParseError.InvalidCharacter,
                }
            },
            .point_left => {
                switch (c) {
                    '0'...'9' => base = (base * 10) + (c - '0'),
                    '.' => state = .point_right,
                    'e' => {
                        state = .exponent;
                        exponent = switch (source[i + 1]) {
                            '+' => exp: {
                                i += 1;
                                break :exp .{ .sign = 1, .value = 0 };
                            },
                            '-' => exp: {
                                i += 1;
                                break :exp .{ .sign = -1, .value = 0 };
                            },
                            '0'...'9' => .{ .sign = 1, .value = 0 },
                            else => return ParseError.InvalidCharacter,
                        };
                    },
                    else => return ParseError.InvalidCharacter,
                }
            },
            .point_right => {
                switch (c) {
                    '0'...'9' => {
                        base = (base * 10) + (c - '0');
                        point_len += 1;
                    },
                    'e' => {
                        state = .exponent;
                        exponent = switch (source[i + 1]) {
                            '+' => exp: {
                                i += 1;
                                break :exp .{ .sign = 1, .value = 0 };
                            },
                            '-' => exp: {
                                i += 1;
                                break :exp .{ .sign = -1, .value = 0 };
                            },
                            '0'...'9' => .{ .sign = 1, .value = 0 },
                            else => return ParseError.InvalidCharacter,
                        };
                    },
                    else => return ParseError.InvalidCharacter,
                }
            },
            .exponent => {
                switch (c) {
                    '0'...'9' => exponent.value = (exponent.value * 10) + (c - '0'),
                    else => return ParseError.InvalidCharacter,
                }
            },
        }
    }

    const power = point_len - (@intCast(i16, exponent.sign) * exponent.value);
    const multiplier = std.math.pow(f64, 10.0, @intToFloat(f64, -power));
    const value = @intToFloat(f64, base) * multiplier;
    return value;
}

fn testParseFloat(source: []const u8, value: f64) !void {
    try std.testing.expect(std.math.approxEqRel(f64, value, try parseFloat(source), std.math.floatEps(f64)));
}

test "literal parsing" {
    // TODO: finish tests and improve state machine
    try testParseFloat("0.", 0.0);
    try testParseFloat("1.0", 1.0);
    try testParseFloat("1.23456789", 1.23456789);
    try testParseFloat("1.5e10", 1.5e10);
    try testParseFloat("123e-10", 123e-10);
}
