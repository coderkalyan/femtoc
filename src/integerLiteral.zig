const std = @import("std");

pub const ParseError = error { InvalidCharacter, Overflow };

const State = enum(u8) {
    start,
    radix,
    binary,
    octal,
    decimal,
    hex,
};

pub fn parseInt(source: []const u8) !u64 {
    var state: State = .start;
    var value: u64 = 0;
    var i: usize = 0;
    while (i < source.len) : (i += 1) {
        const c = source[i];
        switch (state) {
            .start => {
                switch (c) {
                    '0' => state = .radix,
                    '1'...'9' => {
                        state = .decimal;
                        value =  c - '0';
                    },
                    else => return ParseError.InvalidCharacter,
                }
            },
            .radix => {
                switch (c) {
                    'b' => state = .binary,
                    'o' => state = .octal,
                    'x' => state = .hex,
                    '0'...'9' => {
                        state = .decimal;
                        value =  c - '0';
                    },
                    else => return ParseError.InvalidCharacter,
                }
            },
            .binary => {
                switch (c) {
                    '0', '1' => value = (value << 1) | ((c - '0') & 1),
                    else => break,
                }
            },
            .octal => {
                switch (c) {
                    '0'...'7' => value = (value << 3) | ((c - '0') & 7),
                    else => return ParseError.InvalidCharacter,
                }
            },
            .hex => {
                switch (c) {
                    '0'...'9' => value = (value << 4) | ((c - '0') & 0xF),
                    'a'...'f' => {
                        const digit = (c - 'a' + 10) & 0xF;
                        value = (value << 4) | digit;
                    },
                    'A'...'F' => {
                        const digit = (c - 'A' + 10) & 0xF;
                        value = (value << 4) | digit;
                    },
                    else => return ParseError.InvalidCharacter,
                }
            },
            .decimal => {
                switch (c) {
                    '0'...'9' => value = (value * 10) + (c - '0'),
                    else => return ParseError.InvalidCharacter,
                }
            },
        }
    }

    return value;
}

fn testParseInt(source: []const u8, value: u64) !void {
    try std.testing.expectEqual(value, try parseInt(source));
}

test "literal parsing" {
    try testParseInt("0", 0);
    try testParseInt("1", 1);
    try testParseInt("123", 123);
    try testParseInt("0123", 123);
    try testParseInt("00123", 123);
    try testParseInt("123456789", 123456789);
    try testParseInt("0b0", 0);
    try testParseInt("0b1", 0b1);
    try testParseInt("0b01", 0b1);
    try testParseInt("0b101", 0b101);
    try testParseInt("0o0", 0);
    try testParseInt("0o1", 0o1);
    try testParseInt("0x0", 0);
    try testParseInt("0x1", 0x1);
    try testParseInt("0x123456", 0x123456);
    try testParseInt("0x789A", 0x789A);
    try testParseInt("0x789a", 0x789A);
    try testParseInt("0xABCDEF", 0xABCDEF);
    try testParseInt("0xabcdef", 0xABCDEF);
}
