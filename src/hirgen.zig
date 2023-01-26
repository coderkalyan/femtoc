const std = @import("std");
const hir = @import ("hir.zig");
const ast = @import("ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");

const Hir = hir.Hir;
const Inst = hir.Inst;
const Ast = ast.Ast;
const Node = ast.Node;
const Token = lex.Token;

const NumberLiteral = union(enum) { int: u64, float: f64, };
const NumberState = enum(u8) {
    start,
    base,
    binary,
    octal,
    decimal,
    hex,
    float,
    decimal_exponent,
    float_exponent,
};

pub const HirGen = struct {
    gpa: std.Allocator,
    arena: std.Allocator,

    tree: Ast,
    inst: std.ArrayList(Inst),

    fn parseNumber(hg: *HirGen, index: Token.Index) !NumberLiteral {
        const source = hg.tree.source;
        const start = hg.tree.tokens.items(.start)[@intCast(u32, index)];
        var lexer = lex.Lexer {
            .buffer = source,
            .index = start,
            .pending_invalid_token = null,
        };
        const token = lexer.next();
        return parseNumberLiteral(source[start..token.loc.end]);
    }

    fn addNode(hg: *HirGen, inst: Inst) !Inst.Index {
        const result = @intCast(Node.Index, hg.inst.len);
        try hg.inst.append(inst);
        return result;
    }

    fn integerLiteral(hg: *HirGen, index: Node.Index) !Inst.Index {

    }
};

fn parseNumberLiteral(source: []const u8) !NumberLiteral {
    var state: NumberState = .start;
    var base: NumberLiteral = undefined;
    var exponent: struct { sign: i8, value: i8 } = undefined;
    var dot_index: usize = undefined;
    var i: usize = 0;
    while (i <= source.len) : (i += 1) {
        const c = if (i < source.len) source[i] else 0;
        switch (state) {
            .start => {
                switch (c) {
                    '0' => {
                        state = .base;
                        base = .{ .int = 0 };
                    },
                    '1'...'9' => {
                        state = .decimal;
                        base = .{ .int = c - '0' };
                    },
                    '.' => {
                        dot_index = i;
                        state = .float;
                        base = .{ .int = 0.0 };
                    },
                    else => return parse.Error.ParseError,
                }
            },
            .base => {
                switch (c) {
                    'b' => state = .binary,
                    'o' => state = .octal,
                    'x' => state = .hex,
                    '0'...'9' => {
                        state = .decimal;
                        base = .{ .int = c - '0' };
                    },
                    'e' => {
                        state = .decimal_exponent;
                        i += 1;
                        exponent = switch (source[i]) {
                            '+' => exp: {
                                i += 1;
                                break :exp .{ .sign = 1, .value = 0 };
                            }
                            , '0'...'9' => .{ .sign = 1, .value = 0 },
                            '-' => exp: {
                                i += 1;
                                break :exp .{ .sign = -1, .value = 0 };
                            },
                            else => return parse.Error.ParseError,
                        };
                    },
                    '.' => {
                        dot_index = i;
                        state = .float;
                        base = .{ .int = 0.0 };
                    },
                    'a', 'c', 'd', 'g'...'n', 'p'...'w', 'y', 'z' => return parse.Error.ParserError,
                    'A'...'Z' => return parse.Error.ParseError,
                    else => break,
                }
            },
            .binary => {
                switch (c) {
                    '0', '1' => {
                        base.int = (base.int << 1) | ((c - '0') & 1);
                    },
                    'a'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => break,
                }
            },
            .octal => {
                switch (c) {
                    '0'...'7' => {
                        base.int = (base.int << 3) | ((c - '0') & 7);
                    },
                    'a'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => break,
                }
            },
            .hex => {
                switch (c) {
                    '0'...'9' => base.int = (base.int << 4) | ((c - '0') & 0xF),
                    'a'...'f' => {
                        const digit = (c - 'a' + 10) & 0xF;
                        base.int = (base.int << 4) | digit;
                    },
                    'A'...'F' => {
                        const digit = (c - 'A' + 10) & 0xF;
                        base.int = (base.int << 4) | digit;
                    },
                    'g'...'z', 'G'...'Z' => return parse.Error.ParseError,
                    else => break,
                }
            },
            .decimal => {
                switch (c) {
                    '0'...'9' => {
                        base.int = (base.int * 10) + (c - '0');
                    },
                    'e' => {
                        state = .decimal_exponent;
                        exponent = switch (source[i + 1]) {
                            '+' => exp: {
                                i += 1;
                                break :exp .{ .sign = 1, .value = 0 };
                            }
                            , '0'...'9' => .{ .sign = 1, .value = 0 },
                            '-' => exp: {
                                i += 1;
                                break :exp .{ .sign = -1, .value = 0 };
                            },
                            else => return parse.Error.ParseError,
                        };
                    },
                    'f' => {
                        base = .{ .float = @intToFloat(f64, base.int) };
                        break;
                    },
                    '.' => {
                        state = .float;
                        dot_index = i;
                    },
                    'a'...'d', 'g'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => break,
                }
            },
            .decimal_exponent => {
                switch (c) {
                    '0'...'9' => exponent.value = (exponent.value * 10) + @intCast(i8, (c - '0')),
                    'f' => {
                        const power = std.math.pow(u64, 10, @intCast(u64, exponent.sign * exponent.value));
                        base.int *= power;
                        base = .{ .float = @intToFloat(f64, base.int) };
                        break;
                    },
                    'a'...'e', 'g'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => {
                        if (exponent.sign == 1) {
                            const power = std.math.pow(u64, 10, @intCast(u64, exponent.sign * exponent.value));
                            base.int *= power;
                        } else {
                            const power = std.math.pow(f64, 10, @intToFloat(f64, exponent.sign * exponent.value));
                            base = .{ .float = @intToFloat(f64, base.int) * power };
                        }
                        break;
                    },
                }
            },
            .float => {
                switch (c) {
                    '0'...'9' => {
                        base.int = (base.int * 10) + (c - '0');
                    },
                    'e' => {
                        state = .float_exponent;
                        exponent = switch (source[i + 1]) {
                            '+' => exp: {
                                i += 1;
                                break :exp .{ .sign = 1, .value = 0 };
                            }
                            , '0'...'9' => .{ .sign = 1, .value = 0 },
                            '-' => exp: {
                                i += 1;
                                break :exp .{ .sign = -1, .value = 0 };
                            },
                            else => return parse.Error.ParseError,
                        };
                    },
                    'f' => {
                        base = .{ .float = @intToFloat(f64, base.int) / 
                            @intToFloat(f64, i - dot_index) };
                        break;
                    },
                    'a'...'d', 'g'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => {
                        base = .{ .float = @intToFloat(f64, base.int) / 
                            @intToFloat(f64, i - dot_index) };
                        break;
                    },
                }
            },
            .float_exponent => {
                switch (c) {
                    '0'...'9' => exponent.value = (exponent.value * 10) | @intCast(i8, (c - '0')),
                    'f' => {
                        const power = std.math.pow(u64, 10, @intCast(u64, exponent.sign * exponent.value));
                        base.float *= @intToFloat(f64, power);
                        break;
                    },
                    'a'...'e', 'g'...'z', 'A'...'Z' => return parse.Error.ParseError,
                    else => {
                        const power = std.math.pow(u64, 10, @intCast(u64, exponent.sign * exponent.value));
                        base.float *= @intToFloat(f64, power);
                        break;
                    },
                }
            },
        }
    }

    return base;
}

fn testParseInt(source: []const u8, value: u64) !void {
    try std.testing.expectEqual(parseNumberLiteral(source), .{ .int = value });
}

fn testParseFloat(source: []const u8, value: f64) !void {
    try std.testing.expectEqual(parseNumberLiteral(source), .{ .float = value });
}

test "literal parsing" {
    try testParseInt("0", 0);
    try testParseInt("1", 1);
    try testParseInt("123", 123);
    try testParseInt("0123", 123);
    try testParseInt("00123", 123);
    try testParseInt("123456789", 123456789);
    try testParseInt("0e1", 0);
    try testParseInt("1e1", 10);
    try testParseInt("1e4", 10000);
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

    // TODO: finish tests and improve state machine
    try testParseFloat("0f", 0.0);
    try testParseFloat("1.0", 1.0);
    try testParseFloat("1.0f", 1.0);
}
