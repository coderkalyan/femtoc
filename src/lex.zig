const std = @import("std");

pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: usize,
        end: usize,
    };

    pub const Tag = enum {
        // lexer flow control
        invalid,
        eof,

        ident,
        str_lit,
        char_lit,
        int_lit,
        float_lit,

        // single character punctuation
        /// generic
        semi,
        colon,
        equal,
        period,
        comma,
        at,
        underscore,
        /// grouping
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,
        s_quote,
        d_quote,
        /// arithmetic
        plus,
        minus,
        asterisk,
        slash,
        percent,
        /// binary
        ampersand,
        pipe,
        caret,
        tilde,
        /// logical
        bang,
        /// comparison
        l_angle,
        r_angle,

        // double character punctuation
        /// arithmetic
        plus_equal,
        minus_equal,
        asterisk_equal,
        slash_equal,
        percent_equal,
        /// binary
        ampersand_equal,
        pipe_equal,
        caret_equal,
        l_angle_l_angle,
        r_angle_r_angle,
        /// logical
        ampersand_ampersand,
        pipe_pipe,
        caret_caret,
        /// comparison
        equal_equal,
        l_angle_equal,
        r_angle_equal,
        bang_equal,
        /// scope
        colon_colon,

        // triple character punctuation
        /// binary
        l_angle_l_angle_equal,
        r_angle_r_angle_equal,
        /// logical
        ampersand_ampersand_equal,
        pipe_pipe_equal,
        caret_caret_equal,

        // keywords
        k_use,
        k_fn,
        k_assoc,
        k_return,
        k_let,
        k_mut,
        k_type,
        k_if,
        k_else,
        k_yield,
        k_struct,
        k_enum,
        k_variant,
        k_defer,

        // annotations
        a_unknown,
        a_inline,
        a_import,
        a_export,
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "use", .k_use },
        .{ "fn", .k_fn },
        .{ "assoc", .k_assoc },
        .{ "return", .k_return },
        .{ "let", .k_let },
        .{ "mut", .k_mut },
        .{ "type", .k_type },
        .{ "if", .k_if },
        .{ "else", .k_else },
        .{ "yield", .k_yield },
        .{ "struct", .k_struct },
        .{ "enum", .k_enum },
        .{ "variant", .k_variant },
        .{ "defer", .k_defer },
    });

    pub const annotations = std.ComptimeStringMap(Tag, .{
        .{ "inline", .a_inline },
        .{ "import", .a_import },
        .{ "export", .a_export },
    });

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub fn getAnnotation(bytes: []const u8) ?Tag {
        return annotations.get(bytes);
    }
};

pub const Lexer = struct {
    buffer: [:0]const u8,
    index: usize,
    pending_invalid_token: ?Token,

    const State = enum {
        start,

        // named things
        ident,
        annot,

        // literals
        str_lit,
        char_lit,
        base,
        int_lit,
        float_lit,

        // partial operators
        period,
        equal,
        plus,
        minus,
        asterisk,
        slash,
        percent,
        ampersand,
        pipe,
        caret,
        bang,
        colon,
        l_angle,
        r_angle,
        l_angle_l_angle,
        r_angle_r_angle,
        ampersand_ampersand,
        pipe_pipe,
        caret_caret,

        line_comment,
    };

    pub fn init(buffer: [:0]const u8) Lexer {
        return Lexer {
            .buffer = buffer,
            .index = 0,
            .pending_invalid_token = null,
        };
    }

    pub fn next(self: *Lexer) Token {
        if (self.pending_invalid_token) |token| {
            self.pending_invalid_token = null;
            return token;
        }

        var state: State = .start;
        var result = Token {
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        while (true) : (self.index += 1) {
            const c = self.buffer[self.index];
            switch (state) {
                .start => switch (c) {
                    // eof
                    0 => {
                        if (self.index != self.buffer.len) {
                            result.tag = .invalid;
                            result.loc.start = self.index;
                            self.index += 1;
                            result.loc.end = self.index;
                            return result;
                        }
                        break;
                    },

                    // whitespace
                    ' ', '\n', '\r', '\t' => {
                        result.loc.start = self.index + 1;
                    },

                    // identifier
                    'a'...'z', 'A'...'Z', '_' => {
                        state = .ident;
                        result.tag = .ident;
                    },

                    // annotation
                    '@' => {
                        state = .annot;
                    },

                    // string literal
                    '"' => {
                        state = .str_lit;
                        result.tag = .str_lit;
                    },
                    // char literal
                    '\'' => {
                        state = .char_lit;
                        result.tag = .char_lit;
                    },
                    // number literal
                    '0' => {
                        state = .base;
                    },
                    '1'...'9' => {
                        state = .int_lit;
                    },

                    // punctuation
                    ';' => {
                        result.tag = .semi;
                        self.index += 1;
                        break;
                    },
                    ':' => {
                        state = .colon;
                    },
                    '=' => {
                        state = .equal;
                    },
                    '.' => {
                        state = .period;
                    },
                    ',' => {
                        result.tag = .comma;
                        self.index += 1;
                        break;
                    },
                    '(' => {
                        result.tag = .l_paren;
                        self.index += 1;
                        break;
                    },
                    ')' => {
                        result.tag = .r_paren;
                        self.index += 1;
                        break;
                    },
                    '[' => {
                        result.tag = .l_bracket;
                        self.index += 1;
                        break;
                    },
                    ']' => {
                        result.tag = .r_bracket;
                        self.index += 1;
                        break;
                    },
                    '{' => {
                        result.tag = .l_brace;
                        self.index += 1;
                        break;
                    },
                    '}' => {
                        result.tag = .r_brace;
                        self.index += 1;
                        break;
                    },
                    '+' => {
                        state = .plus;
                    },
                    '-' => {
                        state = .minus;
                    },
                    '*' => {
                        state = .asterisk;
                    },
                    '/' => {
                        state = .slash;
                    },
                    '%' => {
                        state = .percent;
                    },
                    '&' => {
                        state = .ampersand;
                    },
                    '|' => {
                        state = .pipe;
                    },
                    '^' => {
                        state = .caret;
                    },
                    '~' => {
                        result.tag = .tilde;
                        self.index += 1;
                        break;
                    },
                    '!' => {
                        state = .bang;
                    },
                    '<' => {
                        state = .l_angle;
                    },
                    '>' => {
                        state = .r_angle;
                    },
                    else => {
                        result.tag = .invalid;
                        self.index += 1;
                        break;
                    },
                },
                .ident => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        // done with identifier, check if keyword
                        if (Token.getKeyword(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },
                .annot => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        // done with identifier, check if keyword
                        if (Token.getAnnotation(self.buffer[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },
                .str_lit => switch (c) {
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    else => {},
                },
                .char_lit => switch (c) {
                    '\'' => {
                        self.index += 1;
                        break;
                    },
                    else => {},
                },
                .base => switch (c) {
                    'b', 'o', 'x', '0'...'9' => {
                        state = .int_lit;
                    },
                    '.' => {
                        state = .float_lit;
                    },
                    'a', 'c'...'n', 'p'...'w', 'y'...'z', 'A'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .int_lit => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_', 'u', 'i' => {},
                    '.' => {
                        state = .float_lit;
                    },
                    'g'...'h', 'j'...'t', 'v'...'z', 'G'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .int_lit;
                        break;
                    },
                },
                .float_lit => switch (c) {
                    '0'...'9', 'e', '_', 'f', '+', '-' => {},
                    'g'...'z', 'G'...'Z' => {
                        self.index += 1;
                        const invalid_length = self.eatInvalidLiteral();
                        result.tag = .invalid;
                        self.index += invalid_length;
                        break;
                    },
                    else => {
                        result.tag = .float_lit;
                        break;
                    },
                },
                .period => switch (c) {
                    '0'...'9' => {
                        state = .float_lit;
                    },
                    else => {
                        result.tag = .period;
                        break;
                    },
                },
                .equal => switch (c) {
                    '=' => {
                        result.tag = .equal_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .equal;
                        break;
                    },
                },
                .plus => switch (c) {
                    '=' => {
                        result.tag = .plus_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .plus;
                        break;
                    },
                },
                .minus => switch (c) {
                    '=' => {
                        result.tag = .minus_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .minus;
                        break;
                    },
                },
                .asterisk => switch (c) {
                    '=' => {
                        result.tag = .asterisk_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .asterisk;
                        break;
                    },
                },
                .slash => switch (c) {
                    '=' => {
                        result.tag = .slash_equal;
                        self.index += 1;
                        break;
                    },
                    '/' => {
                        state = .line_comment;
                    },
                    else => {
                        result.tag = .slash;
                        break;
                    },
                },
                .percent => switch (c) {
                    '=' => {
                        result.tag = .percent_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .percent;
                        break;
                    },
                },
                .ampersand => switch (c) {
                    '=' => {
                        result.tag = .ampersand_equal;
                        self.index += 1;
                        break;
                    },
                    '&' => {
                        state = .ampersand_ampersand;
                    },
                    else => {
                        result.tag = .ampersand;
                        break;
                    },
                },
                .pipe => switch (c) {
                    '=' => {
                        result.tag = .pipe_equal;
                        self.index += 1;
                        break;
                    },
                    '|' => {
                        state = .pipe_pipe;
                    },
                    else => {
                        result.tag = .pipe;
                        break;
                    },
                },
                .caret => switch (c) {
                    '=' => {
                        result.tag = .caret_equal;
                        self.index += 1;
                        break;
                    },
                    '^' => {
                        state = .caret_caret;
                    },
                    else => {
                        result.tag = .caret;
                        break;
                    },
                },
                .bang => switch (c) {
                    '=' => {
                        result.tag = .bang_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .bang;
                        break;
                    },
                },
                .colon => switch (c) {
                    ':' => {
                        result.tag = .colon_colon;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .colon;
                        break;
                    },
                },
                .l_angle => switch (c) {
                    '=' => {
                        result.tag = .l_angle_equal;
                        self.index += 1;
                        break;
                    },
                    '<' => {
                        state = .l_angle_l_angle;
                    },
                    else => {
                        result.tag = .l_angle;
                        break;
                    },
                },
                .r_angle => switch (c) {
                    '=' => {
                        result.tag = .r_angle_equal;
                        self.index += 1;
                        break;
                    },
                    '>' => {
                        state = .r_angle_r_angle;
                    },
                    else => {
                        result.tag = .r_angle;
                        break;
                    },
                },
                .l_angle_l_angle => switch (c) {
                    '=' => {
                        result.tag = .l_angle_l_angle_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .l_angle_l_angle;
                        break;
                    },
                },
                .r_angle_r_angle => switch (c) {
                    '=' => {
                        result.tag = .r_angle_r_angle_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .r_angle_r_angle;
                        break;
                    },
                },
                .ampersand_ampersand => switch (c) {
                    '=' => {
                        result.tag = .ampersand_ampersand_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .ampersand_ampersand;
                        break;
                    },
                },
                .pipe_pipe => switch (c) {
                    '=' => {
                        result.tag = .pipe_pipe_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .pipe_pipe;
                        break;
                    },
                },
                .caret_caret => switch (c) {
                    '=' => {
                        result.tag = .caret_caret_equal;
                        self.index += 1;
                        break;
                    },
                    else => {
                        result.tag = .caret_caret;
                        break;
                    },
                },
                .line_comment => switch (c) {
                    '\n' => {
                        result.loc.start = self.index + 1;
                        state = .start;
                    },
                    else => {},
                },
            }
        }

        if (result.tag == .eof) {
            if (self.pending_invalid_token) |token| {
                self.pending_invalid_token = null;
                return token;
            }
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        return result;
    }

    fn eatInvalidLiteral(self: *Lexer) u32 {
        var length: u32 = 0;

        while (self.index + length < self.buffer.len) : (length += 1) {
            switch (self.buffer[self.index + length]) {
                'a'...'z', 'A'...'Z' => {},
                else => { return length; },
            }
        }

        return length;
    }
};

fn testLex(source: [:0]const u8, expected_token_tags: []const Token.Tag) !void {
    var lexer = Lexer.init(source);
    for (expected_token_tags) |expected_token_tag| {
        const token = lexer.next();
        try std.testing.expectEqual(expected_token_tag, token.tag);
    }
    const eof = lexer.next();
    try std.testing.expectEqual(Token.Tag.eof, eof.tag);
    try std.testing.expectEqual(source.len, eof.loc.start);
    try std.testing.expectEqual(source.len, eof.loc.end);
}

test "literal" {
    // the lexer has very weak numerical literal checks, so invalid literals will get through
    // we simply check that all valid ones do get through

    // decimal
    try testLex("0", &.{.int_lit});
    try testLex("123", &.{.int_lit});
    try testLex("123(", &.{.int_lit, .l_paren});
    try testLex("123;", &.{.int_lit, .semi});
    try testLex("123hjk", &.{.invalid});
    try testLex("123u", &.{.int_lit});
    try testLex("123u8", &.{.int_lit});
    try testLex("123u32", &.{.int_lit});
    try testLex("123i", &.{.int_lit});
    try testLex("123i8", &.{.int_lit});
    try testLex("123i32", &.{.int_lit});
    try testLex("123_456", &.{.int_lit});
    try testLex("123_456_789i64", &.{.int_lit});

    // binary
    try testLex("0b0", &.{.int_lit});
    try testLex("0b1", &.{.int_lit});
    try testLex("0b0101011", &.{.int_lit});
    try testLex("0b0101011i", &.{.int_lit});
    try testLex("0b0101011i8", &.{.int_lit});
    try testLex("0b0101011i32", &.{.int_lit});
    try testLex("0b0101011u", &.{.int_lit});
    try testLex("0b0101011u8", &.{.int_lit});
    try testLex("0b0101011u32", &.{.int_lit});

    // octal
    try testLex("0o0", &.{.int_lit});
    try testLex("0o1", &.{.int_lit});
    try testLex("0o0123456", &.{.int_lit});
    try testLex("0o0123456i", &.{.int_lit});
    try testLex("0o0123456i8", &.{.int_lit});
    try testLex("0o0123456i32", &.{.int_lit});
    try testLex("0o0123456u", &.{.int_lit});
    try testLex("0o0123456u8", &.{.int_lit});
    try testLex("0o0123456u32", &.{.int_lit});

    // hex
    try testLex("0x0", &.{.int_lit});
    try testLex("0x1", &.{.int_lit});
    try testLex("0x018ADFF", &.{.int_lit});
    try testLex("0x018ADFFi", &.{.int_lit});
    try testLex("0x018ADFFi8", &.{.int_lit});
    try testLex("0x018ADFFi32", &.{.int_lit});
    try testLex("0x018ADFFu", &.{.int_lit});
    try testLex("0x018ADFFu8", &.{.int_lit});
    try testLex("0x018ADFFu32", &.{.int_lit});

    // float
    try testLex(".", &.{.period});
    try testLex("0.5f", &.{.float_lit});
    try testLex("1.5f", &.{.float_lit});
    try testLex(".5f", &.{.float_lit});
    try testLex(".51231232f", &.{.float_lit});
    try testLex(".51231232e05f", &.{.float_lit});
    try testLex(".51231232e+15f", &.{.float_lit});
    try testLex(".51231232e-15f", &.{.float_lit});
}
