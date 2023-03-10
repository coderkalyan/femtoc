const std = @import("std");

// each token contains a type (tag), start, and end index in the source string
pub const Token = struct {
    tag: Tag,
    loc: Loc,

    pub const Loc = struct {
        start: u32,
        end: u32,
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
        underscore,
        /// grouping
        l_paren,
        r_paren,
        l_bracket,
        r_bracket,
        l_brace,
        r_brace,
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
        k_as,
        k_fn,
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
        k_for,
        k_break,
        k_or,
        k_and,
        k_not,
        k_true,
        k_false,

        // annotations
        a_unknown,
        a_inline,
        a_import,
        a_export,
    };

    pub const keywords = std.ComptimeStringMap(Tag, .{
        .{ "use", .k_use },
        .{ "as", .k_as },
        .{ "fn", .k_fn },
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
        .{ "for", .k_for },
        .{ "break", .k_break },
        .{ "or", .k_or },
        .{ "and", .k_and },
        .{ "not", .k_not },
        .{ "true", .k_true },
        .{ "false", .k_false },
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
    // source code being analyzed
    source: [:0]const u8,
    // current lexer index in source
    index: u32,

    const State = enum {
        start,

        // named things
        ident,
        annot,
        underscore,

        // literals
        str_lit,
        int_base,
        binary,
        octal,
        decimal,
        hex,
        float_base,
        float_exp,

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

    pub fn init(source: [:0]const u8) Lexer {
        return Lexer.init_index(source, 0);
    }

    pub fn init_index(source: [:0]const u8, index: u32) Lexer {
        // we only parse <= ~4GiB files (u32_max characters)
        std.debug.assert(source.len <= std.math.maxInt(u32)); // TODO: nice error

        return Lexer {
            .source = source,
            .index = index,
        };
    }

    pub fn next(self: *Lexer) Token {
        var state: State = .start;
        var result = Token {
            .tag = .eof,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };

        // finite state machine that parses one character at a time
        // the state starts with .start, where we parse (almost) any
        // character and decide what to do with it
        // 0 = null terminator
        // single character tokens are returned immediately, while
        // multi character tokens (multi character operators, keywords,
        // and identifiers) require intermediate states

        // if the current character ends a token (inclusive), we increment
        // the index to set the correct end location, and break
        // current characters signaling the end of a previous token (exclusive)
        // break but don't increment the index
        // else, the while loop predicate will increment automatically
        while (true) : (self.index += 1) {
            // switch on the state, and then the current character c
            const c = self.source[self.index];
            switch (state) {
                .start => switch (c) {
                    // eof
                    0 => {
                        if (self.index != self.source.len) {
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
                    'a'...'z', 'A'...'Z' => {
                        state = .ident;
                        result.tag = .ident;
                    },
                    '_' => {
                        state = .underscore;
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
                        self.index += 1;
                        if (self.source[self.index] == '\'') {
                            self.index += 1;
                            result.tag = .invalid;
                            break;
                        }
                        if (self.source[self.index] != 0) self.index += 1;
                        if (self.source[self.index] != '\'') {
                            result.tag = .invalid;
                            while (true) : (self.index += 1) {
                                switch (self.source[self.index]) {
                                    '\'', 0 => break,
                                    else => {},
                                }
                            }
                        } else {
                            result.tag = .char_lit;
                        }

                        // eat closing quote
                        if (self.source[self.index] == '\'') self.index += 1;
                        break;
                    },
                    // number literal
                    '0' => {
                        state = .int_base;
                    },
                    '1'...'9' => {
                        state = .decimal;
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
                        if (Token.getKeyword(self.source[result.loc.start..self.index])) |tag| {
                            result.tag = tag;
                        }
                        break;
                    },
                },
                .annot => switch (c) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                    else => {
                        // done with annotation, check if keyword
                        result.tag = Token.getAnnotation(self.source[result.loc.start + 1..self.index]) orelse .a_unknown;
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
                .int_base => switch (c) {
                    'b' => state = .binary,
                    'o' => state = .octal,
                    'x' => state = .hex,
                    '0'...'9' => state = .decimal,
                    '.' => state = .float_base,
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
                .binary => switch (c) {
                    '0'...'1', '_' => {},
                    '2'...'9', 'a'...'z', 'A'...'Z' => {
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
                .octal => switch (c) {
                    '0'...'7', '_' => {},
                    '8'...'9', 'a'...'z', 'A'...'Z' => {
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
                .decimal => switch (c) {
                    '0'...'9', '_' => {},
                    '.', 'e' => state = .float_base,
                    'a'...'d', 'f'...'z', 'A'...'Z' => {
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
                .hex => switch (c) {
                    '0'...'9', 'a'...'f', 'A'...'F', '_' => {},
                    'g'...'z', 'G'...'Z' => {
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
                .float_base => switch (c) {
                    '0'...'9', '_' => {},
                    'e' => {
                        state = .float_exp;
                        switch (self.source[self.index + 1]) {
                            '+', '-' => self.index += 1,
                            else => {},
                        }
                    },
                    'a'...'d', 'f'...'z', 'A'...'Z' => {
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
                .float_exp => switch (c) {
                    '0'...'9', '_' => {},
                    'a'...'z', 'A'...'Z' => {
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
                .underscore => switch (c) {
                    'a'...'z', 'A'...'Z' => {
                        result.tag = .ident;
                        state = .ident;
                    },
                    else => {
                        result.tag = .underscore;
                        break;
                    },
                },
                .period => switch (c) {
                    '0'...'9' => {
                        state = .float_base;
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
                    0 => {
                        state = .start;
                        break;
                    },
                    else => {},
                },
            }
        }

        if (result.tag == .eof) {
            result.loc.start = self.index;
        }

        result.loc.end = self.index;
        return result;
    }

    fn eatInvalidLiteral(self: *Lexer) u32 {
        var length: u32 = 0;

        while (self.index + length < self.source.len) : (length += 1) {
            switch (self.source[self.index + length]) {
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
    try std.testing.expectEqual(@intCast(u32, source.len), eof.loc.start);
    try std.testing.expectEqual(@intCast(u32, source.len), eof.loc.end);
}

test "identifier" {
    try testLex("x", &.{.ident});
    try testLex("abc", &.{.ident});
    try testLex("a123", &.{.ident});
    try testLex("123a", &.{.invalid});
    try testLex("1abc", &.{.invalid});
}

test "string literal" {
    try testLex("\"abc\"", &.{.str_lit});
    try testLex("\"\"", &.{.str_lit});
    try testLex("\"123456\"", &.{.str_lit});
    try testLex("\"let\"", &.{.str_lit});
}

test "character literal" {
    try testLex("'a'", &.{.char_lit});
    try testLex("'0'", &.{.char_lit});
    try testLex("'abc'", &.{.invalid});
    try testLex("''", &.{.invalid});
}

test "integer literal" {
    // decimal
    try testLex("0", &.{.int_lit});
    try testLex("123", &.{.int_lit});
    try testLex("123(", &.{.int_lit, .l_paren});
    try testLex("123;", &.{.int_lit, .semi});
    try testLex("123abc", &.{.invalid});
    try testLex("123", &.{.int_lit});
    try testLex("123_456", &.{.int_lit});
    try testLex("123_456_789", &.{.int_lit});

    // binary
    try testLex("0b0", &.{.int_lit});
    try testLex("0b1", &.{.int_lit});
    try testLex("0b0101011", &.{.int_lit});

    // octal
    try testLex("0o0", &.{.int_lit});
    try testLex("0o1", &.{.int_lit});
    try testLex("0o0123456", &.{.int_lit});
    try testLex("0o17", &.{.int_lit});
    try testLex("0o178", &.{.invalid});
    try testLex("0o17abc", &.{.invalid});
    try testLex("0o12345_67", &.{.int_lit});

    // hex
    try testLex("0x0", &.{.int_lit});
    try testLex("0x1", &.{.int_lit});
    try testLex("0x018ADFF", &.{.int_lit});
    try testLex("0x0123456", &.{.int_lit});
    try testLex("0x789", &.{.int_lit});
    try testLex("0xabcdef", &.{.int_lit});
    try testLex("0xABCDEF", &.{.int_lit});
    try testLex("0x123G", &.{.invalid});
    try testLex("0x12_3456_789_a_b_CDEF", &.{.int_lit});
}

test "float literal" {
    try testLex(".", &.{.period});
    try testLex(".5", &.{.float_lit});
    try testLex("0.5", &.{.float_lit});
    try testLex("1.5", &.{.float_lit});
    try testLex(".51231232", &.{.float_lit});
    try testLex(".51231232e05", &.{.float_lit});
    try testLex(".51231232e+15", &.{.float_lit});
    try testLex(".51231232e-15", &.{.float_lit});
    try testLex(".5_234_12_32e-10", &.{.float_lit});
}

test "punctuation" {
    try testLex(";", &.{.semi});
    try testLex(":", &.{.colon});
    try testLex("=", &.{.equal});
    try testLex(".", &.{.period});
    try testLex(",", &.{.comma});
    try testLex("_", &.{.underscore});
    try testLex("(", &.{.l_paren});
    try testLex(")", &.{.r_paren});
    try testLex("[", &.{.l_bracket});
    try testLex("]", &.{.r_bracket});
    try testLex("{", &.{.l_brace});
    try testLex("}", &.{.r_brace});
    try testLex("+", &.{.plus});
    try testLex("-", &.{.minus});
    try testLex("*", &.{.asterisk});
    try testLex("/", &.{.slash});
    try testLex("%", &.{.percent});
    try testLex("&", &.{.ampersand});
    try testLex("|", &.{.pipe});
    try testLex("^", &.{.caret});
    try testLex("~", &.{.tilde});
    try testLex("!", &.{.bang});
    try testLex("<", &.{.l_angle});
    try testLex(">", &.{.r_angle});

    try testLex("+=", &.{.plus_equal});
    try testLex("+ =", &.{.plus, .equal});
    try testLex("-=", &.{.minus_equal});
    try testLex("- =", &.{.minus, .equal});
    try testLex("*=", &.{.asterisk_equal});
    try testLex("* =", &.{.asterisk, .equal});
    try testLex("/=", &.{.slash_equal});
    try testLex("/ =", &.{.slash, .equal});
    try testLex("%=", &.{.percent_equal});
    try testLex("% =", &.{.percent, .equal});
    try testLex("&=", &.{.ampersand_equal});
    try testLex("& =", &.{.ampersand, .equal});
    try testLex("|=", &.{.pipe_equal});
    try testLex("| =", &.{.pipe, .equal});
    try testLex("^=", &.{.caret_equal});
    try testLex("^ =", &.{.caret, .equal});
    try testLex("<<", &.{.l_angle_l_angle});
    try testLex("< <", &.{.l_angle, .l_angle});
    try testLex(">>", &.{.r_angle_r_angle});
    try testLex("> >", &.{.r_angle, .r_angle});
    try testLex("&&", &.{.ampersand_ampersand});
    try testLex("& &", &.{.ampersand, .ampersand});
    try testLex("||", &.{.pipe_pipe});
    try testLex("| |", &.{.pipe, .pipe});
    try testLex("^^", &.{.caret_caret});
    try testLex("^ ^", &.{.caret, .caret});
    try testLex("==", &.{.equal_equal});
    try testLex("= =", &.{.equal, .equal});
    try testLex("<=", &.{.l_angle_equal});
    try testLex("< =", &.{.l_angle, .equal});
    try testLex(">=", &.{.r_angle_equal});
    try testLex("> =", &.{.r_angle, .equal});
    try testLex("!=", &.{.bang_equal});
    try testLex("! =", &.{.bang, .equal});
    try testLex("::", &.{.colon_colon});
    try testLex(": :", &.{.colon, .colon});

    try testLex("<<=", &.{.l_angle_l_angle_equal});
    try testLex("<< =", &.{.l_angle_l_angle, .equal});
    try testLex(">>=", &.{.r_angle_r_angle_equal});
    try testLex(">> =", &.{.r_angle_r_angle, .equal});
    try testLex("&&=", &.{.ampersand_ampersand_equal});
    try testLex("&& =", &.{.ampersand_ampersand, .equal});
    try testLex("||=", &.{.pipe_pipe_equal});
    try testLex("|| =", &.{.pipe_pipe, .equal});
    try testLex("^^=", &.{.caret_caret_equal});
    try testLex("^^ =", &.{.caret_caret, .equal});
}

test "keywords" {
    try testLex("use", &.{.k_use});
    try testLex("as", &.{.k_as});
    try testLex("fn", &.{.k_fn});
    try testLex("return", &.{.k_return});
    try testLex("let", &.{.k_let});
    try testLex("mut", &.{.k_mut});
    try testLex("type", &.{.k_type});
    try testLex("if", &.{.k_if});
    try testLex("else", &.{.k_else});
    try testLex("yield", &.{.k_yield});
    try testLex("struct", &.{.k_struct});
    try testLex("enum", &.{.k_enum});
    try testLex("variant", &.{.k_variant});
    try testLex("defer", &.{.k_defer});
    try testLex("for", &.{.k_for});
    try testLex("break", &.{.k_break});
}

test "annotations" {
    try testLex("@inline", &.{.a_inline});
    try testLex("@import", &.{.a_import});
    try testLex("@export", &.{.a_export});
}

test "line comments" {
    try testLex("//test", &.{});
}

test "arith.fm" {
    try testLex(@embedFile("tests/arith.fm"), &.{
        .k_let, .ident, .equal, .k_fn, .l_paren, .r_paren, .ident, .l_brace,
        .k_let, .ident, .equal, .int_lit, .semi,
        .k_let, .ident, .equal, .int_lit, .semi,
        .k_let, .ident, .equal, .ident, .plus, .ident, .semi,
        .r_brace, .semi,
    });
}

test "fact-iter.fm" {
    try testLex(@embedFile("tests/fact-iter.fm"), &.{
        .k_let, .ident, .equal, .k_fn, .l_paren, .ident, .colon, .ident, .r_paren, .ident, .l_brace,
        .k_let, .k_mut, .ident, .colon, .ident, .equal, .int_lit, .semi,
        .k_for,
        .k_let, .k_mut, .ident, .equal, .int_lit, .semi,
        .ident, .l_angle_equal, .ident, .semi,
        .ident, .plus_equal, .int_lit,
        .l_brace,
        .ident, .asterisk_equal, .ident, .semi,
        .r_brace,
        .k_return, .ident, .semi,
        .r_brace,
    });
}
