const std = @import("std");
const lex = @import("lex.zig");
const Ast = @import("Ast.zig");
const error_handler = @import("error_handler.zig");

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Lexer = lex.Lexer;
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;

pub const Error = error{UnexpectedToken} || error{HandledUserError} || Allocator.Error;
const null_node: Node.Index = 0;

// parses a string of source characters into an abstract syntax tree
// gpa: allocator for tree data that outlives this function call
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Ast {
    var tokens: Ast.TokenList = .{};
    var integers: std.ArrayListUnmanaged(u64) = .{};
    defer tokens.deinit(gpa);
    defer integers.deinit(gpa);

    // lex entire source file into token list
    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.next();
        try tokens.append(gpa, .{
            .tag = token.tag,
            .start = @intCast(token.loc.start),
        });
        switch (token.tag) {
            .eof => break,
            .int_lit => try integers.append(gpa, lexer.int_value),
            else => {},
        }
    }

    // initialize parser
    var parser = Parser.init(gpa, source, &tokens, integers.items);
    defer parser.deinit();

    _ = try parser.addNode(.{
        .main_token = 0,
        .data = .{ .placeholder = {} },
    });
    _ = try parser.module();

    // copy parser results into an abstract syntax tree
    // that owns the source, token list, node list, and node extra data
    return Ast{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = try parser.extra.toOwnedSlice(gpa),
        .errors = try parser.errors.toOwnedSlice(),
    };
}

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    token_starts: []const u32,
    index: u32,
    integers: []const u64,
    int_index: u32,

    nodes: std.MultiArrayList(Node),
    extra: std.ArrayListUnmanaged(Node.Index),
    scratch: std.ArrayList(Node.Index),
    attributes: std.ArrayListUnmanaged(Node.Index),
    errors: std.ArrayList(error_handler.SourceError),

    pub fn init(gpa: Allocator, source: []const u8, tokens: *Ast.TokenList, integers: []const u64) Parser {
        return .{
            .source = source,
            .gpa = gpa,
            .token_tags = tokens.items(.tag),
            .token_starts = tokens.items(.start),
            .index = 0,
            .integers = integers,
            .int_index = 0,
            .nodes = .{},
            .extra = .{},
            .scratch = std.ArrayList(Node.Index).init(gpa),
            .attributes = .{},
            .errors = std.ArrayList(error_handler.SourceError).init(gpa),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.nodes.deinit(self.gpa);
        self.extra.deinit(self.gpa);
    }

    fn addNode(p: *Parser, node: Node) !Node.Index {
        const result: u32 = @intCast(p.nodes.len);
        try p.nodes.append(p.gpa, node);
        return result;
    }

    fn setNode(p: *Parser, i: usize, node: Node) Node.Index {
        p.nodes.set(i, node);
        return @intCast(i);
    }

    fn reserveNode(p: *Parser, tag: Ast.Node.Tag) !usize {
        try p.nodes.resize(p.gpa, p.nodes.len + 1);
        p.nodes.items(.tag)[p.nodes.len - 1] = tag;
        return p.nodes.len - 1;
    }

    // eats the current token (whatever it is) and returns the index
    fn eatCurrent(p: *Parser) TokenIndex {
        p.index += 1;
        return p.index - 1;
    }

    // eats the current token if it matches a tag, and returns null otherwise
    fn eat(p: *Parser, tag: Token.Tag) ?TokenIndex {
        if (p.token_tags[p.index] == tag) {
            return p.eatCurrent();
        } else {
            return null;
        }
    }

    // east the current token if it matches a tag, and errors otherwise
    fn expect(p: *Parser, tag: Token.Tag) Error!TokenIndex {
        if (p.eat(tag)) |token| {
            return token;
        } else {
            return error.UnexpectedToken;
        }
    }

    inline fn current(p: *Parser) Token.Tag {
        return p.token_tags[p.index];
    }

    // this can quite easily go out of bounds, so it should only be used
    // after checking for the guard token (eof)
    inline fn next(p: *Parser, offset: u32) Token.Tag {
        return p.token_tags[p.index + offset];
    }

    fn addExtra(p: *Parser, extra: anytype) Allocator.Error!Node.Index {
        const fields = std.meta.fields(@TypeOf(extra));
        try p.extra.ensureUnusedCapacity(p.gpa, fields.len);
        const len: u32 = @intCast(p.extra.items.len);
        inline for (fields) |field| {
            comptime std.debug.assert(field.type == Node.Index);
            p.extra.appendAssumeCapacity(@field(extra, field.name));
        }
        return len;
    }

    pub fn extraSlice(p: *Parser, sl: Ast.Node.ExtraSlice) []const u32 {
        const start: u32 = @intFromEnum(sl.start);
        const end: u32 = @intFromEnum(sl.end);
        return p.extra.items[start..end];
    }

    pub fn addSlice(p: *Parser, sl: []const u32) !Ast.Node.ExtraIndex {
        const start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(p.gpa, sl);
        const end: u32 = @intCast(p.extra.items.len);
        return p.addExtra(Ast.Node.ExtraSlice{
            .start = @intCast(start),
            .end = @intCast(end),
        });
    }

    fn parseList(p: *Parser, comptime element: anytype, surround: struct { open: Token.Tag, close: Token.Tag }) !Node.ExtraIndex {
        _ = try p.expect(surround.open);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(surround.close)) |_| break;
            const element_node = try @call(.auto, element, .{p});
            try p.scratch.append(element_node);

            if (p.current() == .comma) {
                _ = p.eatCurrent();
            } else if (p.current() != surround.close) {
                return error.UnexpectedToken;
            }
        }

        const elements = p.scratch.items[scratch_top..];
        return p.addSlice(elements);
    }

    // TODO: refactor this into two seperate methods
    // really small perf impact, but why not just make it better =)
    fn consumeUntilValidTLD(p: *Parser, report_unmatched: bool) Allocator.Error!void {
        // consume until we're in a valid state e.g. next let
        var open_parenths: i32 = 0;
        var open_braces: i32 = 0;
        // var open_quotes: u32 = 0;
        // var open_square: u32 = 0;

        var last_open_parenth: u32 = 0;
        var last_close_parenth: u32 = 0;
        var last_open_brace: u32 = 0;
        var last_close_brace: u32 = 0;
        // var last_open_quote: u32 = 0;
        // var last_open_square: u32 = 0;

        while (true) {
            switch (p.token_tags[p.index]) {
                .eof => break,
                .l_paren => {
                    last_open_parenth = p.index;
                    open_parenths += 1;
                },
                .r_paren => {
                    last_close_parenth = p.index;
                    open_parenths -= 1;
                },
                .l_brace => {
                    last_open_brace = p.index;
                    open_braces += 1;
                },
                .r_brace => {
                    last_close_brace = p.index;
                    open_braces -= 1;
                },
                .k_let => {
                    // have to make sure this isn't in some smaller block
                    if (open_parenths == 0 and open_braces == 0) break;
                },
                else => {},
            }
            p.index += 1;
        }

        if (report_unmatched) {
            if (open_parenths > 0) {
                try p.errors.append(.{ .tag = .unmatched_parenth, .token = last_open_parenth });
            }

            if (open_braces > 0) {
                try p.errors.append(.{ .tag = .unmatched_brace, .token = last_open_brace });
            }

            if (open_parenths < 0) {
                try p.errors.append(.{ .tag = .unmatched_parenth, .token = last_close_parenth });
            }

            if (open_braces < 0) {
                try p.errors.append(.{ .tag = .unmatched_brace, .token = last_close_brace });
            }
        }
    }

    fn consumeUntilSemi(p: *Parser) Allocator.Error!void {
        while (true) {
            switch (p.token_tags[p.index]) {
                .semi => {
                    p.index += 1;
                    break;
                },
                .eof => break,
                else => {},
            }
            p.index += 1;
        }
    }

    const min_precedence: i32 = 0;

    inline fn precedence(tag: Token.Tag) i32 {
        return switch (tag) {
            .k_implies => 9,
            .k_or => 10,
            .k_and => 11,
            .k_xor => 12,
            .equal_equal => 13,
            .bang_equal => 13,
            .l_angle => 14,
            .r_angle => 14,
            .l_angle_equal => 14,
            .r_angle_equal => 14,
            .plus => 20,
            .minus => 20,
            .asterisk => 21,
            .slash => 21,
            .percent => 22,
            .pipe => 30,
            .ampersand => 31,
            .caret => 32,
            .r_angle_r_angle => 33,
            .l_angle_l_angle => 33,
            else => -1,
        };
    }

    pub fn module(p: *Parser) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const node = switch (p.current()) {
                .eof => break,
                .a_export,
                .a_import,
                .k_let,
                .k_mut,
                => p.declaration() catch |err| {
                    if (err == Error.HandledUserError) {
                        while (true) {
                            // TODO: this should be cleaned up by the emitter fo the HandledUserError
                            // e.g. we should be either be at or next to a eof or let.
                            switch (p.current()) {
                                .eof, .k_let => {
                                    break;
                                },
                                else => {},
                            }
                            p.index += 1;
                        }
                        continue;
                    } else return err;
                    return;
                },
                .k_type => try p.parseTypeDecl(),
                else => {
                    try p.errors.append(.{ .tag = .unexpected_tld_token, .token = p.index });
                    try consumeUntilValidTLD(p, false);
                    continue;
                },
            };

            try p.scratch.append(node);
            _ = try p.expect(.semi);
        }

        const stmts = p.scratch.items[scratch_top..];
        const pl = try p.addSlice(stmts);

        return p.addNode(.{
            .main_token = 0,
            .data = .{ .module = .{ .stmts = pl } },
        });
    }

    // expression parsing
    fn expression(p: *Parser) Error!Node.Index {
        const expr = try p.unary(true);
        return p.associateBinary(expr, min_precedence);
    }

    fn primary(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        return switch (p.current()) {
            // even though parentheses aren't necessary due to the ast
            // being nested, they are added to facilitate 1-1 mapping between
            // source and ast for tooling and testing
            .l_paren => {
                const l_paren_token = try p.expect(.l_paren);
                const inner_node = try p.expression();
                _ = try p.expect(.r_paren);

                return p.addNode(.{
                    .main_token = l_paren_token,
                    .data = .{ .paren = inner_node },
                });
            },
            .ident => if (accept_l_brace and p.next(1) == .l_brace) p.structLiteral() else p.identifier(),
            // const ident = try p.identifier();
            // return switch (p.current()) {
            //     .l_paren => p.call(ident),
            //     .period => p.member(ident),
            //     .l_bracket => return p.subscript(ident),
            //     .l_brace => {
            //         if (accept_l_brace) {
            //             return p.structLiteral(ident);
            //         } else {
            //             return ident;
            //         }
            //     },
            //     else => ident,
            // };
            // },
            .k_fn => p.functionLiteral(),
            // switch (p.token_tags[p.index + 1]) {
            //     .l_paren => p.call(try p.identifier()),
            //     else => p.identifier(),
            // },
            .int_lit => {
                const literal = p.addNode(.{
                    .main_token = p.eatCurrent(),
                    .data = .{
                        .integer_literal = p.integers[p.int_index],
                    },
                });
                p.int_index += 1;
                return literal;
            },
            .float_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .float_literal = {} },
            }),
            .k_true => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .bool_literal = {} },
            }),
            .k_false => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .bool_literal = {} },
            }),
            .char_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .char_literal = {} },
            }),
            .str_lit => p.addNode(.{
                .main_token = p.eatCurrent(),
                .data = .{ .string_literal = {} },
            }),
            .l_bracket => p.arrayLiteral(),
            .l_brace => p.block(),
            .k_if => p.branch(),
            // .minus, .bang, .tilde, .ampersand, .asterisk => p.addNode(.{
            //     .main_token = try p.expect(p.token_tags[p.index]),
            //     .data = .{ .unary = try p.primary(true) },
            // }),
            else => return Error.UnexpectedToken,
        };
    }

    fn associateBinary(p: *Parser, l: Node.Index, expr_precedence: i32) !Node.Index {
        // tries to associate an existing "left side" node with a right side
        // in one or more binary expressions - operator precedence parsing
        var l_node = l;
        while (true) {
            const prec = precedence(p.current());
            if (prec < expr_precedence) {
                return l_node;
            }

            const op_token = p.eatCurrent();
            var r_node = try p.unary(false);

            const next_prec = precedence(p.current());
            if (prec < next_prec) {
                r_node = try p.associateBinary(r_node, prec + 1);
            }

            l_node = try p.addNode(.{
                .main_token = op_token,
                .data = .{ .binary = .{
                    .left = l_node,
                    .right = r_node,
                } },
            });
        }
    }

    fn postfix(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        var expr = try p.primary(accept_l_brace);

        while (true) {
            expr = switch (p.current()) {
                // handles both subscript and slice, since we can't
                // yet predict far enough to know which it is
                .l_bracket => try p.subscript(expr),
                .period => try p.member(expr),
                .l_paren => try p.call(expr),
                else => return expr,
            };
        }
    }

    fn unary(p: *Parser, accept_l_brace: bool) Error!Node.Index {
        switch (p.current()) {
            .minus,
            .bang,
            .tilde,
            .ampersand,
            .asterisk,
            => {
                const unary_token = p.eatCurrent();
                const expr = try p.unary(accept_l_brace);
                return p.addNode(.{
                    .main_token = unary_token,
                    .data = .{ .unary = expr },
                });
            },
            else => return p.postfix(accept_l_brace),
        }
    }

    // identifier used as an expression (like a variable or type name)
    // expressions that need an identifier, like a decl or struct init,
    // just use main_token
    fn identifier(p: *Parser) !Node.Index {
        const ident_token = try p.expect(.ident);
        return p.addNode(.{
            .main_token = ident_token,
            .data = .{ .ident = {} },
        });
    }

    fn arrayLiteral(p: *Parser) !Node.Index {
        const l_bracket_token = try p.expect(.l_bracket);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        while (true) {
            if (p.eat(.r_bracket) != null) break;
            const element_node = try p.expression();
            try p.scratch.append(element_node);

            switch (p.current()) {
                .comma => _ = p.eatCurrent(),
                .r_bracket => {},
                else => return error.UnexpectedToken,
            }
        }

        const elements = p.scratch.items[scratch_top..];
        const pl = try p.addSlice(elements);
        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .array_literal = .{ .elements = pl } },
        });
    }

    fn structLiteral(p: *Parser) Error!Node.Index {
        const start_token = p.index;
        const type_node = try p.identifier(); // TODO: this should be a proper type expression

        const fields = try p.parseList(fieldInitializer, .{ .open = .l_brace, .close = .r_brace });
        return p.addNode(.{
            .main_token = start_token,
            .data = .{ .struct_literal = .{ .struct_type = type_node, .fields = fields } },
        });
    }

    // .field = value
    fn fieldInitializer(p: *Parser) !Node.Index {
        var err = false;

        const dot_token = try p.expect(.period);
        _ = p.expect(.ident) catch token: {
            try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
            err = true;
            break :token undefined;
        };

        _ = try p.expect(.equal);

        const initializer = p.expression() catch node: {
            try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
            err = true;
            break :node undefined;
        };

        if (err) return error.HandledUserError;
        return p.addNode(.{
            .main_token = dot_token,
            .data = .{ .field_initializer = initializer },
        });
    }

    // operand[index]
    fn subscript(p: *Parser, operand: Node.Index) Error!Node.Index {
        const l_bracket_token = try p.expect(.l_bracket);
        const index = try p.expression();
        if (p.current() == .period_period) {
            return p.slice(l_bracket_token, operand, index);
        }
        _ = try p.expect(.r_bracket);

        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .subscript = .{ .operand = operand, .index = index } },
        });
    }

    // operand[start .. end]
    fn slice(p: *Parser, token: TokenIndex, op: Node.Index, start: Node.Index) Error!Node.Index {
        _ = try p.expect(.period_period);
        const end = try p.expression();
        _ = try p.expect(.r_bracket);

        const pl = try p.addExtra(Node.GetSlice{
            .start = start,
            .end = end,
        });
        return p.addNode(.{
            .main_token = token,
            .data = .{ .slice = .{ .operand = op, .range = pl } },
        });
    }

    // operand.member
    fn member(p: *Parser, operand: Node.Index) Error!Node.Index {
        const dot_token = try p.expect(.period);
        _ = try p.expect(.ident);

        return p.addNode(.{
            .main_token = dot_token,
            .data = .{ .member = operand },
        });
    }

    inline fn typeExpression(p: *Parser) !Node.Index {
        return p.postfixType();
    }

    fn postfixType(p: *Parser) Error!Node.Index {
        var expr = try p.primaryType();

        while (true) {
            expr = switch (p.current()) {
                .asterisk => try p.pointerType(expr),
                .l_bracket => switch (p.next(1)) {
                    .r_bracket => try p.sliceType(expr),
                    .asterisk => try p.manyPointerType(expr),
                    .eof => return expr,
                    else => try p.arrayType(expr),
                },
                .k_mut => switch (p.next(1)) {
                    .asterisk => try p.pointerType(expr),
                    .l_bracket => switch (p.next(2)) {
                        .r_bracket => try p.sliceType(expr),
                        .asterisk => try p.manyPointerType(expr),
                        .eof => return expr,
                        else => try p.arrayType(expr),
                    },
                    else => return error.UnexpectedToken,
                },
                else => return expr,
            };
        }
    }

    fn primaryType(p: *Parser) Error!Node.Index {
        return switch (p.current()) {
            // even though parentheses aren't necessary due to the ast
            // being nested, they are added to facilitate 1-1 mapping between
            // source and ast for tooling and testing
            .l_paren => {
                const l_paren_token = try p.expect(.l_paren);
                const inner_node = try p.typeExpression();
                _ = try p.expect(.r_paren);

                return p.addNode(.{
                    .main_token = l_paren_token,
                    .data = .{ .paren = inner_node },
                });
            },
            .k_struct => try p.structType(),
            .k_fn => try p.functionType(),
            .ident => node: {
                const ident_token = try p.expect(.ident);
                break :node try p.addNode(.{
                    .main_token = ident_token,
                    .data = .{ .ident = {} },
                });
            },
            else => {
                std.debug.print("{}\n", .{p.token_tags[p.index]});
                return Error.UnexpectedToken;
            },
        };
    }

    fn pointerType(p: *Parser, ty: Node.Index) !Node.Index {
        const mut_token = p.eat(.k_mut);
        const asterisk_token = try p.expect(.asterisk);
        if (mut_token) |mut| {
            return p.addNode(.{
                .main_token = mut,
                .data = .{ .mut_pointer_type = ty },
            });
        } else {
            return p.addNode(.{
                .main_token = asterisk_token,
                .data = .{ .pointer_type = ty },
            });
        }
    }

    fn sliceType(p: *Parser, ty: Node.Index) !Node.Index {
        const mut_token = p.eat(.k_mut);
        const l_bracket_token = try p.expect(.l_bracket);
        _ = try p.expect(.r_bracket);
        if (mut_token) |mut| {
            return p.addNode(.{
                .main_token = mut,
                .data = .{ .mut_slice_type = ty },
            });
        } else {
            return p.addNode(.{
                .main_token = l_bracket_token,
                .data = .{ .slice_type = ty },
            });
        }
    }

    fn manyPointerType(p: *Parser, ty: Node.Index) !Node.Index {
        const mut_token = p.eat(.k_mut);
        const l_bracket_token = try p.expect(.l_bracket);
        _ = try p.expect(.asterisk);
        _ = try p.expect(.r_bracket);
        if (mut_token) |mut| {
            return p.addNode(.{
                .main_token = mut,
                .data = .{ .mut_many_pointer_type = ty },
            });
        } else {
            return p.addNode(.{
                .main_token = l_bracket_token,
                .data = .{ .many_pointer_type = ty },
            });
        }
    }

    fn arrayType(p: *Parser, ty: Node.Index) !Node.Index {
        const l_bracket_token = try p.expect(.l_bracket);
        const count = try p.expression();
        _ = try p.expect(.r_bracket);
        return p.addNode(.{
            .main_token = l_bracket_token,
            .data = .{ .array_type = .{ .element_type = ty, .count_expr = count } },
        });
    }

    fn functionType(p: *Parser) !Node.Index {
        const fn_token = try p.expect(.k_fn);
        const params = try p.parseList(param, .{ .open = .l_paren, .close = .r_paren });
        if (p.typeExpression()) |return_type| {
            return p.addNode(.{
                .main_token = fn_token,
                .data = .{
                    .function_type = .{
                        .params = params,
                        .@"return" = return_type,
                    },
                },
            });
        } else |err| switch (err) {
            Error.UnexpectedToken => {
                try p.errors.append(.{ .tag = .missing_return_type, .token = p.index });
                return Error.HandledUserError;
            },
            else => return err,
        }
    }

    fn structType(p: *Parser) !Node.Index {
        const struct_token = try p.expect(.k_struct);
        const fields = try p.parseList(structField, .{ .open = .l_brace, .close = .r_brace });
        return p.addNode(.{
            .main_token = struct_token,
            .data = .{ .struct_type = .{ .fields = fields } },
        });
    }

    fn functionLiteral(p: *Parser) !Node.Index {
        const function_type = try p.functionType();
        // TODO: there should be some better error recovery like consuming the block on error
        const body = try p.block();
        return p.addNode(.{
            .main_token = undefined,
            .data = .{ .function_literal = .{
                .ty = function_type,
                .body = body,
            } },
        });
    }

    fn param(p: *Parser) !Node.Index {
        var err = false;
        const mut_token = p.eat(.k_mut);
        const ident_token = p.expect(.ident) catch token: {
            // We're missing an identifier flag it
            try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
            err = true;
            break :token undefined;
        };

        _ = p.expect(.colon) catch {
            // We're missing a colon flag it
            try p.errors.append(.{ .tag = .missing_colon, .token = p.index });
            err = true;
        };
        const type_node = p.typeExpression() catch node: {
            // We're missing a type annotation
            try p.errors.append(.{ .tag = .missing_type_annotation, .token = p.index });
            err = true;
            break :node undefined;
        };

        if (err) return error.HandledUserError;
        if (mut_token) |_| {
            return p.addNode(.{
                .main_token = ident_token,
                .data = .{ .param_mut = type_node },
            });
        } else {
            return p.addNode(.{
                .main_token = ident_token,
                .data = .{ .param = type_node },
            });
        }
    }

    fn structField(p: *Parser) !Node.Index {
        var err = false;
        const ident_token = p.expect(.ident) catch token: {
            // We're missing an identifier flag it
            try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
            err = true;
            break :token undefined;
        };

        _ = p.expect(.colon) catch {
            // We're missing a colon flag it
            try p.errors.append(.{ .tag = .missing_colon, .token = p.index });
            err = true;
        };
        const type_node = p.typeExpression() catch node: {
            // We're missing a type annotation
            try p.errors.append(.{ .tag = .missing_type_annotation, .token = p.index });
            err = true;
            break :node undefined;
        };

        if (err) return error.HandledUserError;
        return p.addNode(.{
            .main_token = ident_token,
            .data = .{ .field = type_node },
        });
    }

    // // parses a distinct type declaration (distinct type Point = ...)
    // fn expectDistinctTypeDecl(self: *Parser) !Node.Index {
    //     const distinct_token = try self.expect(.k_distinct);
    //     _ = try self.expect(.k_type);
    //     _ = try self.expect(.ident); // not stored, (main_token == distinct_token) + 2
    //     _ = try self.expect(.equal);
    //     const type_node = try self.typeExpression();
    //
    //     return self.addNode(.{
    //         .main_token = distinct_token,
    //         .data = .{ .distinct_type_decl = type_node },
    //     });
    // }

    fn block(p: *Parser) !Node.Index {
        const l_brace_token = try p.expect(.l_brace);

        // since each block may create an arbitrary number of statements,
        // we collect the toplevel statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eat(.r_brace)) |_| break;
            if (p.statement()) |stmt_node| {
                try p.scratch.append(stmt_node);
            } else |err| switch (err) {
                error.HandledUserError => continue,
                else => return err,
            }
        }

        const stmts = p.scratch.items[scratch_top..];
        const pl = try p.addSlice(stmts);

        return p.addNode(.{
            .main_token = l_brace_token,
            .data = .{ .block = .{ .stmts = pl } },
        });
    }

    fn statement(p: *Parser) Error!Node.Index {
        const node = switch (p.token_tags[p.index]) {
            .k_let, .k_mut => p.declaration(),
            .k_if => p.branch(),
            .k_for => p.loop(),
            .k_type => p.parseTypeDecl(),
            // .k_distinct => p.expectDistinctTypeDecl(),
            .k_return => p.ret(),
            .k_yield => p.yield(),
            .k_break => p.brk(),
            else => node: {
                const expr = try p.expression();
                break :node switch (p.token_tags[p.index]) {
                    .equal,
                    .plus_equal,
                    .minus_equal,
                    .asterisk_equal,
                    .slash_equal,
                    .percent_equal,
                    .ampersand_equal,
                    .pipe_equal,
                    .caret_equal,
                    .l_angle_l_angle_equal,
                    .r_angle_r_angle_equal,
                    => p.assignment(expr),
                    else => {
                        if (p.nodes.items(.data)[expr] == .call) {
                            // function calls can exist on their own
                            // (implicit discard of return value)
                            break :node expr;
                        }

                        try p.errors.append(.{ .tag = .unexpected_identifier, .token = p.index });

                        // try eating until we get a semicolon
                        try consumeUntilSemi(p);

                        return Error.HandledUserError;
                    },
                };
            },
        };

        _ = p.eat(.semi);
        return node;
    }

    fn call(p: *Parser, ptr: Node.Index) !Node.Index {
        const l_paren_token = p.index;
        const args = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });

        return p.addNode(.{
            .main_token = l_paren_token,
            .data = .{ .call = .{
                .ptr = ptr,
                .args = args,
            } },
        });
    }

    fn assignment(p: *Parser, ptr: Node.Index) !Node.Index {
        switch (p.current()) {
            .equal => {
                const equal_token = try p.expect(.equal);
                const val = try p.expression();
                return p.addNode(.{
                    .main_token = equal_token,
                    .data = .{ .assign_simple = .{ .ptr = ptr, .val = val } },
                });
            },
            .plus_equal,
            .minus_equal,
            .asterisk_equal,
            .slash_equal,
            .percent_equal,
            .l_angle_l_angle_equal,
            .r_angle_r_angle_equal,
            => {
                const operator_token = p.eatCurrent();
                const val = try p.expression();
                return p.addNode(.{
                    .main_token = operator_token,
                    .data = .{ .assign_binary = .{ .ptr = ptr, .val = val } },
                });
            },
            else => {
                try p.errors.append(.{
                    .tag = .unexpected_token,
                    .token = p.index,
                });
                return error.HandledUserError;
            },
        }
    }

    fn declaration(p: *Parser) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const attr = switch (p.token_tags[p.index]) {
                inline .a_export,
                .a_import,
                => |tag| try p.attribute(tag),
                else => break,
            };
            try p.scratch.append(attr);
        }

        const is_mut = p.current() == .k_mut;
        const let_token = switch (p.current()) {
            .k_let, .k_mut => p.eatCurrent(),
            else => return error.UnexpectedToken,
        };
        // const let_token = switch (p.current()) {
        //     .k_let,
        // }
        // const let_token = try p.expect(.k_let);
        // var is_mut = false;
        // if (p.token_tags[p.index] == .k_mut) {
        //     _ = p.eatCurrent();
        //     is_mut = true;
        // }

        switch (p.token_tags[p.index]) {
            .ident => _ = p.eatCurrent(),
            .k_use,
            .k_as,
            .k_fn,
            .k_return,
            .k_let,
            .k_mut,
            .k_type,
            .k_distinct,
            .k_if,
            .k_else,
            .k_yield,
            .k_struct,
            .k_enum,
            .k_variant,
            .k_defer,
            .k_for,
            .k_break,
            .k_or,
            .k_and,
            .k_xor,
            .k_implies,
            .k_true,
            .k_false,
            => {
                try p.errors.append(.{ .tag = .shadows_keyword, .token = p.index });
                _ = p.eatCurrent();
            },
            else => try p.errors.append(.{ .tag = .missing_identifier, .token = p.index }),
        }
        const type_annotation = if (p.eat(.colon) == null) 0 else try p.typeExpression();

        var is_definition = true;
        switch (p.token_tags[p.index]) {
            .equal => _ = p.eatCurrent(),
            .semi => is_definition = false,
            else => try p.errors.append(.{ .tag = .missing_equals, .token = p.index }),
        }

        var rvalue: Node.Index = 0;
        if (is_definition) {
            if (p.expression()) |val| {
                rvalue = val;
            } else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
                    try consumeUntilSemi(p);
                    return Error.HandledUserError;
                },
                else => return err,
            }
        }

        if (!is_definition and type_annotation == 0) {
            // nothing to infer type from, so it must be annotated
            try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
            try consumeUntilSemi(p);
            return Error.HandledUserError;
        }

        if (is_mut) {
            // var decl
            return p.addNode(.{
                .main_token = let_token,
                .data = .{ .var_decl = .{ .ty = type_annotation, .val = rvalue } },
            });
        } else {
            // const decl
            const attributes = p.scratch.items[scratch_top..];
            if (attributes.len > 0) {
                // const with attributes
                const attrs_start: u32 = @intCast(p.extra.items.len);
                try p.extra.appendSlice(p.gpa, attributes);
                const attrs_end: u32 = @intCast(p.extra.items.len);

                const data = try p.addExtra(Node.DeclMetadata{
                    .ty = type_annotation,
                    .attrs_start = attrs_start,
                    .attrs_end = attrs_end,
                });
                return p.addNode(.{
                    .main_token = let_token,
                    .data = .{ .const_decl_attr = .{ .metadata = data, .val = rvalue } },
                });
            } else {
                // simple const
                return p.addNode(.{
                    .main_token = let_token,
                    .data = .{ .const_decl = .{ .ty = type_annotation, .val = rvalue } },
                });
            }
        }
    }

    fn parseTypeDecl(p: *Parser) !Node.Index {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        const type_token = try p.expect(.k_type);

        switch (p.token_tags[p.index]) {
            .ident => _ = p.eatCurrent(),
            .k_use,
            .k_as,
            .k_fn,
            .k_return,
            .k_let,
            .k_mut,
            .k_type,
            .k_distinct,
            .k_if,
            .k_else,
            .k_yield,
            .k_struct,
            .k_enum,
            .k_variant,
            .k_defer,
            .k_for,
            .k_break,
            .k_or,
            .k_and,
            .k_xor,
            .k_implies,
            .k_true,
            .k_false,
            => {
                try p.errors.append(.{ .tag = .shadows_keyword, .token = p.index });
                _ = p.eatCurrent();
            },
            else => try p.errors.append(.{ .tag = .missing_identifier, .token = p.index }),
        }

        if (p.eat(.equal) == null) {
            try p.errors.append(.{ .tag = .missing_equals, .token = p.index });
        }

        if (p.typeExpression()) |ty| {
            return p.addNode(.{
                .main_token = type_token,
                .data = .{ .type_decl = ty },
            });
        } else |err| switch (err) {
            Error.UnexpectedToken => {
                try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
                try consumeUntilSemi(p);
                return Error.HandledUserError;
            },
            else => return err,
        }
    }

    fn ret(p: *Parser) !Node.Index {
        const ret_token = try p.expect(.k_return);
        const return_val = if (p.current() == .semi) null_node else try p.expression();
        return p.addNode(.{
            .main_token = ret_token,
            .data = .{ .return_val = return_val },
        });
    }

    fn yield(p: *Parser) !Node.Index {
        const yield_token = try p.expect(.k_yield);
        const yield_val = try p.expression();
        return p.addNode(.{
            .main_token = yield_token,
            .data = .{ .yield_val = yield_val },
        });
    }

    fn branch(p: *Parser) !Node.Index {
        // all conditional branches start with the if keyword
        const if_token = try p.expect(.k_if);

        // we have three kinds of if statements: simple, else, and chain
        // which we progressively try to match against
        const condition = try p.expression();
        const exec_true = try p.block();

        if (p.current() != .k_else) {
            // simple if
            return p.addNode(.{
                .main_token = if_token,
                .data = .{ .if_simple = .{
                    .condition = condition,
                    .exec_true = exec_true,
                } },
            });
        }

        _ = p.eatCurrent();
        if (p.current() == .k_if) {
            // chained if
            const chain_next = try p.branch();
            const pl = try p.addExtra(Node.IfChain{
                .exec_true = exec_true,
                .next = chain_next,
            });
            return p.addNode(.{
                .main_token = if_token,
                .data = .{ .if_chain = .{
                    .condition = condition,
                    .chain = pl,
                } },
            });
        } else {
            // if else
            const exec_false = try p.block();
            const exec = try p.addExtra(Node.IfElse{
                .exec_true = exec_true,
                .exec_false = exec_false,
            });
            return p.addNode(.{
                .main_token = if_token,
                .data = .{ .if_else = .{
                    .condition = condition,
                    .exec = exec,
                } },
            });
        }
    }

    fn loop(p: *Parser) !Node.Index {
        // all loops start with the for keyword
        const for_token = try p.expect(.k_for);

        // we have three kinds of loops: forever, conditional, range
        // which we progressively try to match against
        switch (p.current()) {
            .l_brace => {
                // forever loop
                const body = try p.block();
                return p.addNode(.{
                    .main_token = for_token,
                    .data = .{ .loop_forever = .{ .body = body } },
                });
            },
            // TODO: we should have a more rigorous way to determine this,
            // and also let things that aren't declarations in the binding
            .k_let, .k_mut => {
                // declaration = assume this is the binding, and we are in a range loop
                const binding = try p.declaration();
                _ = try p.expect(.semi);
                const condition = try p.expression();
                _ = try p.expect(.semi);
                const afterthought = try p.statement();
                const signature = try p.addExtra(Node.RangeSignature{
                    .binding = binding,
                    .condition = condition,
                    .afterthought = afterthought,
                });

                const body = try p.block();
                return p.addNode(.{
                    .main_token = for_token,
                    .data = .{ .loop_range = .{
                        .signature = signature,
                        .body = body,
                    } },
                });
            },
            else => {
                // assume this is the condition of a conditional loop
                const condition = try p.expression();
                const body = try p.block();
                return p.addNode(.{
                    .main_token = for_token,
                    .data = .{ .loop_conditional = .{
                        .condition = condition,
                        .body = body,
                    } },
                });
            },
        }
    }

    fn brk(p: *Parser) !Node.Index {
        const break_token = try p.expect(.k_break);
        return p.addNode(.{
            .main_token = break_token,
            .data = .{ .@"break" = {} },
        });
    }

    fn attribute(p: *Parser, comptime tag: Token.Tag) !Node.Index {
        const attr_token = try p.expect(tag);
        if (p.token_tags[p.index] == .l_paren) {
            const args = try p.parseList(expression, .{ .open = .l_paren, .close = .r_paren });
            return p.addNode(.{
                .main_token = attr_token,
                .data = .{ .attr_args = args },
            });
        } else {
            return p.addNode(.{
                .main_token = attr_token,
                .data = .{ .attr_simple = {} },
            });
        }
    }
};

// fn testParse(source: [:0]const u8, allocator: Allocator, anything_changed: *bool) ![]u8 {
//     const stderr = std.io.getStdErr().writer();
//
//     var tree = try parse(allocator, source);

// for (tree.errors) |parse_error| {
//
// }
test "arith.fm" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var allocator = gpa.allocator();

    const ast = try parse(allocator, @embedFile("tests/arith.fm"));
    _ = ast;
}
