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
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);
    var integers = std.ArrayListUnmanaged(u64){};
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
    defer parser.nodes.deinit(gpa);
    defer parser.extra.deinit(gpa);

    _ = try parser.addNode(.{
        .main_token = 0,
        .data = .{ .placeholder = {} },
    });
    _ = try parser.parseModule();

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

    fn eatToken(p: *Parser, tag: Token.Tag) ?TokenIndex {
        if (p.token_tags[p.index] == tag) {
            p.index += 1;
            return p.index - 1;
        } else {
            return null;
        }
    }

    fn expectToken(p: *Parser, tag: Token.Tag) Error!TokenIndex {
        if (p.eatToken(tag)) |token| {
            return token;
        } else {
            return Error.UnexpectedToken;
        }
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

    fn precedence(tag: Token.Tag) i32 {
        return switch (tag) {
            .k_or => 10,
            .k_and => 11,
            .equal_equal => 12,
            .bang_equal => 12,
            .l_angle => 13,
            .r_angle => 13,
            .l_angle_equal => 13,
            .r_angle_equal => 13,
            .plus => 110,
            .minus => 120,
            .asterisk => 130,
            .slash => 140,
            .percent => 140,
            else => -1,
        };
    }

    pub fn parseModule(p: *Parser) !Node.Index {
        // each toplevel (file) may create any number of global statements
        // so we collect the statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const node = switch (p.token_tags[p.index]) {
                .eof => break,
                .a_export => {
                    try p.parseExport();
                    continue;
                },
                .k_let => p.parseDecl() catch |err| {
                    if (err == Error.HandledUserError) {
                        while (true) {
                            // this should be cleaned up by the emitter fo the HandledUserError
                            // e.g. we should be either be at or next to a eof or let.
                            switch (p.token_tags[p.index]) {
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
                else => {
                    try p.errors.append(.{ .tag = .unexpected_tld_token, .token = p.index });
                    try consumeUntilValidTLD(p, false);
                    continue;
                },
            };

            try p.scratch.append(node);
            _ = p.eatToken(.semi);
        }

        const stmts = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, stmts);

        return p.addNode(.{
            .main_token = 0,
            .data = .{
                .module = .{
                    .stmts_start = @intCast(extra_top),
                    .stmts_end = @intCast(p.extra.items.len),
                },
            },
        });
    }

    fn expectExpr(p: *Parser) !Node.Index {
        // declarations that aren't part of logical or arithmetic expressions,
        // like string literals, function and aggregate declarations, etc,
        // are parsed and returned here directly
        // for everything else, we part in parsePrimaryExpr() and try to
        // associate it with a binary companion (parseBinRExp())
        return switch (p.token_tags[p.index]) {
            .k_fn => p.expectFnDecl(),
            else => {
                const left_node = try p.parsePrimaryExpr();
                return p.parseBinRExpr(left_node, 0);
            },
        };
    }

    fn parsePrimaryExpr(p: *Parser) Error!Node.Index {
        // parses an elementary expression such as a literal,
        // variable value, function call, or parenthesis
        return switch (p.token_tags[p.index]) {
            .l_paren => node: {
                // parentheses are used only for grouping in source code,
                // and don't generate ast nodes since the ast nesting itself
                // provides the correct grouping
                _ = try p.expectToken(.l_paren);
                const inner_node = p.expectExpr();
                _ = try p.expectToken(.r_paren);

                break :node inner_node;
            },
            .ident => switch (p.token_tags[p.index + 1]) {
                .l_paren => p.expectCall(try p.expectVarExpr()),
                else => p.expectVarExpr(),
            },
            .int_lit => expr: {
                const literal = p.addNode(.{
                    .main_token = try p.expectToken(.int_lit),
                    .data = .{
                        .integer_literal = p.integers[p.int_index],
                    },
                });
                p.int_index += 1;
                break :expr literal;
            },
            .float_lit => p.addNode(.{
                .main_token = try p.expectToken(.float_lit),
                .data = .{
                    .float_literal = {},
                },
            }),
            .k_true => p.addNode(.{
                .main_token = try p.expectToken(.k_true),
                .data = .{ .bool_literal = {} },
            }),
            .k_false => p.addNode(.{
                .main_token = try p.expectToken(.k_false),
                .data = .{ .bool_literal = {} },
            }),
            .plus, .minus, .bang, .tilde, .ampersand, .asterisk => p.addNode(.{
                .main_token = try p.expectToken(p.token_tags[p.index]),
                .data = .{ .unary_expr = try p.parsePrimaryExpr() },
            }),
            else => return Error.UnexpectedToken,
        };
    }

    fn parseBinRExpr(p: *Parser, l: Node.Index, expr_precedence: i32) !Node.Index {
        // tries to associate an existing "left side" node with a right side
        // in one or more binary expressions - operator precedence parsing
        var l_node = l;
        while (true) {
            const prec = Parser.precedence(p.token_tags[p.index]);
            if (prec < expr_precedence) {
                return l_node;
            }

            const op_token = try p.expectToken(p.token_tags[p.index]);
            var r_node = try p.parsePrimaryExpr();

            const next_prec = Parser.precedence(p.token_tags[p.index]);
            if (prec < next_prec) {
                r_node = try p.parseBinRExpr(r_node, prec + 1);
            }

            l_node = try p.addNode(.{
                .main_token = op_token,
                .data = .{
                    .binary_expr = .{
                        .left = l_node,
                        .right = r_node,
                    },
                },
            });
        }
    }

    fn expectVarExpr(p: *Parser) !Node.Index {
        // parses variable identifier expressions (variable value)
        const ident_token = try p.expectToken(.ident);
        return p.addNode(.{ .main_token = ident_token, .data = .{
            .var_expr = {},
        } });
    }

    fn expectArgList(p: *Parser) !Node.ExtraRange {
        _ = try p.expectToken(.l_paren);

        // since each argument may create arbitrarily many nodes
        // (arguments can be inline exprsesions),
        // we collect the toplevel argument indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eatToken(.r_paren)) |_| break;
            const arg_node = try p.expectExpr();
            try p.scratch.append(arg_node);
            switch (p.token_tags[p.index]) {
                .comma => _ = p.eatToken(.comma),
                .r_paren => {},
                else => return Error.UnexpectedToken,
            }
        }

        const params = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, params);
        return Node.ExtraRange{
            .start = @intCast(extra_top),
            .end = @intCast(p.extra.items.len),
        };
    }

    fn expectType(p: *Parser) !Node.Index {
        // parses a type as either an named identifier (u32, Point),
        // function prototype, or aggregate prototype
        var inner = switch (p.token_tags[p.index]) {
            // .k_struct => p.parseStructProto(),
            // .k_fn => p.expectFnProto(),
            .ident => node: {
                const ident_token = try p.expectToken(.ident);
                break :node try p.addNode(.{
                    .main_token = ident_token,
                    .data = .{
                        .named_ty = {},
                    },
                });
            },
            .l_paren => node: {
                // parentheses are used only for grouping in source code,
                // and don't generate ast nodes since the ast nesting itself
                // provides the correct grouping
                _ = try p.expectToken(.l_paren);
                const inner_node = try p.expectType();
                _ = try p.expectToken(.r_paren);

                break :node inner_node;
            },
            else => {
                std.debug.print("{}\n", .{p.token_tags[p.index]});
                return Error.UnexpectedToken;
            },
        };

        while (true) {
            inner = switch (p.token_tags[p.index]) {
                .asterisk => node: {
                    const asterisk_token = p.eatToken(.asterisk).?;
                    break :node try p.addNode(.{
                        .main_token = asterisk_token,
                        .data = .{ .pointer_ty = inner },
                    });
                },
                else => return inner,
            };
        }
    }

    fn expectFnDecl(p: *Parser) !Node.Index {
        const fn_token = try p.expectToken(.k_fn);
        const params = try p.expectParamList();
        // const return_ty = try p.expectType();
        if (p.expectType()) |return_ty| {
            // const body = try p.expectBlock();
            if (p.expectBlock()) |body| {
                return p.addNode(.{
                    .main_token = fn_token,
                    .data = .{ .fn_decl = .{
                        .signature = try p.addExtra(Node.FnSignature{
                            .params_start = params.start,
                            .params_end = params.end,
                            .return_ty = return_ty,
                        }),
                        .body = body,
                    } },
                });
            } else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_fn_brace, .token = p.index });
                    // TODO: revisit this, could be a better solution to
                    try consumeUntilValidTLD(p, true);
                    return Error.HandledUserError;
                },
                else => return err,
            }
        } else |err| switch (err) {
            Error.UnexpectedToken => {
                try p.errors.append(.{ .tag = .missing_return_type, .token = p.index });
                // consume leftover block
                if (p.expectBlock()) |_| {} else |block_err| {
                    switch (block_err) {
                        Error.UnexpectedToken => {},
                        else => return block_err,
                    }
                }
                return Error.HandledUserError;
            },
            else => return err,
        }
    }

    fn expectParamList(p: *Parser) !Node.ExtraRange {
        _ = try p.expectToken(.l_paren);

        // since each parameter may create multiple nodes (depending on type complexity)
        // we collect the toplevel parameter indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eatToken(.r_paren)) |_| break;

            if (p.expectParam()) |param_node| {
                try p.scratch.append(param_node);
                switch (p.token_tags[p.index]) {
                    .comma => _ = p.eatToken(.comma),
                    .r_paren => {},
                    else => return Error.UnexpectedToken,
                }
            } else |err| {
                switch (err) {
                    Error.HandledUserError => {
                        // eat until we're just left with an closing parenthesis or another comma to try and parse that arg
                        while (true) {
                            switch (p.token_tags[p.index]) {
                                .comma => {
                                    _ = p.eatToken(.comma);
                                    break;
                                },
                                .r_paren => break,
                                else => return Error.UnexpectedToken,
                            }
                            p.index += 1;
                        }
                        continue;
                    },
                    else => return err,
                }
            }
        }

        const params = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, params);
        return Node.ExtraRange{
            .start = @intCast(extra_top),
            .end = @intCast(p.extra.items.len),
        };
    }

    fn expectParam(p: *Parser) !Node.Index {
        // const ident_token = try p.expectToken(.ident);

        if (p.expectToken(.ident)) |ident_token| {
            // if (p.expectToken(.colon)) |_| {} else {
            _ = p.expectToken(.colon) catch {
                // We're missing a colon flag it
                try p.errors.append(.{ .tag = .missing_colon, .token = p.index });
            };

            if (p.expectType()) |type_node| {
                return p.addNode(.{
                    .main_token = ident_token,
                    .data = .{
                        .param = .{
                            .ty = type_node,
                        },
                    },
                });
            } else |err| {
                switch (err) {
                    Error.UnexpectedToken => {
                        // missing the type annotation which means we can't generate the node!
                        try p.errors.append(.{ .tag = .missing_type_annotation, .token = p.index });
                        return Error.HandledUserError;
                    },
                    else => {
                        return err;
                    },
                }

                return err;
            }
        } else |err| {
            switch (err) {
                Error.UnexpectedToken => {
                    // We're missing an identifier flag it
                    try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });

                    // if (p.expectToken(.colon)) |_| {} else {
                    _ = p.expectToken(.colon) catch {
                        // We're missing a colon flag it
                        try p.errors.append(.{ .tag = .missing_colon, .token = p.index });
                    };

                    // if (p.expectType()) |_| {} else {
                    _ = p.expectType() catch {
                        // We're missing a type annotation
                        try p.errors.append(.{ .tag = .missing_type_annotation, .token = p.index });
                    };

                    return Error.HandledUserError;
                },
                else => return err,
            }
        }
    }

    fn parseTypeDecl(self: *Parser) !Node.Index {
        // parses a type declaration (type Point = ...)
        const type_token = try self.expectToken(.k_type);
        _ = try self.expectToken(.ident); // not stored, (main_token == type_token) + 1
        _ = try self.expectToken(.equal);
        const type_node = try self.expectType();

        return self.addNode(.{
            .main_token = type_token,
            .data = .{
                .ty_decl = .{
                    .ty = type_node,
                },
            },
        });
    }

    fn expectBlock(p: *Parser) !Node.Index {
        const l_brace_token = try p.expectToken(.l_brace);

        // since each block may create an arbitrary number of statements,
        // we collect the toplevel statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eatToken(.r_brace)) |_| break;
            // const stmt_node = try p.parseStatement();
            if (p.parseStatement()) |stmt_node| {
                try p.scratch.append(stmt_node);
            } else |err| switch (err) {
                Error.HandledUserError => continue,
                else => return err,
            }
        }

        const stmts = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, stmts);

        return p.addNode(.{
            .main_token = l_brace_token,
            .data = .{
                .block = .{
                    .stmts_start = @intCast(extra_top),
                    .stmts_end = @intCast(p.extra.items.len),
                },
            },
        });
    }

    fn parseStatement(p: *Parser) Error!Node.Index {
        const node = switch (p.token_tags[p.index]) {
            .k_let => p.parseDecl(),
            .k_return => p.expectReturnStmt(),
            .k_if => p.parseConditional(),
            .k_for => p.parseLoop(),
            .k_break => p.expectBreak(),
            else => node: {
                const expr = try p.expectExpr();
                break :node switch (p.token_tags[p.index]) {
                    // function calls can exist on their own (implicit discard of return value)
                    .l_paren => try p.expectCall(expr),
                    // everything else should be an assignment using expr as an lvalue
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
                    => p.parseAssignment(expr),
                    else => {
                        try p.errors.append(.{ .tag = .unexpected_identifier, .token = p.index });

                        // try eating until we get a semicolon
                        try consumeUntilSemi(p);

                        return Error.HandledUserError;
                    },
                };
            },
        };

        _ = p.eatToken(.semi);
        return node;
    }

    fn expectCall(p: *Parser, ptr: Node.Index) !Node.Index {
        const args_range = try p.expectArgList();

        return p.addNode(.{
            .main_token = undefined,
            .data = .{
                .call_expr = .{
                    .ptr = ptr,
                    .args_start = args_range.start,
                    .args_end = args_range.end,
                },
            },
        });
    }

    fn parseAssignment(p: *Parser, ptr: Node.Index) !Node.Index {
        switch (p.token_tags[p.index]) {
            .equal => {
                const equal_token = try p.expectToken(.equal);
                const val = try p.expectExpr();
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
                const operator_token = p.eatToken(p.token_tags[p.index]).?;
                const val = try p.expectExpr();
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

    fn parseDecl(p: *Parser) !Node.Index {
        const let_token = try p.expectToken(.k_let);
        if (p.token_tags[p.index] == .k_mut) {
            _ = p.eatToken(.k_mut);

            if (p.expectToken(.ident)) |_| {} else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
                },
                else => return err,
            }

            const ty = if (p.eatToken(.colon) == null) 0 else try p.expectType();
            if (p.expectToken(.equal)) |_| {} else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_equals, .token = p.index });
                },
                else => return err,
            }

            if (p.expectExpr()) |val| {
                return p.addNode(.{
                    .main_token = let_token,
                    .data = .{ .var_decl = .{ .ty = ty, .val = val } },
                });
            } else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
                    try consumeUntilSemi(p);
                    return Error.HandledUserError;
                },
                else => return err,
            }
        } else {
            // _ = try p.expectToken(.ident);
            if (p.expectToken(.ident)) |_| {} else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_identifier, .token = p.index });
                },
                else => return err,
            }

            const ty = if (p.eatToken(.colon) == null) 0 else try p.expectType();
            if (p.expectToken(.equal)) |_| {} else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_equals, .token = p.index });
                },
                else => return err,
            }

            if (p.expectExpr()) |val| {
                if (p.attributes.items.len > 0) {
                    const attrs_start: u32 = @intCast(p.extra.items.len);
                    try p.extra.appendSlice(p.gpa, p.attributes.items);
                    const attrs_end: u32 = @intCast(p.extra.items.len);

                    const data = try p.addExtra(Node.DeclMetadata{
                        .ty = ty,
                        .attrs_start = attrs_start,
                        .attrs_end = attrs_end,
                    });
                    // p.attributes.clearRetainingCapacity();
                    return p.addNode(.{
                        .main_token = let_token,
                        .data = .{ .const_decl_attr = .{ .metadata = data, .val = val } },
                    });
                } else {
                    return p.addNode(.{
                        .main_token = let_token,
                        .data = .{ .const_decl = .{ .ty = ty, .val = val } },
                    });
                }
            } else |err| switch (err) {
                Error.UnexpectedToken => {
                    try p.errors.append(.{ .tag = .missing_expression, .token = p.index });
                    try consumeUntilSemi(p);
                    return Error.HandledUserError;
                },
                else => return err,
            }
        }
    }

    fn expectReturnStmt(p: *Parser) !Node.Index {
        const ret_token = try p.expectToken(.k_return);

        if (p.token_tags[p.index] == .semi) {
            // no return value, assumed void function (will verify in IR during type checking)
            return p.addNode(.{
                .main_token = ret_token,
                .data = .{
                    .return_val = .{
                        .val = null_node,
                    },
                },
            });
        } else {
            const expr_node = try p.expectExpr();
            return p.addNode(.{
                .main_token = ret_token,
                .data = .{
                    .return_val = .{
                        .val = expr_node,
                    },
                },
            });
        }
    }

    fn parseConditional(p: *Parser) !Node.Index {
        // all conditional branches start with the if keyword
        const if_token = try p.expectToken(.k_if);

        // we have three kinds of if statements: simple, else, and chain
        // which we progressively try to match against
        const condition = try p.expectExpr();
        const exec_true = try p.expectBlock();

        if (p.eatToken(.k_else)) |_| {
            if (p.token_tags[p.index] == .k_if) {
                // chained if
                const next = try p.parseConditional();
                const chain = try p.addExtra(Node.IfChain{
                    .exec_true = exec_true,
                    .next = next,
                });
                return p.addNode(.{
                    .main_token = if_token,
                    .data = .{ .if_chain = .{
                        .condition = condition,
                        .chain = chain,
                    } },
                });
            } else {
                // if else
                const exec_false = try p.expectBlock();
                const exec = try p.addExtra(Node.IfElse{
                    .exec_true = exec_true,
                    .exec_false = exec_false,
                });
                return p.addNode(.{ .main_token = if_token, .data = .{ .if_else = .{
                    .condition = condition,
                    .exec = exec,
                } } });
            }
        } else {
            // simple if
            return p.addNode(.{
                .main_token = if_token,
                .data = .{ .if_simple = .{
                    .condition = condition,
                    .exec_true = exec_true,
                } },
            });
        }
    }

    fn parseLoop(p: *Parser) !Node.Index {
        // all loops start with the for keyword
        const for_token = try p.expectToken(.k_for);

        // we have three kinds of loops: forever, conditional, range
        // which we progressively try to match against
        if (p.token_tags[p.index] == .l_brace) {
            // forever loop
            const body = try p.expectBlock();
            return p.addNode(.{
                .main_token = for_token,
                .data = .{ .loop_forever = .{ .body = body } },
            });
        } else if (p.token_tags[p.index] == .k_let) {
            // declaration = assume this is the binding, and we are in a range loop
            const binding = try p.parseDecl();
            _ = try p.expectToken(.semi);
            const condition = try p.expectExpr();
            _ = try p.expectToken(.semi);
            const afterthought = try p.parseStatement();
            const signature = try p.addExtra(Node.RangeSignature{
                .binding = binding,
                .condition = condition,
                .afterthought = afterthought,
            });

            const body = try p.expectBlock();
            return p.addNode(.{
                .main_token = for_token,
                .data = .{ .loop_range = .{
                    .signature = signature,
                    .body = body,
                } },
            });
        } else {
            // assume this is the condition of a conditional loop
            const condition = try p.expectExpr();
            const body = try p.expectBlock();
            return p.addNode(.{ .main_token = for_token, .data = .{ .loop_conditional = .{
                .condition = condition,
                .body = body,
            } } });
        }
    }

    fn expectBreak(p: *Parser) !Node.Index {
        const break_token = try p.expectToken(.k_break);
        return p.addNode(.{
            .main_token = break_token,
            .data = .{ .loop_break = {} },
        });
    }

    fn parseExport(p: *Parser) !void {
        const export_token = try p.expectToken(.a_export);

        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);
        if (p.token_tags[p.index] == .l_paren) {
            _ = try p.expectToken(.l_paren);
            while (true) {
                switch (p.token_tags[p.index]) {
                    .r_paren => {
                        _ = try p.expectToken(.r_paren);
                        break;
                    },
                    else => {
                        const arg = try p.expectExpr();
                        try p.scratch.append(arg);
                    },
                }
            }
        }
        const args = p.scratch.items[scratch_top..];
        const args_start: u32 = @intCast(p.extra.items.len);
        try p.extra.appendSlice(p.gpa, args);
        const args_end: u32 = @intCast(p.extra.items.len);

        const attribute = try p.addNode(.{ .main_token = export_token, .data = .{
            .attribute = .{
                .args_start = args_start,
                .args_end = args_end,
            },
        } });
        try p.attributes.append(p.gpa, attribute);
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
