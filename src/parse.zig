const std = @import("std");
const lex = @import("lex.zig");
const Ast = @import("Ast.zig");

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Lexer = lex.Lexer;
const Node = Ast.Node;
const TokenIndex = Ast.TokenIndex;

pub const Error = error { UnexpectedToken } || Allocator.Error;
const null_node: Node.Index = 0;

// parses a string of source characters into an abstract syntax tree
// gpa: allocator for tree data that outlives this function call
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Ast {
    var tokens = Ast.TokenList{};
    defer tokens.deinit(gpa);

    // lex entire source file into token list
    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.next();
        try tokens.append(gpa, .{
            .tag = token.tag,
            .start = @intCast(u32, token.loc.start),
        });
        if (token.tag == .eof) break;
    }

    // initialize parser
    var parser = Parser.init(gpa, source, &tokens);
    defer parser.nodes.deinit(gpa);
    defer parser.extra.deinit(gpa);

    _ = try parser.addNode(.{
        .main_token = 0,
        .data = .{ .placeholder = {} },
    });
    _ = try parser.parseModule();

    // copy parser results into an abstract syntax tree
    // that owns the source, token list, node list, and node extra data
    return Ast {
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = parser.extra.toOwnedSlice(gpa),
        .errors = &.{},
    };
}

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    token_starts: []const u32,
    index: u32,

    nodes: std.MultiArrayList(Node),
    extra: std.ArrayListUnmanaged(Node.Index),
    scratch: std.ArrayList(Node.Index),
    attributes: std.ArrayListUnmanaged(Node.Index),
    // errors: std.ArrayListUnmanaged(Ast.Error),

    pub fn init(gpa: Allocator, source: []const u8, tokens: *Ast.TokenList) Parser {
        return .{
            .source = source,
            .gpa = gpa,
            .token_tags = tokens.items(.tag),
            .token_starts = tokens.items(.start),
            .index = 0,
            .nodes = .{},
            .extra = .{},
            .scratch = std.ArrayList(Node.Index).init(gpa),
            .attributes = .{},
        };
    }

    fn addNode(p: *Parser, node: Node) !Node.Index {
        const result = @intCast(Node.Index, p.nodes.len);
        try p.nodes.append(p.gpa, node);
        return result;
    }

    fn setNode(p: *Parser, i: usize, node: Node) Node.Index {
        p.nodes.set(i, node);
        return @intCast(Node.Index, i);
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
        const len = @intCast(u32, p.extra.items.len);
        inline for (fields) |field| {
            comptime std.debug.assert(field.field_type == Node.Index);
            p.extra.appendAssumeCapacity(@field(extra, field.name));
        }
        return len;
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
            .pipe_pipe => 20,
            .ampersand_ampersand => 30,
            .caret_caret => 40,
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
                .k_let => try p.parseDecl(),
                .a_export => { 
                    try p.parseExport();
                    continue;
                },
                else => {
                    std.debug.print("{}\n", .{p.token_tags[p.index]});
                    unreachable;
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
                    .stmts_start = @intCast(Node.ExtraIndex, extra_top),
                    .stmts_end = @intCast(Node.ExtraIndex, p.extra.items.len),
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
                .l_paren => p.expectCall(),
                else => p.expectVarExpr(),
            },
            .int_lit => p.addNode(.{
                .main_token = try p.expectToken(.int_lit),
                .data = .{
                    .integer_literal = {},
                },
            }),
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
        return p.addNode(.{
            .main_token = ident_token,
            .data = .{
                .var_expr = {},
            }
        });
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
        return Node.ExtraRange {
            .start = @intCast(Node.ExtraIndex, extra_top),
            .end = @intCast(Node.ExtraIndex, p.extra.items.len),
        };
    }


    fn expectType(p: *Parser) !Node.Index {
        // parses a type as either an named identifier (u32, Point),
        // function prototype, or aggregate prototype
        return switch (p.token_tags[p.index]) {
            // .k_struct => p.parseStructProto(),
            // .k_fn => p.expectFnProto(),
            .ident => {
                const ident_token = try p.expectToken(.ident);
                return p.addNode(.{
                    .main_token = ident_token,
                    .data = .{
                        .named_ty = {},
                    },
                });
            },
            else => {
                std.debug.print("{}\n", .{p.token_tags[p.index]});
                return Error.UnexpectedToken;
            },
        };
    }

    fn expectFnDecl(p: *Parser) !Node.Index {
        const fn_token = try p.expectToken(.k_fn);
        const params = try p.expectParamList();
        const return_ty = try p.expectType();
        const body = try p.expectBlock();

        return p.addNode(.{
            .main_token = fn_token,
            .data = .{
                .fn_decl = .{
                    .signature = try p.addExtra(Node.FnSignature {
                        .params_start = params.start,
                        .params_end = params.end,
                        .return_ty = return_ty,
                    }),
                    .body = body,
                }
            },
        });
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
            const param_node = try p.expectParam();
            try p.scratch.append(param_node);
            switch (p.token_tags[p.index]) {
                .comma => _ = p.eatToken(.comma),
                .r_paren => {},
                else => return Error.UnexpectedToken,
            }
        }

        const params = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, params);
        return Node.ExtraRange {
            .start = @intCast(Node.ExtraIndex, extra_top),
            .end = @intCast(Node.ExtraIndex, p.extra.items.len),
        };
    }

    fn expectParam(p: *Parser) !Node.Index {
        const ident_token = try p.expectToken(.ident);
        _ = try p.expectToken(.colon);
        
        const type_node = try p.expectType();
        return p.addNode(.{
            .main_token = ident_token,
            .data = .{
                .param = .{
                    .ty = type_node,
                },
            },
        });
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
            const stmt_node = try p.parseStatement();
            try p.scratch.append(stmt_node);
        }

        const stmts = p.scratch.items[scratch_top..];
        const extra_top = p.extra.items.len;
        try p.extra.appendSlice(p.gpa, stmts);

        return p.addNode(.{
            .main_token = l_brace_token,
            .data = .{
                .block = .{
                    .stmts_start = @intCast(Node.ExtraIndex, extra_top),
                    .stmts_end = @intCast(Node.ExtraIndex, p.extra.items.len),
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
            .ident => switch (p.token_tags[p.index + 1]) {
                .l_paren => p.expectCall(),
                .equal,
                .plus_equal, .minus_equal,
                .asterisk_equal, .slash_equal, .percent_equal,
                .ampersand_equal, .pipe_equal, .caret_equal,
                .l_angle_l_angle_equal,
                .r_angle_r_angle_equal => p.parseAssignment(),
                else => return Error.UnexpectedToken,
            },
            .k_break => p.expectBreak(),
            else => {
                std.debug.print("{}\n", .{p.token_tags[p.index]});
                return Error.UnexpectedToken;
            },
        };

        _ = p.eatToken(.semi);
        return node;
    }

    fn expectCall(p: *Parser) !Node.Index {
        const ident_token = try p.expectToken(.ident);
        const args_range = try p.expectArgList();

        return p.addNode(.{
            .main_token = ident_token,
            .data = .{
                .call_expr = .{
                    .args_start = args_range.start,
                    .args_end = args_range.end,
                },
            },
        });
    }

    fn parseAssignment(p: *Parser) !Node.Index {
        const ident_token = try p.expectToken(.ident);

        if (p.token_tags[p.index] == .equal) {
            _ = try p.expectToken(.equal);
            const value = try p.expectExpr();

            return p.addNode(.{
                .main_token = ident_token,
                .data = .{ .assign_simple = .{ .val = value } },
            });
        } else {
            _ = p.eatToken(p.token_tags[p.index]);
            const value = try p.expectExpr();

            return p.addNode(.{
                .main_token = ident_token,
                .data = .{ .assign_binary = .{ .val = value } },
            });
        }
    }

    fn parseDecl(p: *Parser) !Node.Index {
        const let_token = try p.expectToken(.k_let);
        if (p.token_tags[p.index] == .k_mut) {
            _ = p.eatToken(.k_mut);
            _ = try p.expectToken(.ident);

            const ty = if (p.eatToken(.colon) == null) 0 else try p.expectType();
            _ = try p.expectToken(.equal);

            const val = try p.expectExpr();
            return p.addNode(.{
                .main_token = let_token,
                .data = .{ .var_decl = .{ .ty = ty, .val = val } },
            });
        } else {
            _ = try p.expectToken(.ident);

            const ty = if (p.eatToken(.colon) == null) 0 else try p.expectType();
            _ = try p.expectToken(.equal);

            const val = try p.expectExpr();
            if (p.attributes.items.len > 0) {
                const attrs_start = @intCast(u32, p.extra.items.len);
                try p.extra.appendSlice(p.gpa, p.attributes.items);
                const attrs_end = @intCast(u32, p.extra.items.len);

                const data = try p.addExtra(Node.DeclMetadata {
                    .ty = ty,
                    .attrs_start = attrs_start,
                    .attrs_end = attrs_end,
                });
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
                const chain = try p.addExtra(Node.IfChain {
                    .exec_true = exec_true,
                    .next = next,
                });
                return p.addNode(.{
                    .main_token = if_token,
                    .data = .{
                        .if_chain = .{
                            .condition = condition,
                            .chain = chain,
                        }
                    },
                });
            } else {
                // if else
                const exec_false = try p.expectBlock();
                const exec = try p.addExtra(Node.IfElse {
                    .exec_true = exec_true,
                    .exec_false = exec_false,
                });
                return p.addNode(.{
                    .main_token = if_token,
                    .data = .{
                        .if_else = .{
                            .condition = condition,
                            .exec = exec,
                        }
                    }
                });
            }
        } else {
            // simple if
            return p.addNode(.{
                .main_token = if_token,
                .data = .{
                    .if_simple = .{
                        .condition = condition,
                        .exec_true = exec_true,
                    }
                },
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
            const signature = try p.addExtra(Node.RangeSignature {
                .binding = binding,
                .condition = condition,
                .afterthought = afterthought,
            });

            const body = try p.expectBlock();
            return p.addNode(.{
                .main_token = for_token,
                .data = .{
                    .loop_range = .{
                        .signature = signature,
                        .body = body,
                    }
                },
            });
        } else {
            // assume this is the condition of a conditional loop
            const condition = try p.expectExpr();
            const body = try p.expectBlock();
            return p.addNode(.{
                .main_token = for_token,
                .data = .{
                    .loop_conditional = .{
                        .condition = condition,
                        .body = body,
                    }
                }
            });
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
        const args_start = @intCast(u32, p.extra.items.len);
        try p.extra.appendSlice(p.gpa, args);
        const args_end = @intCast(u32, p.extra.items.len);

        const attribute = try p.addNode(.{
            .main_token = export_token,
            .data = .{
                .attribute = .{
                    .args_start = args_start,
                    .args_end = args_end,
                },
            }
        });
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
// }
