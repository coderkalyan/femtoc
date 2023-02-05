const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Lexer = lex.Lexer;
const Node = ast.Node;
const Ast = ast.Ast;
const TokenIndex = ast.TokenIndex;

pub const Error = error { UnexpectedToken } || Allocator.Error;
const null_node: Node.Index = 0;

// parses a string of source characters into an abstract syntax tree
// gpa: allocator for tree data that outlives this function call
pub fn parse(gpa: Allocator, source: [:0]const u8) Error!Ast {
    var tokens = ast.TokenList{};
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
    defer parser.extra_data.deinit();

    _ = try parser.parseToplevel();

    // copy parser results into an abstract syntax tree
    // that owns the source, token list, node list, and node extra data
    return Ast {
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = parser.extra_data.toOwnedSlice(),
    };
}

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    token_starts: []const u32,
    index: u32,

    nodes: std.MultiArrayList(Node),
    extra_data: std.ArrayList(Node.Index),
    scratch: std.ArrayList(Node.Index),
    // errors: std.ArrayListUnmanaged(Ast.Error),

    pub fn init(gpa: Allocator, source: []const u8, tokens: *ast.TokenList) Parser {
        return .{
            .source = source,
            .gpa = gpa,
            .token_tags = tokens.items(.tag),
            .token_starts = tokens.items(.start),
            .index = 0,
            .nodes = .{},
            .extra_data = std.ArrayList(Node.Index).init(gpa),
            .scratch = std.ArrayList(Node.Index).init(gpa),
        };
    }

    fn addNode(self: *Parser, node: Node) !Node.Index {
        const result = @intCast(Node.Index, self.nodes.len);
        try self.nodes.append(self.gpa, node);
        return result;
    }

    fn setNode(self: *Parser, i: usize, node: Node) Node.Index {
        self.nodes.set(i, node);
        return @intCast(Node.Index, i);
    }

    fn reserveNode(self: *Parser, tag: ast.Node.Tag) !usize {
        try self.nodes.resize(self.gpa, self.nodes.len + 1);
        self.nodes.items(.tag)[self.nodes.len - 1] = tag;
        return self.nodes.len - 1;
    }

    fn eatToken(self: *Parser, tag: Token.Tag) ?TokenIndex {
        if (self.token_tags[self.index] == tag) {
            self.index += 1;
            return self.index - 1;
        } else {
            return null;
        }
    }

    fn expectToken(self: *Parser, tag: Token.Tag) Error!TokenIndex {
        if (self.eatToken(tag)) |token| {
            return token;
        } else {
            return Error.UnexpectedToken;
        }
    }

    fn addExtra(self: *Parser, extra: anytype) Allocator.Error!Node.Index {
        const fields = std.meta.fields(@TypeOf(extra));
        try self.extra_data.ensureUnusedCapacity(fields.len);
        const len = @intCast(u32, self.extra_data.items.len);
        inline for (fields) |field| {
            comptime std.debug.assert(field.field_type == Node.Index);
            self.extra_data.appendAssumeCapacity(@field(extra, field.name));
        }
        return len;
    }

    fn precedence(tag: Token.Tag) i32 {
        return switch (tag) {
            .equal_equal => 10,
            .pipe_pipe => 20,
            .ampersand_ampersand => 30,
            .caret_caret => 40,
            .plus => 110,
            .minus => 120,
            .asterisk => 130,
            .slash => 140,
            else => -1,
        };
    }

    pub fn parseToplevel(p: *Parser) !Node.Index {
        // each toplevel (file) may create any number of global statements
        // so we collect the statement indices in the scratch list,
        // append all of them to extra_data at the end, and return the
        // range in extra_data containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            const node = switch (p.token_tags[p.index]) {
                .eof => break,
                .k_let => try p.parseDecl(),
                else => unreachable,
            };
            try p.scratch.append(node);
        }

        const stmts = p.scratch.items[scratch_top..];
        const extra_top = p.extra_data.items.len;
        try p.extra_data.appendSlice(stmts);

        return p.addNode(.{
            .main_token = 0,
            .data = .{
                .toplevel = .{
                    .stmts_start = @intCast(Node.ExtraIndex, extra_top),
                    .stmts_end = @intCast(Node.ExtraIndex, p.extra_data.items.len),
                },
            },
        });
    }

    fn expectExpr(p: *Parser) !Node.Index {
        // declarations that aren't part of logical or arithmetic expressions,
        // like string literals, function and aggregate declarations, etc,
        // are parsed and returned here directly
        // for everything else, we part in expectPrimaryExpr() and try to
        // associate it with a binary companion (parseBinRExp())
        return switch (p.token_tags[p.index]) {
            .k_fn => p.expectFnDecl(),
            else => {
                const left_node = try p.expectPrimaryExpr();
                return p.parseBinRExpr(left_node, 0);
            },
        };
    }

    fn expectPrimaryExpr(p: *Parser) Error!Node.Index {
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
            .ident => p.expectIdentExpr(),      // handles variables and calls (starts with ident)
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
            var r_node = try p.expectPrimaryExpr();

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

    fn expectIdentExpr(p: *Parser) !Node.Index {
        // parses variable identifier expressions (variable value)
        // and function calls
        const ident_token = try p.expectToken(.ident);
        if (p.token_tags[p.index] == .l_paren) {
            // function call
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
        } else {
            // variable value
            return p.addNode(.{
                .main_token = ident_token,
                .data = .{
                    .var_expr = {},
                }
            });
        }
    }

    fn expectArgList(p: *Parser) !Node.ExtraRange {
        _ = try p.expectToken(.l_paren);

        // since each argument may create arbitrarily many nodes
        // (arguments can be inline exprsesions),
        // we collect the toplevel argument indices in the scratch list,
        // append all of them to extra_data at the end, and return the
        // range in extra_data containing those indices
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
        const extra_top = p.extra_data.items.len;
        try p.extra_data.appendSlice(params);
        return Node.ExtraRange {
            .start = @intCast(Node.ExtraIndex, extra_top),
            .end = @intCast(Node.ExtraIndex, p.extra_data.items.len),
        };
    }


    fn expectType(p: *Parser) !Node.Index {
        // parses a type as either an named identifier (u32, Point),
        // function prototype, or aggregate prototype
        return switch (p.token_tags[p.index]) {
            // .k_struct => p.parseStructProto(),
            .k_fn => p.expectFnProto(),
            .ident => {
                const ident_token = try p.expectToken(.ident);
                return p.addNode(.{
                    .main_token = ident_token,
                    .data = .{
                        .named_ty = {},
                    },
                });
            },
            else => Error.UnexpectedToken,
        };
    }

    // fn parseStructProto(self: *Parser) !Node.Index {
    //     const struct_token = self.eatToken(.k_struct) orelse return null_node;
    //     _ = try self.expectToken(.l_brace);
        // const members = try self.parseStructMembers();

    //     return self.addNode(.{
    //         .tag = .struct_proto,
    //         .main_token = struct_token,
    //         .data = .{
    //             // .l = members,
    //             .l = undefined,
    //             .r = undefined,
    //         }
    //     });
    // }

    fn expectFnDecl(p: *Parser) !Node.Index {
        const proto_node = try p.expectFnProto();
        const body_node = try p.expectBlock();

        return p.addNode(.{
            .main_token = 0,            // no main token
            .data = .{
                .fn_decl = .{
                    .proto = proto_node,
                    .body = body_node,
                },
            },
        });
    }

    fn expectFnProto(p: *Parser) !Node.Index {
        const fn_token = try p.expectToken(.k_fn);
        const params_range = try p.expectParamList();
        const return_type_node = try p.expectType();

        return p.addNode(.{
            .main_token = fn_token,
            .data = .{
                .fn_proto = .{
                    .params = try p.addExtra(Node.CallSignature {
                        .params_start = params_range.start,
                        .params_end = params_range.end,
                    }),
                    .return_ty = return_type_node,
                },
            },
        });
    }

    fn expectParamList(p: *Parser) !Node.ExtraRange {
        _ = try p.expectToken(.l_paren);

        // since each parameter may create multiple nodes (depending on type complexity)
        // we collect the toplevel parameter indices in the scratch list,
        // append all of them to extra_data at the end, and return the
        // range in extra_data containing those indices
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
        const extra_top = p.extra_data.items.len;
        try p.extra_data.appendSlice(params);
        return Node.ExtraRange {
            .start = @intCast(Node.ExtraIndex, extra_top),
            .end = @intCast(Node.ExtraIndex, p.extra_data.items.len),
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
        // append all of them to extra_data at the end, and return the
        // range in extra_data containing those indices
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eatToken(.r_brace)) |_| break;
            const stmt_node = try p.expectStmt();
            try p.scratch.append(stmt_node);
        }

        const stmts = p.scratch.items[scratch_top..];
        const extra_top = p.extra_data.items.len;
        try p.extra_data.appendSlice(stmts);

        return p.addNode(.{
            .main_token = l_brace_token,
            .data = .{
                .block = .{
                    .stmts_start = @intCast(Node.ExtraIndex, extra_top),
                    .stmts_end = @intCast(Node.ExtraIndex, p.extra_data.items.len),
                },
            },
        });
    }

    fn expectStmt(p: *Parser) Error!Node.Index {
        const node = switch (p.token_tags[p.index]) {
            .k_let => p.parseDecl(),
            .k_return => p.expectReturnStmt(),
            .k_if => p.expectIfStmt(),
            else => null_node,
        };

        _ = p.eatToken(.semi);
        return node;
    }

    fn parseDecl(p: *Parser) !Node.Index {
        const let_token = try p.expectToken(.k_let);
        const mut_token = p.eatToken(.k_mut);
        _ = p.eatToken(.ident);

        const type_node = if (p.eatToken(.colon) == null) null else try p.expectType();
        _ = try p.expectToken(.equal);
        const value_node = try p.expectExpr();

        // instead of storing the mutability token,
        // create separate tags for constant and variable declarations
        if (mut_token) |_| {
            return p.addNode(.{
                .main_token = let_token,
                .data = .{
                    .var_decl = .{
                        .ty = type_node orelse 0,
                        .val = value_node,
                    },
                },
            });
        } else {
            return p.addNode(.{
                .main_token = let_token,
                .data = .{
                    .const_decl = .{
                        .ty = type_node orelse 0,
                        .val = value_node,
                    },
                },
            });
        }
    }

    fn expectReturnStmt(p: *Parser) !Node.Index {
        const ret_token = try p.expectToken(.k_return);

        if (p.token_tags[p.index] == .semi) {
            // no return value, assumed void function (will verify in IR during type checking)
            return p.addNode(.{
                .main_token = ret_token,
                .data = .{
                    .return_void = {},
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

    fn expectIfStmt(p: *Parser) !Node.Index {
        const if_token = try p.expectToken(.k_if);
        const condition_node = try p.expectExpr();
        const body_node = try p.expectBlock();

        return p.addNode(.{
            .main_token = if_token,
            .data = .{
                .if_stmt = .{
                    .condition = condition_node,
                    .body = body_node,
                },
            },
        });
    }

    // fn parseUse(self: *Parser) !Node.Index {
    //     const use_token = try self.expectToken(.k_use);
    //     const scope = try self.expectScopeList();
    //
    //     const as_token = self.eatToken(.k_as) orelse 0;
    //     if (as_token != 0) _ = try self.expectToken(.ident);
    //
    //     return self.addNode(.{
    //         .tag = .use,
    //         .main_token = use_token,
    //         .data = .{
    //             .l = scope,
    //             .r = as_token,
    //         }
    //     });
    // }
    //
    // fn expectScopeList(p: *Parser) !Node.Range {
    //     const scratch_top = p.scratch.items.len;
    //     defer p.scratch.shrinkRetainingCapacity(scratch_top);
    //
    //     while (true) {
    //         _ = try p.expectToken(.ident);
    //         if (p.token_tags[p.index] == .colon_colon) {
    //             _ = p.eatToken(.colon_colon);
    //         } else break;
    //     }
    //
    //     const params = p.scratch.items[scratch_top..];
    //     try p.extra_data.appendSlice(p.gpa, params);
    //     return Node.Range {
    //         .start = @intCast(Node.Index, scratch_top),
    //         .end = @intCast(Node.Index, p.extra_data.items.len),
    //     };
    // }
};

// fn testParse(source: [:0]const u8, allocator: Allocator, anything_changed: *bool) ![]u8 {
//     const stderr = std.io.getStdErr().writer();
//
//     var tree = try parse(allocator, source);

    // for (tree.errors) |parse_error| {
    //
    // }
// }
