const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");

const Allocator = std.mem.Allocator;
const Token = lex.Token;
const Lexer = lex.Lexer;
const Node = ast.Node;
const Ast = ast.Ast;
const TokenIndex = ast.TokenIndex;

pub const Error = error { ParseError } || Allocator.Error;
const null_node: Node.Index = 0;

pub fn parse(gpa: Allocator, source: [:0]const u8) Allocator.Error!Ast {
    var tokens = ast.TokenList{};
    defer tokens.deinit(gpa);

    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.next();
        try tokens.append(gpa, .{
            .tag = token.tag,
            .start = @intCast(u32, token.loc.start),
        });
        if (token.tag == .eof) break;
    }

    // std.log.debug("{any}", .{tokens.items(.tag)});

    var parser = Parser {
        .source = source,
        .gpa = gpa,
        .token_tags = tokens.items(.tag),
        .token_starts = tokens.items(.start),
        .index = 0,
        .nodes = .{},
        .extra_data = .{},
        .scratch = .{},
    };
    defer parser.nodes.deinit(gpa);
    defer parser.extra_data.deinit(gpa);

    parser.parseRoot();

    return Ast {
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = parser.extra_data.toOwnedSlice(gpa),
    };
}

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    token_starts: []const u32,
    index: u32,

    nodes: std.MultiArrayList(Node),
    extra_data: std.ArrayListUnmanaged(Node.Index),
    scratch: std.ArrayListUnmanaged(Node.Index),
    // errors: std.ArrayListUnmanaged(Ast.Error),

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
            return Error.ParseError;
        }
    }

    fn addExtra(self: *Parser, extra: anytype) Allocator.Error!Node.Index {
        const fields = std.meta.fields(@TypeOf(extra));
        try self.extra_data.ensureUnusedCapacity(self.gpa, fields.len);
        const len = @intCast(u32, self.extra_data.items.len);
        inline for (fields) |field| {
            comptime std.debug.assert(field.field_type == Node.Index);
            self.extra_data.appendAssumeCapacity(@field(extra, field.name));
        }
        return len;
    }

    pub fn parseRoot(self: *Parser) void {
        while (true) {
            const tag = self.token_tags[self.index];
            const node: Node.Index = switch (tag) {
                // .k_use => self.parseUse() catch null_node,
                // .k_type => self.parseTypeDecl() catch null_node,
                .k_let => self.parseDecl() catch null_node,
                .eof => break,
                else => node: {
                    self.index += 1;
                    break :node null_node;
                },
            };

            _ = node;
        }
    }

    fn parseDecl(p: *Parser) !Node.Index {
        const global_node = try p.reserveNode(.const_decl);

        const let_token = p.eatToken(.k_let) orelse return null_node;
        if (p.token_tags[p.index] == .k_mut) p.index += 1;
        _ = try p.expectToken(.ident);
        const type_node: Node.Index = if (p.eatToken(.colon) == null) 0 else try p.expectTypeExpr();
        // TODO: extern symbols will only be declarations
        _ = try p.expectToken(.equal);
        const value_node = try p.expectExpr();

        return p.setNode(global_node, .{
            .tag = .const_decl,
            .main_token = let_token,
            .data = .{
                .l = type_node,
                .r = value_node,
            },
        });
    }

    fn precedence(tag: Token.Tag) i32 {
        return switch (tag) {
            .plus => 10,
            .minus => 20,
            .asterisk => 30,
            .slash => 40,
            else => -1,
        };
    }

    fn expectExpr(p: *Parser) !Node.Index {
        const tag = p.token_tags[p.index];
        return switch (tag) {
            .k_fn => p.expectFnDecl(),
            else => {
                const left_node = try p.expectPrimaryExpr();
                return p.parseBinRExpr(left_node, 0);
            },
        };
    }

    fn expectPrimaryExpr(p: *Parser) Error!Node.Index {
        const tag = p.token_tags[p.index];
        return switch (tag) {
            .l_paren => p.expectParenExpr(),
            .ident => p.expectIdentExpr(),
            else => p.addNode(.{
                .tag = switch (tag) {
                    .int_lit => .int_lit,
                    .float_lit => .float_lit,
                    else => return Error.ParseError,
                },
                .main_token = p.eatToken(tag) orelse unreachable,
                .data = undefined,
            }),
        };
    }

    fn expectParenExpr(p: *Parser) !Node.Index {
        _ = try p.expectToken(.l_paren);
        const value_node = p.expectExpr();
        _ = try p.expectToken(.r_paren);

        return value_node;
    }

    fn expectIdentExpr(p: *Parser) !Node.Index {
        const ident_token = try p.expectToken(.ident);
        if (p.token_tags[p.index] == .l_paren) {
            std.log.debug("call", .{});
            std.log.debug("reading param list", .{});
            const params = try p.expectParamList();
            std.log.debug("read param list", .{});
            return p.addNode(.{
                .tag = .call_expr,
                .main_token = ident_token,
                .data = .{
                    .l = params.start,
                    .r = params.end,
                },
            });
        } else {
            return p.addNode(.{
                .tag = .ident_expr,
                .main_token = ident_token,
                .data = undefined,
            });
        }
    }

    fn expectParamList(p: *Parser) !Node.Range {
        _ = try p.expectToken(.l_paren);
        const params_start = p.nodes.len;
        while (true) {
            if (p.eatToken(.r_paren)) |_| break;
            // std.log.debug("next token: {any}", .{p.token_tags[p.index - 1..p.index + 2]});
            _ = try p.expectExpr();
            switch (p.token_tags[p.index]) {
                .comma => _ = p.eatToken(.comma),
                .r_paren => {},
                else => return Error.ParseError,
            }
        }
        const params_end = p.nodes.len;

        return Node.Range {
            .start = @intCast(u32, params_start),
            .end = @intCast(u32, params_end),
        };
    }

    fn parseBinRExpr(p: *Parser, l: Node.Index, expr_precedence: i32) !Node.Index {
        var l_node = l;
        while (true) {
            const prec = Parser.precedence(p.token_tags[p.index]);
            if (prec < expr_precedence) {
                return l_node;
            }

            const op_token = p.eatToken(p.token_tags[p.index]) orelse unreachable;
            var r_node = try p.expectPrimaryExpr();

            const next_prec = Parser.precedence(p.token_tags[p.index]);
            if (prec < next_prec) {
                r_node = try p.parseBinRExpr(r_node, prec + 1);
            }

            l_node = try p.addNode(.{
                .tag = .bin_expr,
                .main_token = op_token,
                .data = .{
                    .l = l_node,
                    .r = r_node,
                },
            });
        }
    }

    fn parseTypeDecl(self: *Parser) !Node.Index {
        const type_token = try self.expectToken(.k_type);
        _ = try self.expectToken(.ident);
        _ = try self.expectToken(.equal);
        const type_expr = try self.expectTypeExpr();

        return self.addNode(.{
            .tag = .type_decl,
            .main_token = type_token,
            .data = .{
                .l = type_expr,
                .r = undefined,
            },
        });
    }

    fn expectTypeExpr(self: *Parser) !Node.Index {
        const tag = self.token_tags[self.index];
        return switch (tag) {
            .k_struct => self.parseStructProto(),
            .k_fn => self.expectFnProto(),
            .ident => {
                _ = self.eatToken(.ident);
                return null_node;
            },
            else => Error.ParseError,
        };
    }

    fn parseStructProto(self: *Parser) !Node.Index {
        const struct_token = self.eatToken(.k_struct) orelse return null_node;
        _ = try self.expectToken(.l_brace);
        // const members = try self.parseStructMembers();

        return self.addNode(.{
            .tag = .struct_proto,
            .main_token = struct_token,
            .data = .{
                // .l = members,
                .l = undefined,
                .r = undefined,
            }
        });
    }

    fn expectFnProto(p: *Parser) !Node.Index {
        const fn_token = p.eatToken(.k_fn) orelse return null_node;

        // fn proto node should before children in array
        const fn_proto_index = try p.reserveNode(.fn_proto);
        const params = try p.parseParamDeclList();
        const return_type = try p.expectTypeExpr();

        return p.setNode(fn_proto_index, .{
            .tag = .fn_proto,
            .main_token = fn_token,
            .data = .{
                .l = try p.addExtra(Node.FnProto {
                    .params_start = params.start,
                    .params_end = params.end,
                }),
                .r = return_type,
            },
        });
    }

    fn parseParamDeclList(p: *Parser) !Node.Range {
        _ = try p.expectToken(.l_paren);
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            if (p.eatToken(.r_paren)) |_| break;
            const param = try p.expectParamDecl();
            if (param != null_node) try p.scratch.append(p.gpa, param);
            switch (p.token_tags[p.index]) {
                .comma, => _ = p.eatToken(.comma),
                .r_paren => {},
                else => return Error.ParseError,
            }
        }

        const params = p.scratch.items[scratch_top..];
        try p.extra_data.appendSlice(p.gpa, params);
        return Node.Range {
            .start = @intCast(Node.Index, scratch_top),
            .end = @intCast(Node.Index, p.extra_data.items.len),
        };
    }

    fn expectParamDecl(p: *Parser) !Node.Index {
        _ = try p.expectToken(.ident);
        _ = try p.expectToken(.colon);
        return p.expectTypeExpr();
    }

    fn expectFnDecl(p: *Parser) !Node.Index {
        const proto_node = try p.expectFnProto();
        const block_node = try p.expectBlock();

        return p.addNode(.{
            .tag = .fn_decl,
            .main_token = 0,
            .data = .{
                .l = proto_node,
                .r = block_node,
            },
        });
    }

    fn expectBlock(p: *Parser) !Node.Index {
        const block_node = try p.reserveNode(.block);
        const l_brace_token = try p.expectToken(.l_brace);

        const block_start = p.nodes.len;
        while (true) {
            // std.log.debug("next token: {any}", .{p.token_tags[p.index - 2..p.index + 1]});
            if (p.eatToken(.r_brace)) |_| break;

            switch (p.token_tags[p.index]) {
                // TODO: remove
                .l_brace => _ = try p.expectBlock(),
                else => {
                    if (try p.parseStmt() == null_node) p.index += 1;
                },
            }
            // std.log.debug("next token: {any}", .{p.token_tags[p.index - 1..p.index + 2]});
            // const param = try p.expectParamDecl();
            // if (param != null_node) try p.scratch.append(p.gpa, param);
            // switch (p.token_tags[p.index]) {
            //     .comma, .r_paren => p.index += 1,
            //     else => return Error.ParseError,
            // }
        }
        const block_end = p.nodes.len;

        return p.setNode(block_node, .{
            .tag = .block,
            .main_token = l_brace_token,
            .data = .{
                .l = @intCast(u32, block_start),
                .r = @intCast(u32, block_end),
            }
        });
    }

    fn parseStmt(p: *Parser) Error!Node.Index {
        const tag = p.token_tags[p.index];
        const node = switch (tag) {
            .k_let => p.parseDecl(),
            .k_return => p.parseRetStmt(),
            else => null_node,
        };

        _ = p.eatToken(.semi);
        return node;
    }

    fn parseRetStmt(p: *Parser) !Node.Index {
        const ret_token = try p.expectToken(.k_return);
        const value_node: Node.Index = p.expectExpr() catch null_node;

        return p.addNode(.{
            .tag = .ret_stmt,
            .main_token = ret_token,
            .data = .{
                .l = value_node,
                .r = undefined,
            },
        });
    }
    // fn parseStructMembers(self: *Parser) !Node.Index {
    //     while (true) {
    //         const member_token = try self.expectToken(.ident);
    //         _ = try self.expectToken(.colon);
    //         const member_type = try self.parseTypeExpr();
    //         _ = try self.expect
    //     }
    // }

    fn parseUse(self: *Parser) !Node.Index {
        const use_token = try self.expectToken(.k_use);
        const scope = try self.expectScopeList();

        const as_token = self.eatToken(.k_as) orelse 0;
        if (as_token != 0) _ = try self.expectToken(.ident);

        return self.addNode(.{
            .tag = .use,
            .main_token = use_token,
            .data = .{
                .l = scope,
                .r = as_token,
            }
        });
    }

    fn expectScopeList(p: *Parser) !Node.Range {
        const scratch_top = p.scratch.items.len;
        defer p.scratch.shrinkRetainingCapacity(scratch_top);

        while (true) {
            _ = try p.expectToken(.ident);
            if (p.token_tags[p.index] == .colon_colon) {
                _ = p.eatToken(.colon_colon);
            } else break;
        }

        const params = p.scratch.items[scratch_top..];
        try p.extra_data.appendSlice(p.gpa, params);
        return Node.Range {
            .start = @intCast(Node.Index, scratch_top),
            .end = @intCast(Node.Index, p.extra_data.items.len),
        };
    }
};
