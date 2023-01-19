const std = @import("std");
const lex = @import("lex.zig");
const ast = @import("ast.zig");
const Token = lex.Token;
const Tokenizer = lex.Tokenizer;

const Parser = struct {
    gpa: Allocator,
    source: []const u8,

    token_tags: []const Token.Tag,
    token_starts: []const u32,
    index: u32,

    nodes: std.ArrayListUnmanaged(Node),
};
