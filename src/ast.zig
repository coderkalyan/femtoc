const std = @import("std");
const lex = @import("lex.zig");
const Token = lex.Token;

const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
};

const Ast = struct {
    source: [:0]const u8,
    tokens: []const Token,
    nodes: []const Node,
};
