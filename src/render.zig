const std = @import("std");
const ast = @import("ast.zig");

const Ast = ast.Ast;
const Node = ast.Node;

// pub fn renderAst(tree: *Ast) void {
//     renderAstNode(tree, 0, 0);
// }

fn renderAstNode(tree: *Ast, index: Node.Index, depth: u32) void {
    const token_tags = tree.tokens.items(.tag);
    const main_tokens = tree.tokens.items(.main_token);
    const datas = tree.tokens.items(.data);

    switch (token_tags[index]) {
        .global_decl => {
            std.debug.print("global_decl {}", .{});
        },
        else => {},
    }
}
