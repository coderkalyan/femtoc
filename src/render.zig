const std = @import("std");
const ast = @import("ast.zig");
const lex = @import("lex.zig");

const Ast = ast.Ast;
const Node = ast.Node;

pub fn renderAst(tree: *const Ast) !void {
    // const Writer = WriterType.writer();
    try renderNode(tree, @intCast(u32, tree.nodes.len - 1), std.io.getStdOut().writer(), 0);
}

pub fn renderNode(tree: *const Ast, index: Node.Index, writer: @TypeOf(std.io.getStdOut().writer()), depth: u32) !void {
    const main_token = tree.nodes.items(.main_token)[index];
    const data = tree.nodes.items(.data)[index];
    // const main_tag = tree.tokens.items(.tag)[main_token];
    const main_start = tree.tokens.items(.start)[main_token];
    // std.debug.print("{}\n", .{tree.tokens.items(.tag)[main_token]});

    switch (data) {
        .named_ty => |_| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];

            _ = try writer.write(ident);
        },
        .fn_decl => |decl| {
            try indent(writer, depth);
            _ = try writer.write("fn");
            try renderNode(tree, decl.proto, writer, depth);
            _ = try writer.write(" ");
            try renderNode(tree, decl.body, writer, depth + 1);
        },
        .fn_proto => |proto| {
            _ = try writer.write("(");
            const params = tree.extraData(proto.params, Node.CallSignature);
            var param_index: u32 = params.params_start;
            while (param_index < params.params_end) : (param_index += 1) {
                var param_node = tree.extra_data[param_index];
                try renderNode(tree, param_node, writer, depth);
                if (param_index < params.params_end - 1) _ = try writer.write(", ");
            }
            _ = try writer.write(") ");
            try renderNode(tree, proto.return_ty, writer, depth);
        },
        .param => |param| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            
            _ = try writer.write(ident);
            _ = try writer.write(": ");
            try renderNode(tree, param.ty, writer, depth);
        },
        .integer_literal => |_| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const literal = tree.source[token.loc.start..token.loc.end];

            _ = try writer.write(literal);
        },
        .float_literal => |_| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const literal = tree.source[token.loc.start..token.loc.end];

            _ = try writer.write(literal);
        },
        .binary_expr => |expr| {
            _ = try writer.write("(");
            try renderNode(tree, expr.left, writer, depth);
            _ = try writer.write(" ");

            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const op = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(op);
            
            _ = try writer.write(" ");
            try renderNode(tree, expr.right, writer, depth);
            _ = try writer.write(")");
        },
        .var_expr => |_| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(ident);
        },
        .call_expr => |call| {
            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(ident);

            _ = try writer.write("(");
            var arg_index = call.args_start;
            while (arg_index < call.args_end) : (arg_index += 1) {
                const arg = tree.extra_data[arg_index];
                try renderNode(tree, arg, writer, depth);
                if (arg_index < call.args_end - 1) _ = try writer.write(", ");
            }
            _ = try writer.write(")");
        },
        .ty_decl => |decl| {
            try indent(writer, depth);
            _ = try writer.write("type ");

            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(ident);

            _ = try writer.write(" = ");
            try renderNode(tree, decl.ty, writer, depth);
            _ = try writer.write("\n");
        },
        .block => |block| {
            _ = try writer.write("{\n");

            var stmt_index = block.stmts_start;
            while (stmt_index < block.stmts_end) : (stmt_index += 1) {
                var stmt_node = tree.extra_data[stmt_index];
                try renderNode(tree, stmt_node, writer, depth + 1);
            }

            _ = try writer.write("}");
        },
        .const_decl => |decl| {
            try indent(writer, depth);
            _ = try writer.write("let ");

            var lexer = lex.Lexer {
                .source = tree.source,
                .index = tree.tokens.items(.start)[main_token + 1],
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(ident);

            _ = try writer.write(" = ");
            try renderNode(tree, decl.val, writer, depth);
            _ = try writer.write(";\n");
        },
        .var_decl => |decl| {
            try indent(writer, depth);
            _ = try writer.write("let mut ");

            var lexer = lex.Lexer {
                .source = tree.source,
                .index = main_start,
            };
            const token = lexer.next();
            const ident = tree.source[token.loc.start..token.loc.end];
            _ = try writer.write(ident);

            _ = try writer.write(" = ");
            try renderNode(tree, decl.val, writer, depth);
            _ = try writer.write("\n");
        },
        // .return_void => |_| {
        //     try indent(writer, depth);
        //     _ = try writer.write("return;\n");
        // },
        .return_val => |val| {
            try indent(writer, depth);
            _ = try writer.write("return ");
            try renderNode(tree, val.val, writer, depth);
            _ = try writer.write(";\n");
        },
        .if_stmt => |stmt| {
            try indent(writer, depth);
            _ = try writer.write("if ");
            try renderNode(tree, stmt.condition, writer, depth);
            _ = try writer.write("\n");
            try renderNode(tree, stmt.body, writer, depth + 1);
        },
        .toplevel => |toplevel| {
            var stmt_index = toplevel.stmts_start;
            while (stmt_index < toplevel.stmts_end) : (stmt_index += 1) {
                var stmt_node = tree.extra_data[stmt_index];
                try renderNode(tree, stmt_node, writer, depth);
                _ = try writer.write("\n");
            }
        },
    }
}

fn indent(writer: @TypeOf(std.io.getStdOut().writer()), depth: u32) !void {
    // TODO: source
    var i: u32 = 0;
    while (i < depth * 2) : (i += 1) {
        _ = try writer.write(" ");
    }
}
// fn renderAstNode(tree: *Ast, index: Node.Index, depth: u32) void {
//     const token_tags = tree.tokens.items(.tag);
//     const main_tokens = tree.tokens.items(.main_token);
//     const datas = tree.tokens.items(.data);
//
//     switch (token_tags[index]) {
//         .global_decl => {
//             std.debug.print("global_decl {}", .{});
//         },
//         else => {},
//     }
// }
