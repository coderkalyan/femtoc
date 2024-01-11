const std = @import("std");
const Ast = @import("../Ast.zig");
const indenting_writer = @import("indenting_writer.zig");

const IndentingWriter = indenting_writer.IndentingWriter;
const indentingWriter = indenting_writer.indentingWriter;
const Node = Ast.Node;

pub fn AstSourceFormatter(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        tree: *const Ast,

        pub const Self = @This();

        pub fn init(writer: anytype, tree: *const Ast) Self {
            return .{ .stream = indentingWriter(width, writer), .tree = tree };
        }

        pub fn render(self: *Self) !void {
            try self.renderNode(@intCast(self.tree.nodes.len - 1));
        }

        pub fn renderNode(self: *Self, node: Node.Index) !void {
            const tree = self.tree;
            var writer = self.stream.writer();

            switch (tree.nodes.items(.data)[node]) {
                .placeholder => {},
                .toplevel => |toplevel| {
                    var stmt = toplevel.stmts_start;
                    while (stmt < toplevel.stmts_end) : (stmt += 1) {
                        try self.renderNode(tree.extra_data[stmt]);
                        try writer.writeAll(";");
                        try self.stream.newline();
                    }
                },
                .named_ty => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                },
                .fn_decl => |decl| {
                    try writer.writeAll("fn ");

                    try writer.writeAll("(");
                    const signature = tree.extraData(decl.signature, Node.FnSignature);
                    if (signature.params_end > signature.params_start) {
                        var param: u32 = signature.params_start;
                        while (param < signature.params_end - 1) : (param += 1) {
                            try self.renderNode(tree.extra_data[param]);
                            try writer.writeAll(", ");
                        }
                        try self.renderNode(tree.extra_data[param]);
                    }
                    try writer.writeAll(") ");
                    try self.renderNode(signature.return_ty);

                    try writer.writeAll(" ");
                    try self.renderNode(decl.body);
                },
                .param => |param| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(": ");
                    try self.renderNode(param.ty);
                },
                .integer_literal, .float_literal, .bool_literal => {
                    const literal = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(literal);
                },
                .binary_expr => |expr| {
                    try writer.writeAll("(");
                    try self.renderNode(expr.left);

                    try writer.writeAll(" ");
                    const operator = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(operator);
                    try writer.writeAll(" ");

                    try self.renderNode(expr.right);
                    try writer.writeAll(")");
                },
                .var_expr => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                },
                .call_expr => |call| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);

                    try writer.writeAll("(");
                    if (call.args_end > call.args_start) {
                        var arg = call.args_start;
                        while (arg < call.args_end - 1) : (arg += 1) {
                            try self.renderNode(tree.extra_data[arg]);
                            try writer.writeAll(", ");
                        }
                        try self.renderNode(tree.extra_data[arg]);
                    }
                    try writer.writeAll(")");
                },
                .ty_decl => |decl| {
                    try writer.writeAll("type ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.ty);
                    try self.stream.newline();
                },
                .block => |block| {
                    try writer.writeAll("{");
                    self.stream.indent();
                    try self.stream.newline();

                    var stmt = block.stmts_start;
                    while (stmt < block.stmts_end) : (stmt += 1) {
                        try self.renderNode(tree.extra_data[stmt]);
                        switch (tree.nodes.items(.data)[tree.extra_data[stmt]]) {
                            .ty_decl, .const_decl, .var_decl, .return_val, .assign_simple, .assign_binary => {
                                try writer.writeAll(";");
                                try self.stream.newline();
                            },
                            else => {},
                        }
                    }

                    self.stream.dedent();
                    try writer.writeAll("}");
                },
                .const_decl => |decl| {
                    try writer.writeAll("let ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .var_decl => |decl| {
                    try writer.writeAll("let mut ");
                    const ident = tree.tokenString(tree.mainToken(node) + 2);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .return_val => |ret| {
                    try writer.writeAll("return ");
                    try self.renderNode(ret.val);
                },
                .if_simple => |data| {
                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(data.exec_true);
                    try self.stream.newline();
                },
                .if_else => |data| {
                    const exec = tree.extraData(data.exec, Node.IfElse);

                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(exec.exec_true);
                    try writer.writeAll(" else ");
                    try self.renderNode(exec.exec_false);
                    try self.stream.newline();
                },
                .if_chain => |data| {
                    const chain = tree.extraData(data.chain, Node.IfChain);

                    try writer.writeAll("if ");
                    try self.renderNode(data.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(chain.exec_true);
                    try writer.writeAll(" else ");
                    try self.renderNode(chain.next);
                },
                .loop_forever => |loop| {
                    try writer.writeAll("for ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .loop_conditional => |loop| {
                    try writer.writeAll("for ");
                    try self.renderNode(loop.condition);
                    try writer.writeAll(" ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .loop_range => |loop| {
                    const signature = tree.extraData(loop.signature, Node.RangeSignature);

                    try writer.writeAll("for ");
                    try self.renderNode(signature.binding);
                    try writer.writeAll("; ");
                    try self.renderNode(signature.condition);
                    try writer.writeAll("; ");
                    try self.renderNode(signature.afterthought);
                    try writer.writeAll(" ");
                    try self.renderNode(loop.body);
                    try self.stream.newline();
                },
                .assign_simple => |assign| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(assign.val);
                },
                .assign_binary => |assign| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    const operator = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" ");
                    try writer.writeAll(operator);
                    try writer.writeAll(" ");
                    try self.renderNode(assign.val);
                },
            }
        }
    };
}

pub fn AstYamlFormatter(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        tree: *const Ast,

        pub const Self = @This();

        pub fn init(writer: anytype, tree: *const Ast) Self {
            return .{ .stream = indentingWriter(width, writer), .tree = tree };
        }

        pub fn fmt(self: *Self) !void {
            try self.fmtNode(@intCast(self.tree.nodes.len - 1));
        }

        pub fn fmtNode(self: *Self, node: Node.Index) !void {
            const tree = self.tree;
            var writer = self.stream.writer();

            _ = tree;
            // _ = writer;
            std.debug.print("hi\n", .{});
            try writer.print("hi\n", .{});
            // try writer.print("hi\n", .{});
            _ = node;
            // switch (tree.nodes.items(.data)[node]) {
            //     .placeholder => {},
            //     .toplevel => |toplevel| {
            //         var stmt = toplevel.stmts_start;
            //         while (stmt < toplevel.stmts_end) : (stmt += 1) {
            //             try self.renderNode(tree.extra_data[stmt]);
            //             try writer.writeAll(";");
            //             try self.stream.newline();
            //         }
            //     },
            //     .named_ty => {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(ident);
            //     },
            //     .fn_decl => |decl| {
            //         try writer.writeAll("fn ");
            //
            //         try writer.writeAll("(");
            //         const signature = tree.extraData(decl.signature, Node.FnSignature);
            //         if (signature.params_end > signature.params_start) {
            //             var param: u32 = signature.params_start;
            //             while (param < signature.params_end - 1) : (param += 1) {
            //                 try self.renderNode(tree.extra_data[param]);
            //                 try writer.writeAll(", ");
            //             }
            //             try self.renderNode(tree.extra_data[param]);
            //         }
            //         try writer.writeAll(") ");
            //         try self.renderNode(signature.return_ty);
            //
            //         try writer.writeAll(" ");
            //         try self.renderNode(decl.body);
            //     },
            //     .param => |param| {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(ident);
            //         try writer.writeAll(": ");
            //         try self.renderNode(param.ty);
            //     },
            //     .integer_literal, .float_literal, .bool_literal => {
            //         const literal = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(literal);
            //     },
            //     .binary_expr => |expr| {
            //         try writer.writeAll("(");
            //         try self.renderNode(expr.left);
            //
            //         try writer.writeAll(" ");
            //         const operator = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(operator);
            //         try writer.writeAll(" ");
            //
            //         try self.renderNode(expr.right);
            //         try writer.writeAll(")");
            //     },
            //     .var_expr => {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(ident);
            //     },
            //     .call_expr => |call| {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(ident);
            //
            //         try writer.writeAll("(");
            //         if (call.args_end > call.args_start) {
            //             var arg = call.args_start;
            //             while (arg < call.args_end - 1) : (arg += 1) {
            //                 try self.renderNode(tree.extra_data[arg]);
            //                 try writer.writeAll(", ");
            //             }
            //             try self.renderNode(tree.extra_data[arg]);
            //         }
            //         try writer.writeAll(")");
            //     },
            //     .ty_decl => |decl| {
            //         try writer.writeAll("type ");
            //         const ident = tree.tokenString(tree.mainToken(node) + 1);
            //         try writer.writeAll(ident);
            //         try writer.writeAll(" = ");
            //         try self.renderNode(decl.ty);
            //         try self.stream.newline();
            //     },
            //     .block => |block| {
            //         try writer.writeAll("{");
            //         self.stream.indent();
            //         try self.stream.newline();
            //
            //         var stmt = block.stmts_start;
            //         while (stmt < block.stmts_end) : (stmt += 1) {
            //             try self.renderNode(tree.extra_data[stmt]);
            //             switch (tree.nodes.items(.data)[tree.extra_data[stmt]]) {
            //                 .ty_decl, .const_decl, .var_decl, .return_val, .assign_simple, .assign_binary => {
            //                     try writer.writeAll(";");
            //                     try self.stream.newline();
            //                 },
            //                 else => {},
            //             }
            //         }
            //
            //         self.stream.dedent();
            //         try writer.writeAll("}");
            //     },
            //     .const_decl => |decl| {
            //         try writer.writeAll("let ");
            //         const ident = tree.tokenString(tree.mainToken(node) + 1);
            //         try writer.writeAll(ident);
            //         try writer.writeAll(" = ");
            //         try self.renderNode(decl.val);
            //     },
            //     .var_decl => |decl| {
            //         try writer.writeAll("let mut ");
            //         const ident = tree.tokenString(tree.mainToken(node) + 2);
            //         try writer.writeAll(ident);
            //         try writer.writeAll(" = ");
            //         try self.renderNode(decl.val);
            //     },
            //     .return_val => |ret| {
            //         try writer.writeAll("return ");
            //         try self.renderNode(ret.val);
            //     },
            //     .if_simple => |data| {
            //         try writer.writeAll("if ");
            //         try self.renderNode(data.condition);
            //         try writer.writeAll(" ");
            //         try self.renderNode(data.exec_true);
            //         try self.stream.newline();
            //     },
            //     .if_else => |data| {
            //         const exec = tree.extraData(data.exec, Node.IfElse);
            //
            //         try writer.writeAll("if ");
            //         try self.renderNode(data.condition);
            //         try writer.writeAll(" ");
            //         try self.renderNode(exec.exec_true);
            //         try writer.writeAll(" else ");
            //         try self.renderNode(exec.exec_false);
            //         try self.stream.newline();
            //     },
            //     .if_chain => |data| {
            //         const chain = tree.extraData(data.chain, Node.IfChain);
            //
            //         try writer.writeAll("if ");
            //         try self.renderNode(data.condition);
            //         try writer.writeAll(" ");
            //         try self.renderNode(chain.exec_true);
            //         try writer.writeAll(" else ");
            //         try self.renderNode(chain.next);
            //     },
            //     .loop_forever => |loop| {
            //         try writer.writeAll("for ");
            //         try self.renderNode(loop.body);
            //         try self.stream.newline();
            //     },
            //     .loop_conditional => |loop| {
            //         try writer.writeAll("for ");
            //         try self.renderNode(loop.condition);
            //         try writer.writeAll(" ");
            //         try self.renderNode(loop.body);
            //         try self.stream.newline();
            //     },
            //     .loop_range => |loop| {
            //         const signature = tree.extraData(loop.signature, Node.RangeSignature);
            //
            //         try writer.writeAll("for ");
            //         try self.renderNode(signature.binding);
            //         try writer.writeAll("; ");
            //         try self.renderNode(signature.condition);
            //         try writer.writeAll("; ");
            //         try self.renderNode(signature.afterthought);
            //         try writer.writeAll(" ");
            //         try self.renderNode(loop.body);
            //         try self.stream.newline();
            //     },
            //     .assign_simple => |assign| {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         try writer.writeAll(ident);
            //         try writer.writeAll(" = ");
            //         try self.renderNode(assign.val);
            //     },
            //     .assign_binary => |assign| {
            //         const ident = tree.tokenString(tree.mainToken(node));
            //         const operator = tree.tokenString(tree.mainToken(node) + 1);
            //         try writer.writeAll(ident);
            //         try writer.writeAll(" ");
            //         try writer.writeAll(operator);
            //         try writer.writeAll(" ");
            //         try self.renderNode(assign.val);
            //     },
            // }
        }
    };
}
