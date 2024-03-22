const std = @import("std");
const Ast = @import("../Ast.zig");
const InternPool = @import("../InternPool.zig");
const Type = @import("../air/type.zig").Type;
const render = @import("../render.zig");

const Allocator = std.mem.Allocator;
const io = std.io;
const IndentingWriter = render.IndentingWriter;
const indentingWriter = render.indentingWriter;
const Node = Ast.Node;

pub fn AstFormatter(comptime width: u32, comptime WriterType: anytype) type {
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
                .ident => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                },
                .paren => |paren| {
                    try writer.writeAll("(");
                    try self.renderNode(paren);
                    try writer.writeAll(")");
                },
                .pointer_type => |pointee| {
                    try self.renderNode(pointee);
                    try writer.writeAll("*");
                },
                .mut_pointer_type => |pointee| {
                    try self.renderNode(pointee);
                    try writer.writeAll(" mut*");
                },
                .many_pointer_type => |pointee| {
                    try self.renderNode(pointee);
                    try writer.writeAll("[*]");
                },
                .mut_many_pointer_type => |pointee| {
                    try self.renderNode(pointee);
                    try writer.writeAll(" mut[*]");
                },
                .slice_type => |element| {
                    try self.renderNode(element);
                    try writer.writeAll("[]");
                },
                .mut_slice_type => |element| {
                    try self.renderNode(element);
                    try writer.writeAll(" mut[]");
                },
                .array_type => |array| {
                    try self.renderNode(array.element_type);
                    try writer.writeAll("[");
                    try self.renderNode(array.count_expr);
                    try writer.writeAll("]");
                },
                .function_type => |func| {
                    try writer.writeAll("fn (");
                    const params_slice = tree.extraData(func.params, Node.ExtraSlice);
                    const params = tree.extraSlice(params_slice);
                    for (params, 0..) |param, i| {
                        try self.renderNode(param);
                        if (i < params.len - 1) try writer.writeAll(", ");
                    }
                    try writer.writeAll(") ");
                    try self.renderNode(func.@"return");
                },
                .struct_type => unreachable, // TODO
                .function_literal => |decl| {
                    try self.renderNode(decl.ty);
                    try writer.writeAll(" ");
                    try self.renderNode(decl.body);
                },
                .param => |param| {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(": ");
                    try self.renderNode(param);
                },
                .param_mut => |param| {
                    try writer.writeAll("mut ");
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(ident);
                    try writer.writeAll(": ");
                    try self.renderNode(param);
                },
                .field => unreachable, // TODO
                .integer_literal,
                .float_literal,
                .bool_literal,
                .char_literal,
                .string_literal,
                => {
                    const literal = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(literal);
                },
                .struct_literal, .field_initializer => unreachable, // TODO
                .array_literal => |arr| {
                    try writer.writeAll("[");
                    const elements_slice = tree.extraData(arr.elements, Node.ExtraSlice);
                    const elements = tree.extraSlice(elements_slice);
                    for (elements, 0..) |element, i| {
                        try self.renderNode(element);
                        if (i < elements.len - 1) try writer.writeAll(", ");
                    }
                    try writer.writeAll("]");
                },
                .binary => |expr| {
                    try self.renderNode(expr.left);

                    try writer.writeAll(" ");
                    const operator = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(operator);
                    try writer.writeAll(" ");

                    try self.renderNode(expr.right);
                },
                .unary => |expr| {
                    const operator = tree.tokenString(tree.mainToken(node));
                    try writer.writeAll(operator);
                    try self.renderNode(expr);
                },
                .call => |call| {
                    try self.renderNode(call.ptr);
                    try writer.writeAll("(");
                    const args_slice = tree.extraData(call.args, Node.ExtraSlice);
                    const args = tree.extraSlice(args_slice);
                    for (args, 0..) |arg, i| {
                        try self.renderNode(arg);
                        if (i < args.len - 1) try writer.writeAll(", ");
                    }
                    try writer.writeAll(")");
                },
                .subscript => |subscript| {
                    try self.renderNode(subscript.operand);
                    try writer.print("[{}]", .{subscript.index});
                },
                .slice => |slice| {
                    const range = tree.extraData(slice.range, Node.Range);
                    try self.renderNode(slice.operand);
                    try writer.print("[{}..{}]", .{ range.start, range.end });
                },
                .member => |member| {
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try self.renderNode(member);
                    try writer.print(".{s}", .{ident});
                },
                .const_decl => |decl| {
                    try writer.writeAll("let ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    if (decl.ty != 0) {
                        try writer.writeAll(": ");
                        try self.renderNode(decl.ty);
                    }
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .const_decl_attr => |decl| {
                    const metadata = tree.extraData(decl.metadata, Node.DeclMetadata);
                    const attrs = tree.extraSlice(.{
                        .start = metadata.attrs_start,
                        .end = metadata.attrs_end,
                    });
                    for (attrs) |attr| {
                        try self.renderNode(attr);
                        try writer.writeAll(" ");
                    }

                    try writer.writeAll("let ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    if (metadata.ty != 0) {
                        try writer.writeAll(": ");
                        try self.renderNode(metadata.ty);
                    }
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .var_decl => |decl| {
                    try writer.writeAll("mut ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    if (decl.ty != 0) {
                        try writer.writeAll(": ");
                        try self.renderNode(decl.ty);
                    }
                    try writer.writeAll(" = ");
                    try self.renderNode(decl.val);
                },
                .type_decl => |decl| {
                    try writer.writeAll("type ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl);
                    try self.stream.newline();
                },
                .distinct_type_decl => |decl| {
                    try writer.writeAll("distinct type ");
                    const ident = tree.tokenString(tree.mainToken(node) + 1);
                    try writer.writeAll(ident);
                    try writer.writeAll(" = ");
                    try self.renderNode(decl);
                    try self.stream.newline();
                },
                .block => |block| {
                    try writer.writeAll("{");
                    self.stream.indent();
                    try self.stream.newline();

                    const stmts_slice = tree.extraData(block.stmts, Node.ExtraSlice);
                    const stmts = tree.extraSlice(stmts_slice);
                    for (stmts) |stmt| {
                        try self.renderNode(stmt);
                        switch (tree.nodes.items(.data)[stmt]) {
                            .if_simple,
                            .if_else,
                            .if_chain,
                            .loop_forever,
                            .loop_conditional,
                            .loop_range,
                            .block,
                            => {},
                            else => {
                                try writer.writeAll(";");
                                try self.stream.newline();
                            },
                        }
                    }

                    self.stream.dedent();
                    try writer.writeAll("}");
                },
                .attr_simple => {
                    const ident = tree.tokenString(tree.mainToken(node));
                    try writer.print("{s}", .{ident});
                },
                .attr_args => unreachable, // TODO
                .assign_simple => |assign| {
                    try self.renderNode(assign.ptr);
                    try writer.writeAll(" = ");
                    try self.renderNode(assign.val);
                },
                .assign_binary => |assign| {
                    const operator = tree.tokenString(tree.mainToken(node));
                    try self.renderNode(assign.ptr);
                    try writer.print(" {s} ", .{operator});
                    try self.renderNode(assign.val);
                },
                .return_val => |ret| {
                    try writer.writeAll("return ");
                    try self.renderNode(ret);
                },
                .yield_val => |ret| {
                    try writer.writeAll("yield ");
                    try self.renderNode(ret);
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
                .@"break" => try writer.writeAll("break"),
                .module => |module| {
                    const data = tree.extraData(module.stmts, Node.ExtraSlice);
                    const stmts = tree.extraSlice(data);
                    for (stmts) |stmt| {
                        try self.renderNode(stmt);
                        try writer.writeAll(";");
                        try self.stream.newline();
                    }
                },
            }
        }
    };
}
