const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Hir = @import("Hir.zig");
const Type = @import("hir/type.zig").Type;
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;

const io = std.io;

const Node = Ast.Node;

pub fn AstRenderer(comptime width: u32, comptime WriterType: anytype) type {
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

pub fn HirRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        hir: *const Hir,
        arena: Allocator,

        pub const Self = @This();

        pub fn init(writer: anytype, arena: Allocator, hir_data: *const Hir) Self {
            return .{ .stream = indentingWriter(width, writer), .arena = arena, .hir = hir_data };
        }

        pub fn render(r: *Self) !void {
            const writer = r.stream.writer();

            const module_pl = r.hir.insts.items(.data)[r.hir.module_index].pl_node.pl;
            const data = r.hir.extraData(module_pl, Hir.Inst.Module);

            var extra_index: u32 = 0;
            while (extra_index < data.len * 2) : (extra_index += 2) {
                const base = module_pl + 1;
                const id = r.hir.extra_data[base + extra_index];
                const inst = r.hir.extra_data[base + extra_index + 1];
                const member_str = try r.hir.interner.get(id);

                try writer.print("{s}: ", .{member_str});
                try r.renderInst(inst);
            }
        }

        pub fn renderInst(self: *Self, index: u32) !void {
            const ir = self.hir;
            const writer = self.stream.writer();

            switch (ir.insts.items(.tag)[index]) {
                .block,
                .block_inline,
                .branch_single,
                .branch_double,
                .loop,
                .loop_break,
                .store,
                => {},
                else => try writer.print("%{} = ", .{index}),
            }

            switch (ir.insts.items(.tag)[index]) {
                .load_global => {
                    const pl = ir.get(index, .load_global).operand;
                    const inst = ir.untyped_decls.get(pl).?;
                    const ident = try ir.interner.get(pl);
                    try writer.print("load_global(%{}) [{s}]", .{ inst, ident });
                },
                .fn_decl => {
                    const fn_decl = ir.get(index, .fn_decl);

                    try writer.print("func(ret_ty=%{}, params={{", .{fn_decl.return_type});
                    self.stream.indent();
                    self.stream.indent();
                    try self.stream.newline();
                    var extra_index: u32 = fn_decl.params_start;
                    while (extra_index < fn_decl.params_end) : (extra_index += 1) {
                        const param = ir.extra_data[extra_index];
                        try self.renderInst(param);
                    }
                    self.stream.dedent();
                    try writer.print("}}, body={{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(fn_decl.body);
                    self.stream.dedent();
                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .param => {
                    const param = ir.get(index, .param);
                    const param_str = try ir.interner.get(param.name);
                    try writer.print("param(\"{s}\", %{})", .{ param_str, param.ty });
                },
                inline .block, .block_inline => |tag| {
                    const block = ir.get(index, .block);

                    try writer.print("[%{}]", .{index});
                    if (tag == .block_inline) try writer.print(" [inline]", .{});
                    try writer.print(":", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const insts = ir.block_slices[block.head];
                    if (insts.len == 0) {
                        try writer.print("[empty]", .{});
                        try self.stream.newline();
                    } else {
                        for (insts) |inst| {
                            try self.renderInst(inst);
                        }
                    }

                    self.stream.dedent();
                    return;
                },
                .branch_single => {
                    const branch_single = ir.get(index, .branch_single);
                    const cond = branch_single.condition;
                    try writer.print("[%{}] branch_single(%{}) {{", .{ index, cond });
                    try self.stream.newline();
                    try writer.print("true ", .{});
                    try self.renderInst(branch_single.exec_true);
                    try writer.print("}}", .{});
                    try self.stream.newline();
                    return;
                },
                .branch_double => {
                    const branch_double = ir.get(index, .branch_double);
                    const cond = branch_double.condition;
                    try writer.print("[%{}] branch_double(%{}) {{", .{ index, cond });
                    try self.stream.newline();
                    try writer.print("true ", .{});
                    try self.renderInst(branch_double.exec_true);
                    try writer.print("false ", .{});
                    try self.renderInst(branch_double.exec_false);
                    try writer.print("}}", .{});
                    try self.stream.newline();
                    return;
                },
                .loop => {
                    const loop = ir.get(index, .loop);
                    try writer.print("[%{}] loop {{", .{index});
                    try self.stream.newline();
                    try writer.print("condition ", .{});
                    try self.renderInst(loop.condition);
                    try writer.print("body ", .{});
                    try self.renderInst(loop.body);
                    try writer.print("}}", .{});
                },
                .loop_break => {
                    try writer.print("[%{}] break", .{index});
                },
                .call => {
                    const call = ir.get(index, .call);
                    try writer.print("call(%{}", .{call.ptr});

                    var extra_index: u32 = call.args_start;
                    while (extra_index < call.args_end) : (extra_index += 1) {
                        const arg = ir.extra_data[extra_index];
                        try writer.print(", %{}", .{arg});
                    }

                    try writer.print(")", .{});
                },
                .create_function_type => {
                    const function_type = ir.get(index, .create_function_type);
                    const ret_type = function_type.return_type;
                    try writer.print("function_type(%{}", .{ret_type});

                    var extra_index: u32 = function_type.params_start;
                    while (extra_index < function_type.params_end) : (extra_index += 1) {
                        const arg = ir.extra_data[extra_index];
                        try writer.print(", %{}", .{arg});
                    }

                    try writer.print(")", .{});
                },
                .constant => {
                    const data = ir.get(index, .constant);
                    const ty = try ir.resolveType(undefined, data.ty);
                    switch (ty.kind()) {
                        .comptime_uint, .uint => {
                            const val = ir.instToInt(index);
                            try writer.print("constant(%{}, {})", .{ data.ty, val });
                        },
                        .comptime_sint, .sint => {
                            const val = ir.instToInt(index);
                            const signed: i32 = @bitCast(@as(u32, @truncate(val)));
                            try writer.print("constant(%{}, {})", .{ data.ty, signed });
                        },
                        .comptime_float, .float => {
                            const val = ir.instToFloat(index);
                            try writer.print("constant(%{}, {})", .{ data.ty, val });
                        },
                        .function => {
                            const func = ir.pool.getValue(data.val).function;
                            const func_type = ty.extended.cast(Type.Function).?;

                            try writer.print("function(", .{});
                            for (func.params, func_type.param_types) |param, param_type| {
                                _ = param_type;
                                const param_pl = ir.insts.items(.data)[param].pl_node.pl;
                                const param_data = ir.extraData(param_pl, Hir.Inst.Param);
                                const ident_str = try ir.interner.get(param_data.name);
                                try writer.print("{s}: %{}, ", .{ ident_str, param_data.ty });
                            }
                            try writer.print(") {{", .{});
                            try self.stream.newline();
                            try writer.print("body ", .{});
                            try self.renderInst(func.body);
                            try writer.print("}}", .{});
                        },
                        else => |kind| try writer.print("{}", .{kind}),
                    }
                },
                else => try self.formatInst(index),
            }

            try self.stream.newline();
        }

        fn formatIndex(_: *Self, index: Hir.Index, buf: []u8) !void {
            @memset(buf, 0);
            _ = try std.fmt.bufPrint(buf, "%{}", .{index});
        }

        fn formatType(self: *Self, ty: Type) ![]const u8 {
            switch (ty.kind()) {
                .void => return "void",
                .comptime_uint => return "comptime_uint",
                .comptime_sint => return "comptime_sint",
                .comptime_float => return "comptime_float",
                .uint => return std.fmt.allocPrint(self.arena, "u{}", .{ty.basic.width}),
                .sint => return std.fmt.allocPrint(self.arena, "i{}", .{ty.basic.width}),
                .float => return std.fmt.allocPrint(self.arena, "f{}", .{ty.basic.width}),
                .function => {
                    const function = ty.extended.cast(Type.Function).?;
                    const ret = try self.formatType(function.return_type);
                    return std.fmt.allocPrint(self.arena, "function(return_ty = {s})", .{ret});
                },
                .pointer => {
                    const pointer = ty.extended.cast(Type.Pointer).?;
                    const pointee = try self.formatType(pointer.pointee);
                    return std.fmt.allocPrint(self.arena, "{s}*", .{pointee});
                },
                .array => {
                    const array = ty.extended.cast(Type.Array).?;
                    const element = try self.formatType(array.element);
                    return std.fmt.allocPrint(self.arena, "{s}[{}]", .{ element, array.count });
                },
                .many_pointer => {
                    const pointer = ty.extended.cast(Type.ManyPointer).?;
                    const pointee = try self.formatType(pointer.pointee);
                    return std.fmt.allocPrint(self.arena, "{s}[*]", .{pointee});
                },
                .slice => {
                    const slice = ty.extended.cast(Type.Slice).?;
                    const element = try self.formatType(slice.element);
                    return std.fmt.allocPrint(self.arena, "{s}[]", .{element});
                },
                .comptime_array => {
                    const array = ty.extended.cast(Type.ComptimeArray).?;
                    return std.fmt.allocPrint(self.arena, "comptime_array({})", .{array.count});
                },
                .string => return "u8[]",
                else => unreachable, //return error.NotImplemented,
            }
        }

        fn formatInst(self: *Self, inst: Hir.Index) !void {
            const hir = self.hir;
            const writer = self.stream.writer();

            switch (hir.insts.items(.tag)[inst]) {
                inline else => |tag| {
                    try writer.print("{s}(", .{@tagName(tag)});

                    const data = hir.get(inst, tag);
                    const active_field = comptime Hir.activeDataField(tag);
                    switch (active_field) {
                        .int => try writer.print("{}", .{data.int}),
                        .float => try writer.print("{}", .{data.float}),
                        .ty => {
                            const buf = try self.formatType(data.ty);
                            try writer.print("{s}", .{buf});
                        },
                        .placeholder => {},
                        .un_node,
                        .un_tok,
                        .pl_node,
                        .pl_tok,
                        .ty_op,
                        => {
                            const fields = std.meta.fields(@TypeOf(data));
                            inline for (fields, 0..) |field, i| {
                                if (!std.mem.eql(u8, field.name, "node") and !std.mem.eql(u8, field.name, "tok") and !std.mem.eql(u8, field.name, "pl")) {
                                    const content = @field(data, field.name);
                                    var lbuf: [128]u8 = [_]u8{0} ** 128;
                                    try self.formatIndex(content, &lbuf);
                                    try writer.print("{s}", .{lbuf});

                                    if (i < fields.len - 1) {
                                        try writer.print(", ", .{});
                                    }
                                }
                            }
                        },
                        .node, .token => {},
                    }

                    try writer.print(")", .{});
                },
            }
        }
    };
}

fn IndentingWriter(comptime width: u32, comptime WriterType: type) type {
    return struct {
        depth: u32,
        underlying_writer: WriterType,
        needs_indent: bool,

        const Self = @This();
        pub const Error = WriterType.Error;
        pub const Writer = io.Writer(*Self, Error, write);

        pub fn newline(self: *Self) !void {
            if (self.needs_indent) try self.writeIndent();
            try self.underlying_writer.writeAll("\n");
            self.needs_indent = true;
        }

        pub fn indent(self: *Self) void {
            self.depth += 1;
        }

        pub fn dedent(self: *Self) void {
            std.debug.assert(self.depth >= 1);
            self.depth -= 1;
        }

        fn writeIndent(self: *Self) !void {
            self.needs_indent = false;

            var i: u32 = 0;
            while (i < self.depth) : (i += 1) {
                try self.underlying_writer.writeAll(" " ** width);
            }
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        pub fn write(self: *Self, bytes: []const u8) Error!usize {
            if (self.needs_indent) try self.writeIndent();
            return self.underlying_writer.write(bytes);
        }
    };
}

fn indentingWriter(comptime width: u32, underlying_stream: anytype) IndentingWriter(width, @TypeOf(underlying_stream)) {
    return .{ .depth = 0, .underlying_writer = underlying_stream, .needs_indent = false };
}
