const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Hir = @import("Hir.zig");
const Type = @import("hir/type.zig").Type;
const Value = @import("value.zig").Value;

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

        pub const Self = @This();

        pub fn init(writer: anytype, hir_data: *const Hir) Self {
            return .{ .stream = indentingWriter(width, writer), .hir = hir_data };
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

            var lbuf: [256]u8 = [_]u8{0} ** 256;
            var rbuf: [256]u8 = [_]u8{0} ** 256;
            switch (ir.insts.items(.tag)[index]) {
                .load_global => {
                    const pl = ir.insts.items(.data)[index].un_node.operand;
                    const inst = ir.untyped_decls.get(pl).?;
                    const ident = try ir.interner.get(pl);
                    try self.formatIndex(inst, &lbuf);
                    try writer.print("load_global({s}) [{s}]", .{ ident, lbuf });
                },
                .fn_decl => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const fn_decl = ir.extraData(pl, Hir.Inst.FnDecl);

                    try self.formatIndex(fn_decl.return_type, &lbuf);
                    try writer.print("func(ret_ty={s}, params={{", .{lbuf});
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
                    try self.formatIndex(param.ty, &lbuf);
                    try writer.print("param(\"{s}\", {s})", .{ param_str, lbuf });
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
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const branch_single = ir.extraData(pl, Hir.Inst.BranchSingle);

                    try self.formatIndex(branch_single.condition, &lbuf);
                    try writer.print("[%{}] branch_single({s}) {{", .{ index, lbuf });
                    // self.stream.indent();
                    try self.stream.newline();
                    try writer.print("true ", .{});
                    try self.renderInst(branch_single.exec_true);
                    // self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                    return;
                },
                .branch_double => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const branch_double = ir.extraData(pl, Hir.Inst.BranchDouble);

                    try self.formatIndex(branch_double.condition, &lbuf);
                    try writer.print("[%{}] branch_double({s}) {{", .{ index, lbuf });
                    // self.stream.indent();
                    try self.stream.newline();
                    try writer.print("true ", .{});
                    try self.renderInst(branch_double.exec_true);
                    try writer.print("false ", .{});
                    try self.renderInst(branch_double.exec_false);
                    // self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                    return;
                },
                .loop => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const loop = ir.extraData(pl, Hir.Inst.Loop);

                    try writer.print("[%{}] loop {{", .{index});
                    // self.stream.indent();
                    try self.stream.newline();
                    try writer.print("condition ", .{});
                    try self.renderInst(loop.condition);
                    try writer.print("body ", .{});
                    try self.renderInst(loop.body);
                    // self.stream.dedent();
                    try writer.print("}}", .{});
                },
                .loop_break => {
                    try writer.print("[%{}] break", .{index});
                },
                .call => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const call = ir.extraData(pl, Hir.Inst.Call);
                    try self.formatIndex(call.ptr, &lbuf);
                    try writer.print("call({s}", .{lbuf});

                    var extra_index: u32 = call.args_start;
                    while (extra_index < call.args_end) : (extra_index += 1) {
                        const arg = ir.extra_data[extra_index];
                        try self.formatIndex(arg, &rbuf);
                        try writer.print(", {s}", .{rbuf});
                    }

                    try writer.print(")", .{});
                },
                .create_function_type => {
                    const function_type = ir.get(index, .create_function_type);
                    try self.formatIndex(function_type.return_type, &lbuf);
                    try writer.print("function_type({s}", .{lbuf});

                    var extra_index: u32 = function_type.params_start;
                    while (extra_index < function_type.params_end) : (extra_index += 1) {
                        const arg = ir.extra_data[extra_index];
                        try self.formatIndex(arg, &rbuf);
                        try writer.print(", {s}", .{rbuf});
                    }

                    try writer.print(")", .{});
                },
                // .dbg_value => {
                //     const pl = ir.insts.items(.data)[index].pl_node.pl;
                //     const data = ir.extraData(pl, Hir.Inst.DebugValue);
                //     const ident_str = try ir.interner.get(data.name);
                //     try self.formatRef(data.value, &lbuf);
                //     try writer.print("dbg_value({s}, {s})", .{ ident_str, lbuf });
                // },
                .constant => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const data = ir.extraData(pl, Hir.Inst.Constant);
                    const ty = try ir.resolveType(undefined, data.ty);
                    try self.formatIndex(data.ty, &lbuf);
                    switch (ty.kind()) {
                        .comptime_uint, .uint => {
                            const val = ir.instToInt(index);
                            try writer.print("constant({s}, {})", .{ lbuf, val });
                        },
                        .comptime_sint, .sint => {
                            const val = ir.instToInt(index);
                            const signed: i32 = @bitCast(@as(u32, @truncate(val)));
                            try writer.print("constant({s}, {})", .{ lbuf, signed });
                        },
                        .comptime_float, .float => {
                            const val = ir.instToFloat(index);
                            try writer.print("constant({s}, {})", .{ lbuf, val });
                        },
                        .function => {
                            const val = ir.values[data.val];
                            const func = val.extended.cast(Value.Function).?;
                            const func_type = ty.extended.cast(Type.Function).?;

                            try writer.print("function(", .{});
                            for (func.params, func_type.param_types) |param, param_type| {
                                _ = param_type;
                                const param_pl = ir.insts.items(.data)[param].pl_node.pl;
                                const param_data = ir.extraData(param_pl, Hir.Inst.Param);
                                const ident_str = try ir.interner.get(param_data.name);
                                try self.formatIndex(param_data.ty, &lbuf);
                                try writer.print("{s}: {s}, ", .{ ident_str, lbuf });
                            }
                            try writer.print(") {{", .{});
                            try self.stream.newline();
                            try writer.print("body ", .{});
                            try self.renderInst(func.body);
                            try writer.print("}}", .{});
                        },
                        else => {},
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

        fn formatType(self: *Self, ty: Type, buf: []u8) !void {
            @memset(buf, 0);
            _ = try std.fmt.bufPrint(buf, "{s}", .{switch (ty.kind()) {
                .void => "void",
                .comptime_uint => "comptime_uint",
                .comptime_sint => "comptime_sint",
                .comptime_float => "comptime_float",
                .uint => {
                    _ = try std.fmt.bufPrint(buf, "u{}", .{ty.basic.width});
                    return;
                },
                .sint => {
                    _ = try std.fmt.bufPrint(buf, "i{}", .{ty.basic.width});
                    return;
                },
                .float => {
                    _ = try std.fmt.bufPrint(buf, "f{}", .{ty.basic.width});
                    return;
                },
                .function => {
                    const function = ty.extended.cast(Type.Function).?;
                    var ret_buf: [128]u8 = [_]u8{0} ** 128;
                    try self.formatType(function.return_type, &ret_buf);
                    _ = try std.fmt.bufPrint(buf, "function(return_ty = {s})", .{ret_buf});
                    return;
                },
                .pointer => {
                    const pointer = ty.extended.cast(Type.Pointer).?;
                    var pointee_buf: [128]u8 = [_]u8{0} ** 128;
                    try self.formatType(pointer.pointee, &pointee_buf);
                    _ = try std.fmt.bufPrint(buf, "pointer({s})", .{pointee_buf});
                    return;
                },
                .array => {
                    const array = ty.extended.cast(Type.Array).?;
                    var array_buf: [128]u8 = [_]u8{0} ** 128;
                    try self.formatType(array.element, &array_buf);
                    _ = try std.fmt.bufPrint(buf, "array({s}, {})", .{ array_buf, array.count });
                    return;
                },
                .unsafe_pointer => {
                    const pointer = ty.extended.cast(Type.UnsafePointer).?;
                    var pointee_buf: [128]u8 = [_]u8{0} ** 128;
                    try self.formatType(pointer.pointee, &pointee_buf);
                    _ = try std.fmt.bufPrint(buf, "unsafe_pointer({s})", .{pointee_buf});
                    return;
                },
                .slice => {
                    const slice = ty.extended.cast(Type.Slice).?;
                    var element_buf: [128]u8 = [_]u8{0} ** 128;
                    try self.formatType(slice.element, &element_buf);
                    _ = try std.fmt.bufPrint(buf, "slice({s})", .{element_buf});
                    return;
                },
                .comptime_array => {
                    const array = ty.extended.cast(Type.ComptimeArray).?;
                    _ = try std.fmt.bufPrint(buf, "comptime_array({})", .{array.count});
                    return;
                },
                else => unreachable, //return error.NotImplemented,
            }});
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
                            var buf: [256]u8 = [_]u8{0} ** 256;
                            try self.formatType(data.ty, &buf);
                            try writer.print("{s}", .{buf});
                        },
                        .placeholder => {},
                        .un_node,
                        .un_tok,
                        .pl_node,
                        .pl_tok,
                        => {
                            const fields = std.meta.fields(@TypeOf(data));
                            inline for (fields, 0..) |field, i| {
                                if (!std.mem.eql(u8, field.name, "node") and !std.mem.eql(u8, field.name, "tok") and !std.mem.eql(u8, field.name, "pl")) {
                                    const content = @field(data, field.name);
                                    var lbuf: [256]u8 = [_]u8{0} ** 256;
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
