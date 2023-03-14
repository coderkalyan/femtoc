const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;

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
            try self.renderNode(@intCast(u32, self.tree.nodes.len - 1));
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
                            .ty_decl, .const_decl, .var_decl,
                            .return_val,
                            .assign_simple, .assign_binary => {
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
            const toplevel = r.hir.insts.items(.data)[r.hir.insts.len - 1];
            const data = r.hir.extraData(toplevel.pl_node.pl, Hir.Inst.Module);

            var extra_index: u32 = 0;
            while (extra_index < data.len) : (extra_index += 1) {
                const index = r.hir.extra_data[toplevel.pl_node.pl + 1 + extra_index];
                try r.renderInst(index);
            }
        }

        pub fn renderInst(self: *Self, index: u32) !void {
            const ir = self.hir;
            const writer = self.stream.writer();

            try writer.print("%{} = ", .{index});
            var lbuf: [32]u8 = [_]u8{0} ** 32;
            var rbuf: [32]u8 = [_]u8{0} ** 32;
            switch (ir.insts.items(.tag)[index]) {
                .int => try writer.print("int({})", .{ir.insts.items(.data)[index].int}),
                .float => try writer.print("float({})", .{ir.insts.items(.data)[index].float}),
                .add, .sub, .mul, .div, .mod,
                .eq, .neq, .leq, .geq, .lt, .gt => {
                    try writer.writeAll(switch (ir.insts.items(.tag)[index]) {
                        .add => "add",
                        .sub => "sub",
                        .mul => "mul",
                        .div => "div",
                        .mod => "mod",
                        .eq => "eq",
                        .neq => "neq",
                        .leq => "leq",
                        .geq => "geq",
                        .lt => "lt",
                        .gt => "gt",
                        else => unreachable,
                    });

                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const bin = ir.extraData(pl, Hir.Inst.Binary);
                    try self.formatRef(bin.lref, &lbuf);
                    try self.formatRef(bin.rref, &rbuf);
                    try writer.print("({s}, {s})", .{lbuf, rbuf});
                },
                .validate_ty => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const validate_ty = ir.extraData(pl, Hir.Inst.ValidateTy);
                    try self.formatRef(validate_ty.ty, &lbuf);
                    try self.formatRef(validate_ty.ref, &rbuf);
                    try writer.print("validate_ty({s}, {s})", .{lbuf, rbuf});
                },
                .alloc => {
                    try self.formatRef(ir.insts.items(.data)[index].un_node.operand, &lbuf);
                    try writer.print("alloc({s})", .{lbuf});
                },
                .store => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const store = ir.extraData(pl, Hir.Inst.Store);
                    try self.formatIndex(store.addr, &lbuf);
                    try self.formatRef(store.val, &rbuf);
                    try writer.print("store({s}, {s})", .{lbuf, rbuf});
                },
                .load => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("load({s})", .{lbuf});
                },
                .load_inline => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const ref = ir.resolution_map.get(pl).?;
                    try self.formatRef(ref, &lbuf);
                    try writer.print("load_inline({s})", .{lbuf});
                },
                .fn_decl => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const fn_decl = ir.extraData(pl, Hir.Inst.FnDecl);

                    try self.formatRef(fn_decl.return_ty, &lbuf);
                    try writer.print("func(ret_ty={s}, body={{", .{lbuf});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(fn_decl.body);
                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .block => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const block = ir.extraData(pl, Hir.Inst.Block);

                    try writer.print("block({{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    var extra_index: u32 = 0;
                    while (extra_index < block.len) : (extra_index += 1) {
                        const inst = self.hir.extra_data[pl + 1 + extra_index];
                        try self.renderInst(inst);
                    }

                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .branch_single => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const branch_single = ir.extraData(pl, Hir.Inst.BranchSingle);

                    try self.formatRef(branch_single.condition, &lbuf);
                    try writer.print("branch_single({s},", .{lbuf});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("exec_true = {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(branch_single.exec_true);
                    self.stream.dedent();
                    try writer.print("}})", .{});
                    self.stream.dedent();
                },
                .branch_double => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const branch_double = ir.extraData(pl, Hir.Inst.BranchDouble);

                    try self.formatRef(branch_double.condition, &lbuf);
                    try writer.print("branch_double({s},", .{lbuf});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("exec_true = {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(branch_double.exec_true);
                    self.stream.dedent();
                    try writer.print("}}, exec_false = {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(branch_double.exec_false);
                    self.stream.dedent();
                    try writer.print("}})", .{});
                    self.stream.dedent();
                },
                .ret_implicit => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("ret_implicit({s})", .{lbuf});
                },
                .ret_node => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("ret_node({s})", .{lbuf});
                },
                .call => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const call = ir.extraData(pl, Hir.Inst.Call);
                    try self.formatRef(call.addr, &lbuf);
                    try writer.print("call({s}", .{lbuf});
                    
                    var extra_index: u32 = 0;
                    while (extra_index < call.args_len) : (extra_index += 1) {
                        const arg = ir.extra_data[pl + 2 + extra_index];
                        try self.formatRef(@intToEnum(Hir.Ref, arg), &rbuf);
                        try writer.print(", {s}", .{rbuf});
                    }
                    
                    try writer.print(")", .{});
                },
                else => {try writer.print("{}", .{ir.insts.items(.tag)[index]});},
            }

            try self.stream.newline();
        }

        fn formatRef(_: *Self, ref: Hir.Ref, buf: []u8) !void {
            if (Hir.Inst.refToIndex(ref)) |index| {
                _ = try std.fmt.bufPrint(buf, "%{}", .{index});
            } else {
                _ = try std.fmt.bufPrint(buf, "@Ref.{s}", .{switch (ref) {
                    .zero_val => "zero",
                    .one_val => "one",
                    .btrue_val => "btrue",
                    .bfalse_val => "bfalse",
                    .void_val => "void_val",
                    .u8_ty => "u8",
                    .u16_ty => "u16",
                    .u32_ty => "u32",
                    .u64_ty => "u64",
                    .i8_ty => "i8",
                    .i16_ty => "i16",
                    .i32_ty => "i32",
                    .i64_ty => "i64",
                    .f32_ty => "f32",
                    .f64_ty => "f64",
                    .bool_ty => "bool",
                    .void_ty => "void",
                    else => unreachable,
                }});
            }
        }

        fn formatIndex(_: *Self, index: Hir.Index, buf: []u8) !void {
            _ = try std.fmt.bufPrint(buf, "%{}", .{index});
        }
    };
}

pub fn MirRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        mir: *const Mir,

        pub const Self = @This();

        pub fn init(writer: anytype, mir_data: *const Mir) Self {
            return .{ .stream = indentingWriter(width, writer), .mir = mir_data };
        }

        pub fn render(r: *Self) !void {
            for (r.mir.blocks) |block| {
                try r.renderBlock(block);
            }
            // var index: u32 = 0;
            // while (index < r.mir.insts.len) : (index += 1) {
            //     try r.renderInst(index);
            // }
            // const module = r.mir.insts.items(.data)[r.mir.insts.len - 1];
            // const data = r.mir.extraData(module.pl, Mir.Module);
            //
            // var extra_index: u32 = 0;
            // while (extra_index < data.len) : (extra_index += 1) {
            //     const index = r.mir.extra[module.pl + 1 + extra_index];
            //     try r.renderInst(index);
            // }
        }

        pub fn renderBlock(r: *Self, b: u32) !void {
            const writer = r.stream.writer();
            try writer.print(".{}:", .{b});
            r.stream.indent();
            try r.stream.newline();

            // const len = r.mir.extra[b];
            // std.debug.print("render block: {any}\n", .{r.mir.extra[b + 1..b + 1 + len]});
            var index: u32 = 0;
            while (index < r.mir.extra[b]) : (index += 1) {
                const inst = r.mir.extra[b + 1 + index];
                try r.renderInst(inst);
            }
            r.stream.dedent();
        }

        pub fn renderInst(self: *Self, index: u32) !void {
            const ir = self.mir;
            const writer = self.stream.writer();

            try writer.print("%{} = ", .{index});
            var lbuf: [32]u8 = [_]u8{0} ** 32;
            var rbuf: [32]u8 = [_]u8{0} ** 32;
            switch (ir.insts.items(.tag)[index]) {
                .constant => {
                    const data = ir.insts.items(.data)[index];
                    const value = ir.values[data.ty_pl.pl];
                    try self.formatTy(data.ty_pl.ty, &lbuf);
                    switch (data.ty_pl.ty.tag) {
                        .comptime_int,
                        .u1, .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                            try writer.print("constant({s}, {})", .{lbuf, value.int});
                        },
                        .comptime_float, .f32, .f64 => {
                            try writer.print("constant({s}, {})", .{lbuf, value.float});
                        },
                        else => {},
                        // else => {std.debug.print("tag = {}\n", .{data.ty_pl.ty.tag});},
                    }
                },
                .add, .sub, .mul, .div, .mod,
                .eq, .neq, .leq, .geq, .lt, .gt => {
                    try writer.writeAll(switch (ir.insts.items(.tag)[index]) {
                        .add => "add",
                        .sub => "sub",
                        .mul => "mul",
                        .div => "div",
                        .mod => "mod",
                        .eq => "eq",
                        .neq => "neq",
                        .leq => "leq",
                        .geq => "geq",
                        .lt => "lt",
                        .gt => "gt",
                        else => unreachable,
                    });

                    const bin = ir.insts.items(.data)[index].bin_op;
                    try self.formatRef(bin.lref, &lbuf);
                    try self.formatRef(bin.rref, &rbuf);
                    try writer.print("({s}, {s})", .{lbuf, rbuf});
                },
                .alloc => {
                    try self.formatTy(ir.insts.items(.data)[index].ty, &lbuf);
                    try writer.print("alloc({s})", .{lbuf});
                },
                .store => {
                    const addr = ir.insts.items(.data)[index].bin_op.lref;
                    const val = ir.insts.items(.data)[index].bin_op.rref;
                    try self.formatRef(addr, &lbuf);
                    try self.formatRef(val, &rbuf);
                    try writer.print("store({s}, {s})", .{lbuf, rbuf});
                },
                .load => {
                    const ref = ir.insts.items(.data)[index].un_op;
                    try self.formatRef(ref, &lbuf);
                    try writer.print("load({s})", .{lbuf});
                },
                .function => {
                    const pl = ir.insts.items(.data)[index].pl;
                    const data = self.mir.extraData(pl, Mir.Function);
                    try writer.print("func(ret_ty=, body={{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    var extra_index: u32 = 0;
                    while (extra_index < data.len) : (extra_index += 1) {
                        const inst = self.mir.extra[pl + 1 + extra_index];
                        try self.renderInst(inst);
                    }

                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .br => {
                    const pl = ir.insts.items(.data)[index].pl;
                    try writer.print("br(.{})", .{pl});
                },
                .condbr => {
                    const condition = ir.insts.items(.data)[index].op_pl.op;
                    const pl = ir.insts.items(.data)[index].op_pl.pl;
                    const data = self.mir.extraData(pl, Mir.Inst.CondBr);
                    try self.formatRef(condition, &lbuf);
                    const exec_true = data.exec_true;
                    const exec_false = data.exec_false;
                    try writer.print("condbr({s}, .{}, .{})", .{lbuf, exec_true, exec_false});
                },
                // .ret_implicit => {
                //     const operand = ir.insts.items(.data)[index].un_node.operand;
                //     try self.formatRef(operand, &lbuf);
                //     try writer.print("ret_implicit({s})", .{lbuf});
                // },
                // .ret_node => {
                //     const operand = ir.insts.items(.data)[index].un_node.operand;
                //     try self.formatRef(operand, &lbuf);
                //     try writer.print("ret_node({s})", .{lbuf});
                // },
                // .call => {
                //     const pl = ir.insts.items(.data)[index].pl_node.pl;
                //     const call = ir.extraData(pl, Hir.Inst.Call);
                //     try self.formatRef(call.addr, &lbuf);
                //     try writer.print("call({s}", .{lbuf});
                //     
                //     var extra_index: u32 = 0;
                //     while (extra_index < call.args_len) : (extra_index += 1) {
                //         const arg = ir.extra_data[pl + 2 + extra_index];
                //         try self.formatRef(@intToEnum(Hir.Ref, arg), &rbuf);
                //         try writer.print(", {s}", .{rbuf});
                //     }
                //     
                //     try writer.print(")", .{});
                // },
                else => {try writer.print("{}", .{ir.insts.items(.tag)[index]});},
            }

            try self.stream.newline();
        }

        fn formatRef(_: *Self, ref: Mir.Ref, buf: []u8) !void {
            if (Mir.refToIndex(ref)) |index| {
                _ = try std.fmt.bufPrint(buf, "%{}", .{index});
            } else {
                _ = try std.fmt.bufPrint(buf, "@Ref.{s}", .{switch (ref) {
                    .zero_val => "zero",
                    .one_val => "one",
                    .void_val => "void_val",
                    else => unreachable,
                }});
            }
        }

        fn formatIndex(_: *Self, index: Mir.Index, buf: []u8) !void {
            _ = try std.fmt.bufPrint(buf, "%{}", .{index});
        }

        fn formatTy(_: *Self, ty: Type, buf: []u8) !void {
            if (ty.isTag()) {
                _ = try std.fmt.bufPrint(buf, "{s}", .{switch (ty.tag) {
                    .void => "void",
                    .comptime_int => "comptime_int",
                    .u1 => "u1",
                    .u8 => "u8",
                    .i8 => "i8",
                    .u16 => "u16",
                    .i16 => "i16",
                    .u32 => "u32",
                    .i32 => "i32",
                    .u64 => "u64",
                    .i64 => "i64",
                    .comptime_float => "comptime_float",
                    .f32 => "f32",
                    .f64 => "f64",
                }});
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

pub fn AstErrorRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        ast: *const Ast,
        newlines: []Ast.ByteOffset,
        source: [:0]const u8,
        allocator: *const std.mem.Allocator,
        filename: [:0]const u8,

        pub const Self = @This();

        const TextTokenPos = struct {
            line: u32,
            character: Ast.ByteOffset,

            line_pos: Ast.ByteOffset,
            next_line_pos : Ast.ByteOffset,
        };

        pub fn init(writer: anytype, ast_data: *const Ast, source: [:0]const u8, allocator: *const std.mem.Allocator, filename: [:0]const u8) Self {
            // TODO: maybe move this to the actual parsing step.
            // TODO: reuse other Ast renderer
            // TODO: not have such a bad init for this :(

            return .{ 
                .stream = indentingWriter(width, writer), 
                .ast = ast_data,
                .source = source,
                .newlines = &.{},
                .allocator = allocator,
                .filename = filename,
            };
        }

        fn build_newline_arr(self: *Self) !void {
            var newline_locs: std.ArrayList(Ast.ByteOffset) = std.ArrayList(Ast.ByteOffset).init(self.allocator.*);
            for (self.source) |char, i| {

                // HACK: idk how to fix this rn, and don't want to bother kalyan so will fix later
                if (char == '\n') try newline_locs.append(@intCast(Ast.ByteOffset, i)); 
            }

            // append the last char so we have a marker at the end of the file
            // this will make slicing for the line preview simpler

            try newline_locs.append(@intCast(Ast.ByteOffset, self.source.len - 1)); 

            self.newlines = newline_locs.toOwnedSlice();
        }

        fn lookup_pos(self: *Self, source_index: Ast.ByteOffset) TextTokenPos {
            // PERF: For now do a linear probe, but we should really do a binary search
            var prev_newline_pos: Ast.ByteOffset = 0;
            var current_newline_pos: Ast.ByteOffset = 0;

            // Line counts start at 1
            var line_number: u32 = 1; // TODO: check if this is the correct type

            // std.debug.print("SRC I {any} \n", .{source_index});

            for (self.newlines) |newline_pos| {
                // std.debug.print("WE AT {any}, prev={}, linenum={} \n", .{newline_pos, prev_newline_pos, line_number});
                if (source_index <= newline_pos)  {
                    current_newline_pos = newline_pos;
                    break;
                }
                prev_newline_pos = newline_pos;
                line_number += 1;
            }

            return .{
                .line = line_number, 
                .character = (source_index - prev_newline_pos), 
                .line_pos = prev_newline_pos,
                .next_line_pos = current_newline_pos
            };
        }

        pub fn render(r: *Self) !void {
            try r.build_newline_arr();

            // TODO: actually iterate errors
            // try formatError(r, r.ast.errors[0]);
            for (r.ast.errors) |ast_error| {
                try formatError(r, ast_error);
            }
        }

        fn getTokenStartLoc(r: *Self, token_index: Ast.TokenIndex) !Ast.ByteOffset {
            const tokens = r.ast.tokens;
            const token_start = tokens.items(.start)[token_index];
            var lexer = lex.Lexer.init_index(r.ast.source, token_start);
            const token = lexer.next();

            return token.loc.start;
        }

        fn skipWhitespace(r: *Self, source_index: Ast.ByteOffset) !Ast.ByteOffset {
            var i = source_index;
            while (i < r.source.len) : (i += 1) {
                if (!std.ascii.isWhitespace(r.source[i])) break;
            }
            return i;
        }
    
        fn formatError(r: *Self, ast_error: Ast.Error) !void {
            const preview_message = "";
            // const preview_message = "Preview: ";

            var error_message = switch (ast_error.tag) {
                Ast.Error.Tag.missing_colon => "Missing colon",
                Ast.Error.Tag.missing_equals => "Missing an '='",
                Ast.Error.Tag.missing_expression => "Missing an expression",
                Ast.Error.Tag.missing_fn_brace => "Malformed function block",
                Ast.Error.Tag.missing_identifier => "Missing an identifier",
                Ast.Error.Tag.missing_return_type => "Missing a return type",
                Ast.Error.Tag.missing_type_annotation => "Missing a type annotation",
                Ast.Error.Tag.unexpected_identifier => "Unexpected identifier",
                Ast.Error.Tag.unexpected_statement_token => "Unexpected statement",
                Ast.Error.Tag.unexpected_tld_token => "Unexpected statement", // TODO: better name for this?
                Ast.Error.Tag.unmatched_brace => "Unmatched brace",
                Ast.Error.Tag.unmatched_parenth => "Unmatched parenthesis",
                Ast.Error.Tag.unexpected_token => "Unexpected token",
            };

            var token_source_loc = try r.getTokenStartLoc(ast_error.token);
            var token_pos = lookup_pos(r, token_source_loc);

            r.stream.indent();
            var writer = r.stream.writer();

            try writer.print("{s}:{any}:{any}: ERROR: ",.{r.filename, token_pos.line, token_pos.character});
            try writer.writeAll(error_message);
            try writer.writeByte('\n');

            var line_pos_nw = try r.skipWhitespace(token_pos.line_pos);
            var skipped_ws = line_pos_nw - token_pos.line_pos;
            // std.debug.print("skipped_ws={any}\npreview_message.len={any}\ntoken_pos.character={}", .{skipped_ws, preview_message.len, token_pos.character});

            try writer.writeAll(preview_message);
            try writer.writeAll(r.source[line_pos_nw .. token_pos.next_line_pos]);
            try writer.writeByte('\n');

            var num_spaces = preview_message.len + token_pos.character;

            // Prevent undeflows
            if (num_spaces >= skipped_ws) num_spaces -= skipped_ws;
            if (num_spaces >= 1) num_spaces -= 1;

            try writer.writeByteNTimes(' ', num_spaces);
            try writer.writeByte('^');
            try writer.writeByte('\n');

            // Prevent undeflows
            if (num_spaces >= 2) num_spaces -= 2;

            // todo figure out if this is correct
            try writer.writeByteNTimes(' ', num_spaces);
            try writer.writeAll("HERE");
            try writer.writeByte('\n');

            r.stream.dedent();
            try writer.writeByte('\n');
        }
    };
}
