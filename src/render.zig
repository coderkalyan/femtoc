const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Hir = @import("Hir.zig");
const Type = @import("typing.zig").Type;
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

            try writer.print("%{} = ", .{index});
            var lbuf: [256]u8 = [_]u8{0} ** 256;
            var rbuf: [256]u8 = [_]u8{0} ** 256;
            switch (ir.insts.items(.tag)[index]) {
                .int => try writer.print("int({})", .{ir.insts.items(.data)[index].int}),
                .float => try writer.print("float({})", .{ir.insts.items(.data)[index].float}),
                .add, .sub, .mul, .div, .mod, .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge, .cmp_lt, .cmp_gt, .icmp_eq, .icmp_ne, .icmp_ugt, .icmp_uge, .icmp_ult, .icmp_ule, .icmp_sgt, .icmp_sge, .icmp_slt, .icmp_sle, .fcmp_gt, .fcmp_ge, .fcmp_lt, .fcmp_le => {
                    try writer.writeAll(switch (ir.insts.items(.tag)[index]) {
                        .add => "add",
                        .sub => "sub",
                        .mul => "mul",
                        .div => "div",
                        .mod => "mod",
                        .cmp_eq => "cmp_eq",
                        .cmp_ne => "cmp_ne",
                        .cmp_gt => "cmp_gt",
                        .cmp_ge => "cmp_ge",
                        .cmp_lt => "cmp_lt",
                        .cmp_le => "cmp_le",
                        .icmp_eq => "icmp_eq",
                        .icmp_ne => "icmp_ne",
                        .icmp_ugt => "icmp_ugt",
                        .icmp_uge => "icmp_uge",
                        .icmp_ult => "icmp_ult",
                        .icmp_ule => "icmp_ule",
                        .icmp_sgt => "icmp_sgt",
                        .icmp_sge => "icmp_sge",
                        .icmp_slt => "icmp_slt",
                        .icmp_sle => "icmp_le",
                        .fcmp_gt => "fcmp_gt",
                        .fcmp_ge => "fcmp_ge",
                        .fcmp_lt => "fcmp_lt",
                        .fcmp_le => "fcmp_le",
                        else => unreachable,
                    });

                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const bin = ir.extraData(pl, Hir.Inst.Binary);
                    try self.formatRef(bin.lref, &lbuf);
                    try self.formatRef(bin.rref, &rbuf);
                    try writer.print("({s}, {s})", .{ lbuf, rbuf });
                },
                .coerce => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const coerce = ir.extraData(pl, Hir.Inst.Coerce);
                    try self.formatRef(coerce.ty, &lbuf);
                    try self.formatRef(coerce.val, &rbuf);
                    try writer.print("coerce({s}, {s})", .{ lbuf, rbuf });
                },
                .zext => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const zext = ir.extraData(pl, Hir.Inst.Extend);
                    try self.formatRef(zext.ty, &lbuf);
                    try self.formatRef(zext.val, &rbuf);
                    try writer.print("zext({s}, {s})", .{ lbuf, rbuf });
                },
                .sext => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const sext = ir.extraData(pl, Hir.Inst.Extend);
                    try self.formatRef(sext.ty, &lbuf);
                    try self.formatRef(sext.val, &rbuf);
                    try writer.print("sext({s}, {s})", .{ lbuf, rbuf });
                },
                .fpext => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const fpext = ir.extraData(pl, Hir.Inst.Extend);
                    try self.formatRef(fpext.ty, &lbuf);
                    try self.formatRef(fpext.val, &rbuf);
                    try writer.print("fpext({s}, {s})", .{ lbuf, rbuf });
                },
                .push => {
                    try self.formatRef(ir.insts.items(.data)[index].un_node.operand, &lbuf);
                    try writer.print("push({s})", .{lbuf});
                },
                .alloca => {
                    try self.formatRef(ir.insts.items(.data)[index].un_node.operand, &lbuf);
                    try writer.print("alloca({s})", .{lbuf});
                },
                .global => {
                    try self.formatRef(ir.insts.items(.data)[index].un_node.operand, &lbuf);
                    try writer.print("global({s})", .{lbuf});
                },
                .global_mut => {
                    try self.formatRef(ir.insts.items(.data)[index].un_node.operand, &lbuf);
                    try writer.print("global_mut({s})", .{lbuf});
                },
                .store => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const store = ir.extraData(pl, Hir.Inst.Store);
                    try self.formatIndex(store.addr, &lbuf);
                    try self.formatRef(store.val, &rbuf);
                    try writer.print("store({s}, {s})", .{ lbuf, rbuf });
                },
                .load => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    try self.formatIndex(pl, &lbuf);
                    try writer.print("load({s})", .{lbuf});
                },
                .load_global => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const inst = ir.untyped_decls.get(pl).?;
                    const ident = try ir.interner.get(pl);
                    try self.formatIndex(inst, &lbuf);
                    try writer.print("load_global({s}) [{s}]", .{ ident, lbuf });
                },
                .fn_decl => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const fn_decl = ir.extraData(pl, Hir.Inst.FnDecl);

                    try self.formatRef(fn_decl.return_type, &lbuf);
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
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const param = ir.extraData(pl, Hir.Inst.Param);

                    const param_str = try ir.interner.get(param.name);
                    try self.formatRef(param.ty, &lbuf);
                    try writer.print("param(\"{s}\", {s})", .{ param_str, lbuf });
                },
                .block => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const block = ir.extraData(pl, Hir.Inst.Block);

                    try writer.print("block({{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const insts = ir.block_slices[block.head];
                    for (insts) |inst| {
                        try self.renderInst(inst);
                    }

                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .block_inline => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const block = ir.extraData(pl, Hir.Inst.Block);

                    try writer.print("block_inline({{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const insts = ir.block_slices[block.head];
                    for (insts) |inst| {
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
                    const operand = ir.insts.items(.data)[index].un_tok.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("ret_implicit({s})", .{lbuf});
                },
                .ret_node => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("ret_node({s})", .{lbuf});
                },
                .yield_implicit => {
                    const operand = ir.insts.items(.data)[index].un_tok.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("yield_implicit({s})", .{lbuf});
                },
                .yield_node => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("yield_node({s})", .{lbuf});
                },
                .yield_inline => {
                    const operand = ir.insts.items(.data)[index].un_node.operand;
                    try self.formatRef(operand, &lbuf);
                    try writer.print("yield_inline({s})", .{lbuf});
                },
                .loop => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const loop = ir.extraData(pl, Hir.Inst.Loop);

                    try writer.print("loop(", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("condition = {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(loop.condition);
                    self.stream.dedent();
                    try writer.print("}}, body = {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try self.renderInst(loop.body);
                    self.stream.dedent();
                    self.stream.dedent();
                    try writer.print("}})", .{});
                },
                .loop_break => {
                    try writer.print("break()", .{});
                },
                .call => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const call = ir.extraData(pl, Hir.Inst.Call);
                    try self.formatRef(call.addr, &lbuf);
                    try writer.print("call({s}", .{lbuf});

                    var extra_index: u32 = 0;
                    while (extra_index < call.args_len) : (extra_index += 1) {
                        const arg = ir.extra_data[pl + 2 + extra_index];
                        try self.formatRef(@enumFromInt(arg), &rbuf);
                        try writer.print(", {s}", .{rbuf});
                    }

                    try writer.print(")", .{});
                },
                .dbg_value => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const data = ir.extraData(pl, Hir.Inst.DebugValue);
                    const ident_str = try ir.interner.get(data.name);
                    try self.formatRef(data.value, &lbuf);
                    try writer.print("dbg_value({s}, {s})", .{ ident_str, lbuf });
                },
                .constant => {
                    const pl = ir.insts.items(.data)[index].pl_node.pl;
                    const data = ir.extraData(pl, Hir.Inst.Constant);
                    const ty = ir.resolveType(data.ty);
                    try self.formatRef(data.ty, &lbuf);
                    switch (ty.kind()) {
                        .comptime_uint, .uint => {
                            const val = ir.refToInt(Hir.Inst.indexToRef(index));
                            try writer.print("constant({s}, {})", .{ lbuf, val });
                        },
                        .comptime_sint, .sint => {
                            const val = ir.refToInt(Hir.Inst.indexToRef(index));
                            const signed: i32 = @bitCast(@as(u32, @truncate(val)));
                            try writer.print("constant({s}, {})", .{ lbuf, signed });
                        },
                        .comptime_float, .float => {
                            const val = ir.refToFloat(Hir.Inst.indexToRef(index));
                            try writer.print("constant({s}, {})", .{ lbuf, val });
                        },
                        .function => {
                            const val = ir.values[data.val];
                            const payload = val.payload.cast(Value.Payload.Function).?;
                            const func = payload.func;

                            try writer.print("constant({s}, body={{", .{lbuf});
                            self.stream.indent();
                            try self.stream.newline();
                            try self.renderInst(func.body);
                            self.stream.dedent();
                            try writer.print("}})", .{});
                            // try writer.print("func(ret_ty={s}, params={{", .{lbuf});
                            // self.stream.indent();
                            // self.stream.indent();
                            // try self.stream.newline();
                            // var extra_index: u32 = fn_decl.params_start;
                            // while (extra_index < fn_decl.params_end) : (extra_index += 1) {
                            //     const param = ir.extra_data[extra_index];
                            //     try self.renderInst(param);
                            // }
                        },
                        else => {},
                    }
                },
                .ty => {
                    const ty = ir.insts.items(.data)[index].ty;
                    try self.formatType(ty, &lbuf);
                    try writer.print("type({s})", .{lbuf});
                },
                else => {
                    try writer.print("{}", .{ir.insts.items(.tag)[index]});
                },
            }

            try self.stream.newline();
        }

        fn formatRef(self: *Self, ref: Hir.Ref, buf: []u8) !void {
            @memset(buf, 0);
            const resolved_ref = self.hir.resolveRef(ref);
            if (Hir.Inst.refToIndex(resolved_ref)) |index| {
                _ = try std.fmt.bufPrint(buf, "%{}", .{index});
            } else {
                try self.formatPrimitiveRef(ref, buf);
            }
        }

        fn formatPrimitiveRef(_: *Self, ref: Hir.Ref, buf: []u8) !void {
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
                .comptime_uint => "comptime_uint",
                .comptime_sint => "comptime_sint",
                .comptime_float => "comptime_float",
                _ => unreachable,
            }});
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
                    std.debug.print("ret ty: {}\n", .{function.return_type.basic});
                    try self.formatType(function.return_type, &ret_buf);
                    _ = try std.fmt.bufPrint(buf, "function(return_ty = {s})", .{ret_buf});
                    return;
                },
                else => unreachable, //return error.NotImplemented,
            }});
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
