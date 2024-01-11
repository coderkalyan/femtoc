const std = @import("std");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const Fir = @import("fir/Fir.zig");
const Air = @import("air/Air.zig");
const InternPool = @import("InternPool.zig");
const Type = @import("air/type.zig").Type;
const Decl = Air.Decl;
const Allocator = std.mem.Allocator;

const io = std.io;

const Node = Ast.Node;

const indenting_writer = @import("fmt/indenting_writer.zig");

const IndentingWriter = indenting_writer.IndentingWriter;
const indentingWriter = indenting_writer.indentingWriter;

pub fn FirRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        fir: *const Fir,
        arena: Allocator,

        pub const Self = @This();

        pub fn init(writer: anytype, arena: Allocator, fir: *const Fir) Self {
            return .{
                .stream = indentingWriter(width, writer),
                .arena = arena,
                .fir = fir,
            };
        }

        pub fn render(self: *Self) !void {
            const fir = self.fir;

            const module_pl = fir.insts.items(.data)[@intFromEnum(fir.module_index)].module.insts;
            const slice = fir.extraData(Fir.Inst.ExtraSlice, module_pl);
            const globals = fir.extraSlice(slice);
            for (globals) |global| {
                try self.renderInst(@enumFromInt(global));
            }
        }

        pub fn renderInst(self: *Self, inst: Fir.Index) !void {
            const fir = self.fir;
            const writer = self.stream.writer();

            try writer.print("%{} = ", .{@intFromEnum(inst)});

            switch (fir.get(inst).data) {
                .global => |global| {
                    const ident = fir.pool.getString(global.name).?;
                    try writer.print("[global] {s}: ", .{ident});
                    try self.renderInst(global.block);
                },
                .function => |function| {
                    try writer.print("function(%{}) body: ", .{@intFromEnum(function.signature)});
                    try self.renderInst(function.body);
                },
                .block => |block| {
                    try writer.print("block {{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const slice = fir.extraData(Fir.Inst.ExtraSlice, block.insts);
                    const insts = fir.extraSlice(slice);
                    for (insts) |block_inst| {
                        try self.renderInst(@enumFromInt(block_inst));
                    }

                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .block_inline => |block| {
                    try writer.print("block_inline {{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const slice = fir.extraData(Fir.Inst.ExtraSlice, block.insts);
                    const insts = fir.extraSlice(slice);
                    for (insts) |block_inst| {
                        try self.renderInst(@enumFromInt(block_inst));
                    }

                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .branch_single => |branch_single| {
                    const cond = @intFromEnum(branch_single.cond);
                    try writer.print("branch_single(%{}) true: ", .{cond});
                    try self.renderInst(branch_single.exec_true);
                },
                .branch_double => |data| {
                    const cond = @intFromEnum(data.cond);
                    const branch_double = fir.extraData(Fir.Inst.BranchDouble, data.pl);
                    try writer.print("branch_double(%{}) {{", .{cond});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("true: ", .{});
                    try self.renderInst(branch_double.exec_true);
                    try writer.print("false: ", .{});
                    try self.renderInst(branch_double.exec_false);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .loop_forever => |loop| {
                    try writer.print("loop_forever {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("body: ", .{});
                    try self.renderInst(loop.body);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .loop_while => |loop| {
                    try writer.print("loop_while {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("condition: ", .{});
                    try self.renderInst(loop.cond);
                    try writer.print("body: ", .{});
                    try self.renderInst(loop.body);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .load_global => |load_global| {
                    // const inst = ir.untyped_decls.get(pl).?;
                    // try writer.print("load_global(%{}) [{s}]", .{ inst, ident });
                    const ident = fir.pool.getString(load_global.name).?;
                    try writer.print("load_global({s})", .{ident});
                    try self.stream.newline();
                },
                .call => |call| {
                    try writer.print("call(%{}", .{@intFromEnum(call.function)});
                    const slice = fir.extraData(Fir.Inst.ExtraSlice, call.args);
                    const args = fir.extraSlice(slice);
                    for (args) |arg| {
                        try writer.print(", %{}", .{arg});
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
                .function_type => |function_type| {
                    try writer.print("function_type((", .{});
                    const slice = fir.extraData(Fir.Inst.ExtraSlice, function_type.params);
                    const params = fir.extraSlice(slice);
                    for (params, 0..) |param, i| {
                        try writer.print("%{}", .{param});
                        if (i < params.len - 1) try writer.print(", ", .{});
                    }
                    try writer.print(") %{})", .{@intFromEnum(function_type.@"return")});
                    try self.stream.newline();
                },
                .builtin_type => {
                    const node = fir.locs[@intFromEnum(inst)].node;
                    const main_token = fir.tree.mainToken(node);
                    const type_str = fir.tree.tokenString(main_token);
                    try writer.print("builtin_type({s})", .{type_str});
                    try self.stream.newline();
                },
                .array => |array| {
                    const slice = fir.extraData(Fir.Inst.ExtraSlice, array);
                    const elements = fir.extraSlice(slice);
                    try writer.print("array(", .{});
                    for (elements, 0..) |element, i| {
                        try writer.print("%{}", .{element});
                        if (i < elements.len - 1) try writer.print(", ", .{});
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
                .slice => |slice| {
                    const data = fir.extraData(Fir.Inst.Slice, slice.pl);
                    try writer.print("slice(base %{}, start %{}, end %{})", .{ @intFromEnum(data.base), @intFromEnum(data.start), @intFromEnum(data.end) });
                    try self.stream.newline();
                },
                inline else => |data| {
                    try writer.print("{s}(", .{@tagName(fir.tag(inst))});
                    switch (@TypeOf(data)) {
                        void => {},
                        inline u32, u64, f64, bool => try writer.print("{}", .{data}),
                        Fir.Index => try writer.print("%{}", .{@intFromEnum(data)}),
                        InternPool.StringIndex => {
                            const str = fir.pool.getString(data).?;
                            try writer.print("\"{s}\"", .{str});
                        },
                        else => {
                            const fields = std.meta.fields(@TypeOf(data));
                            inline for (fields, 0..) |field, i| {
                                const arg = @field(data, field.name);
                                switch (field.type) {
                                    u32 => try writer.print("{}", .{arg}),
                                    Fir.Index => try writer.print("%{}", .{@intFromEnum(arg)}),
                                    else => try writer.print("format error", .{}),
                                }

                                if (i < fields.len - 1) try writer.print(", ", .{});
                            }
                        },
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
            }
        }
    };
}

pub fn AirRenderer(comptime width: u32, comptime WriterType: anytype) type {
    return struct {
        stream: IndentingWriter(width, WriterType),
        pool: *InternPool,
        arena: Allocator,

        pub const Self = @This();

        pub fn init(writer: anytype, arena: Allocator, pool: *InternPool) Self {
            return .{
                .stream = indentingWriter(width, writer),
                .arena = arena,
                .pool = pool,
            };
        }

        pub fn renderAllDecls(self: *Self) !void {
            var iterator = self.pool.decls.iterator(0);
            while (iterator.next()) |decl| {
                try self.renderDecl(decl);
            }
        }

        pub fn renderAllBodies(self: *Self) !void {
            var iterator = self.pool.bodies.iterator(0);
            while (iterator.next()) |body| {
                try self.renderBody(body);
            }
        }

        fn renderDecl(self: *Self, decl: *const Decl) !void {
            const writer = self.stream.writer();

            const name = switch (decl.name) {
                .named => |name| self.pool.getString(name).?,
                .unnamed => "unnamed",
            };
            try writer.print("Air decl: {s}", .{name});
            try self.stream.newline();
            try writer.print("mutable: {}", .{decl.mutable});
            try self.stream.newline();
            try writer.print("linkage: {}", .{decl.linkage});
            try self.stream.newline();
            try self.stream.newline();
        }

        fn renderBody(self: *Self, body: *const Air) !void {
            try self.renderInst(body, body.toplevel);
            try self.stream.newline();
        }

        pub fn renderInst(self: *Self, air: *const Air, inst: Air.Index) !void {
            const writer = self.stream.writer();

            const x = @intFromEnum(inst);
            try writer.print("%{} = ", .{x});

            const air_inst = air.insts.get(x);
            switch (air_inst) {
                .block => |block| {
                    try writer.print("block {{", .{});
                    self.stream.indent();
                    try self.stream.newline();

                    const slice = air.extraData(Air.Inst.ExtraSlice, block.insts);
                    const insts = air.extraSlice(slice);
                    for (insts) |block_inst| {
                        try self.renderInst(air, @enumFromInt(block_inst));
                    }

                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .branch_single => |branch_single| {
                    const cond = @intFromEnum(branch_single.cond);
                    try writer.print("branch_single(%{}) true: ", .{cond});
                    try self.renderInst(air, branch_single.exec_true);
                },
                .branch_double => |data| {
                    const cond = @intFromEnum(data.cond);
                    const branch_double = air.extraData(Air.Inst.BranchDouble, data.pl);
                    try writer.print("branch_double(%{}) {{", .{cond});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("true: ", .{});
                    try self.renderInst(air, branch_double.exec_true);
                    try writer.print("false: ", .{});
                    try self.renderInst(air, branch_double.exec_false);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .loop_while => |loop| {
                    try writer.print("loop_while {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("condition: ", .{});
                    try self.renderInst(air, loop.cond);
                    try writer.print("body: ", .{});
                    try self.renderInst(air, loop.body);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .loop_forever => |loop| {
                    try writer.print("loop_forever {{", .{});
                    self.stream.indent();
                    try self.stream.newline();
                    try writer.print("body: ", .{});
                    try self.renderInst(air, loop.body);
                    self.stream.dedent();
                    try writer.print("}}", .{});
                    try self.stream.newline();
                },
                .alloc => |alloc| {
                    try writer.print("alloc({s}) [{s}]", .{
                        try self.formatInterned(alloc.slot_type),
                        try self.formatInterned(alloc.pointer_type),
                    });
                    try self.stream.newline();
                },
                // .load_global => |load_global| {
                //     // const inst = ir.untyped_decls.get(pl).?;
                //     // try writer.print("load_global(%{}) [{s}]", .{ inst, ident });
                //     const ident = air.pool.getString(load_global.name).?;
                //     try writer.print("load_global({s})", .{ident});
                //     try self.stream.newline();
                // },
                .param => |param| {
                    const param_str = air.pool.getString(param.name).?;
                    try writer.print("param(%{s}, \"{s}\")", .{ try self.formatInterned(param.ty), param_str });
                    try self.stream.newline();
                },
                .call => |call| {
                    try writer.print("call(%{}", .{@intFromEnum(call.function)});
                    const slice = air.extraData(Air.Inst.ExtraSlice, call.args);
                    const args = air.extraSlice(slice);
                    for (args) |arg| {
                        try writer.print(", %{}", .{arg});
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
                .slice_init => |slice_init| {
                    const data = air.extraData(Air.Inst.SliceInit, slice_init.pl);
                    const ty = try self.formatInterned(data.ty);
                    try writer.print("slice_init({s}, ptr %{}, len %{})", .{ ty, @intFromEnum(data.ptr), @intFromEnum(data.len) });
                    try self.stream.newline();
                },
                .array_init => |array| {
                    const slice = air.extraData(Air.Inst.ExtraSlice, array.elements);
                    const elements = air.extraSlice(slice);
                    try writer.print("array({s}", .{try self.formatInterned(array.ty)});
                    for (elements, 0..) |element, i| {
                        try writer.print("%{}", .{element});
                        if (i < elements.len - 1) try writer.print(", ", .{});
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
                .index_ref => |index_ref| {
                    const data = air.extraData(Air.Inst.IndexRef, index_ref.pl);
                    const ty = try self.formatInterned(data.ty);
                    try writer.print("index_ref({s}, base %{}, idx %{})", .{ ty, @intFromEnum(data.base), @intFromEnum(data.index) });
                    try self.stream.newline();
                },
                inline else => |data| {
                    try writer.print("{s}(", .{@tagName(@as(std.meta.Tag(Air.Inst), air_inst))});
                    switch (@TypeOf(data)) {
                        void => {},
                        inline u32, u64, f64 => try writer.print("{}", .{data}),
                        Air.Index => try writer.print("%{}", .{@intFromEnum(data)}),
                        InternPool.Index => try writer.print("{s}", .{try self.formatInterned(data)}),
                        else => {
                            const fields = std.meta.fields(@TypeOf(data));
                            inline for (fields, 0..) |field, i| {
                                const arg = @field(data, field.name);
                                switch (field.type) {
                                    u32 => try writer.print("{}", .{arg}),
                                    Air.Index => try writer.print("%{}", .{@intFromEnum(arg)}),
                                    InternPool.Index => try writer.print("{s}", .{try self.formatInterned(arg)}),
                                    else => try writer.print("format error", .{}),
                                }

                                if (i < fields.len - 1) try writer.print(", ", .{});
                            }
                        },
                    }
                    try writer.print(")", .{});
                    try self.stream.newline();
                },
            }
        }

        pub fn formatInterned(self: *Self, ip_index: InternPool.Index) ![]const u8 {
            return switch (self.pool.indexToKey(ip_index)) {
                .ty => |ty| switch (ty) {
                    .void => "void",
                    .comptime_int => |int| switch (int.sign) {
                        .unsigned => "comptime_uint",
                        .signed => "comptime_sint",
                    },
                    .int => |int| switch (int.sign) {
                        .unsigned => std.fmt.allocPrint(self.arena, "u{}", .{int.width}),
                        .signed => std.fmt.allocPrint(self.arena, "i{}", .{int.width}),
                    },
                    .comptime_float => "comptime_float",
                    .float => |float| std.fmt.allocPrint(self.arena, "f{}", .{float.width}),
                    .bool => "bool",
                    .ref => |ref| switch (ref.mutable) {
                        false => std.fmt.allocPrint(self.arena, "{s} ref", .{try self.formatInterned(ref.pointee)}),
                        true => std.fmt.allocPrint(self.arena, "{s} ref mut", .{try self.formatInterned(ref.pointee)}),
                    },
                    .pointer => |pointer| std.fmt.allocPrint(self.arena, "{s}*", .{try self.formatInterned(pointer.pointee)}),
                    .many_pointer => |pointer| std.fmt.allocPrint(self.arena, "{s}[*]", .{try self.formatInterned(pointer.pointee)}),
                    .slice => |slice| std.fmt.allocPrint(self.arena, "{s}[]", .{try self.formatInterned(slice.element)}),
                    .array => |array| std.fmt.allocPrint(self.arena, "{s}[{}]", .{ try self.formatInterned(array.element), array.count }),
                    .@"struct" => "struct unimplemented",
                    .function => |_| "function unimplemented",
                },
                .tv => |tv| {
                    const ty = try self.formatInterned(tv.ty);
                    const val = switch (tv.val) {
                        .none => "none",
                        inline .integer, .float => |num| try std.fmt.allocPrint(self.arena, "{}", .{num}),
                        .array => |array| fmt: {
                            var str = try std.fmt.allocPrint(self.arena, "[", .{});
                            const elements = self.pool.extra.items[array.start..array.end];
                            for (elements, 0..) |element, i| {
                                const interned = try self.formatInterned(@enumFromInt(element));
                                str = try std.fmt.allocPrint(self.arena, "{s}{s}", .{ str, interned });
                                if (i < elements.len - 1) str = try std.fmt.allocPrint(self.arena, "{s}, ", .{str});
                            }
                            str = try std.fmt.allocPrint(self.arena, "{s}]", .{str});
                            break :fmt str;
                        },
                        .body => "body unimplemented",
                        .string => |string| self.pool.getString(string).?,
                    };
                    return std.fmt.allocPrint(self.arena, "{s}({s})", .{ ty, val });
                },
                .decl => |decl_index| {
                    const decl = self.pool.decls.at(@intFromEnum(decl_index));
                    return switch (decl.name) {
                        .named => |name| self.pool.getString(name).?,
                        .unnamed => "unnamed",
                    };
                },
                .field_map => "field map unimplemented", // TODO: do we need this?
            };
        }
    };
}
