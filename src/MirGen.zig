const std = @import("std");
const hir = @import("hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Analyzer = @import("Analyzer.zig");

const math = std.math;

const Allocator = std.mem.Allocator;
const MirGen = @This();
const Hir = hir.Hir;
const HirRef = hir.Inst.Ref;
const Value = Mir.Value;
const TypedValue = Mir.TypedValue;
const Error = error { TypeError, NotImplemented };

pub const MirMap = struct {
    parent: ?*MirMap,
    start: hir.Inst.Index,
    map: []?Mir.Ref,

    pub fn init(parent: ?*MirMap) MirMap {
        return MirMap {
            .parent = parent,
            .start = 0,
            .map = &.{},
        };
    }

    pub fn deinit(m: *MirMap, a: Allocator) void {
        a.free(m.map);
    }

    pub fn ensureSliceCapacity(m: *MirMap, a: Allocator, insts: []const hir.Inst.Index) !void {
        var better_start = m.start;
        var found_start: bool = false;
        var max: u32 = 0;
        for (m.map) |entry, i| {
            if (entry) |_| {
                if (!found_start) {
                    better_start = @max(better_start, m.start + @intCast(u32, i));
                    found_start = true;
                }
                max = @max(max, m.start + @intCast(u32, i));
            }
        }
        if (m.map.len == 0) {
            better_start = std.mem.min(hir.Inst.Index, insts);
            max = std.mem.max(hir.Inst.Index, insts);
        } else {
            for (insts) |inst| {
                better_start = @min(better_start, inst);
                max = @max(max, inst);
            }
        }

        const size = max - better_start + 1;
        var new_map = try a.alloc(?Mir.Ref, size);
        var i: u32 = 0;
        while (i < size) : (i += 1) new_map[i] = null;
        for (m.map) |entry, j| {
            if (entry) |ref| new_map[j + m.start - better_start] = ref;
        }

        a.free(m.map);
        m.map = new_map;
        m.start = better_start;
    }

    pub fn putAssumeCapacity(m: *MirMap, inst: hir.Inst.Index, ref: Mir.Ref) void {
        m.map[inst - m.start] = ref;
    }

    pub fn putAssumeCapacityNoClobber(m: *MirMap, inst: hir.Inst.Index, ref: Mir.Ref) void {
        std.debug.assert(inst - m.start < m.map.len);
        std.debug.assert(m.map[inst - m.start] == null);
        m.map[inst - m.start] = ref;
    }

    pub fn get(m: *MirMap, inst: hir.Inst.Index) ?Mir.Ref {
        // std.debug.print("start = {}, inst = {}\n", .{m.start, inst});
        if (inst < m.start) return null;
        if (inst - m.start < m.map.len) {
            return m.map[inst - m.start];
        } else {
            return null;
        }
    }

    pub fn remove(m: *MirMap, inst: hir.Inst.Index) void {
        std.debug.assert(inst - m.start < m.map.len);
        m.map[inst - m.start] = null;
    }

    pub fn resolveRef(m: *MirMap, ref: hir.Inst.Ref) Mir.Ref {
        return switch (ref) {
            .zero_val => .zero_val,
            .one_val => .one_val,
            .void_val => .void_val,
            .btrue_val => .one_val,
            .bfalse_val => .zero_val,
            else => {
                // std.debug.print("ref: {}\n", .{ref});
                if (m.get(hir.Inst.refToIndex(ref).?)) |mapping| {
                    return mapping;
                } else {
                    if (m.parent) |parent| {
                        return parent.resolveRef(ref);
                    } else {
                        std.debug.print("unable to find ref {}\n", .{ref});
                        return Mir.indexToRef(0);
                    }
                }
            },
        };
    }
};

test "mirmap" {
    var m = MirMap.init();
    defer m.deinit(std.testing.allocator);

    try m.ensureSliceCapacity(std.testing.allocator, &[_]hir.Inst.Index{3, 4, 5, 8, 9, 10});
    try std.testing.expectEqual(m.start, 3);
    try std.testing.expectEqual(m.map.len, 8);

    m.putAssumeCapacity(3, @intToEnum(Mir.Ref, 100));
    m.putAssumeCapacity(4, @intToEnum(Mir.Ref, 101));
    m.putAssumeCapacity(5, @intToEnum(Mir.Ref, 102));
    m.putAssumeCapacity(9, @intToEnum(Mir.Ref, 103));
    m.putAssumeCapacity(10, @intToEnum(Mir.Ref, 104));
    try std.testing.expectEqual(m.get(3).?, @intToEnum(Mir.Ref, 100));
    try std.testing.expectEqual(m.get(4).?, @intToEnum(Mir.Ref, 101));
    try std.testing.expectEqual(m.get(5).?, @intToEnum(Mir.Ref, 102));
    try std.testing.expectEqual(m.get(9).?, @intToEnum(Mir.Ref, 103));
    try std.testing.expectEqual(m.get(10).?, @intToEnum(Mir.Ref, 104));

    m.remove(3);
    m.remove(4);
    m.remove(5);
    try std.testing.expectEqual(m.get(3), null);
    try std.testing.expectEqual(m.get(4), null);
    try std.testing.expectEqual(m.get(5), null);
    try std.testing.expectEqual(m.get(9).?, @intToEnum(Mir.Ref, 103));
    try std.testing.expectEqual(m.get(10).?, @intToEnum(Mir.Ref, 104));

    try m.ensureSliceCapacity(std.testing.allocator, &[_]hir.Inst.Index{11, 12, 13});
    try std.testing.expectEqual(m.start, 9);
    try std.testing.expectEqual(m.map.len, 5);

    try std.testing.expectEqual(m.get(9).?, @intToEnum(Mir.Ref, 103));
    try std.testing.expectEqual(m.get(10).?, @intToEnum(Mir.Ref, 104));
    try std.testing.expectEqual(m.get(11), null);
    try std.testing.expectEqual(m.get(12), null);
    try std.testing.expectEqual(m.get(13), null);

    m.putAssumeCapacity(11, @intToEnum(Mir.Ref, 105));
    m.putAssumeCapacity(12, @intToEnum(Mir.Ref, 106));
    m.putAssumeCapacity(13, @intToEnum(Mir.Ref, 107));
    try std.testing.expectEqual(m.get(9).?, @intToEnum(Mir.Ref, 103));
    try std.testing.expectEqual(m.get(10).?, @intToEnum(Mir.Ref, 104));
    try std.testing.expectEqual(m.get(11).?, @intToEnum(Mir.Ref, 105));
    try std.testing.expectEqual(m.get(12).?, @intToEnum(Mir.Ref, 106));
    try std.testing.expectEqual(m.get(13).?, @intToEnum(Mir.Ref, 107));
}

gpa: Allocator,
arena: Allocator,

hir: *const Hir,
insts: std.MultiArrayList(Mir.Inst),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(Value),
scratch: std.ArrayListUnmanaged(u32),
map: MirMap,

pub fn addInst(mg: *MirGen, inst: Mir.Inst) !Mir.Index {
    const result = @intCast(Mir.Index, mg.insts.len);
    try mg.insts.append(mg.gpa, inst);
    return result;
}

pub fn addExtra(mg: *MirGen, extra: anytype) !u32 {
    const fields = std.meta.fields(@TypeOf(extra));
    try mg.extra.ensureUnusedCapacity(mg.gpa, fields.len);
    const len = @intCast(u32, mg.extra.items.len);
    inline for (fields) |field| {
        comptime std.debug.assert(field.field_type == u32);
        mg.extra.appendAssumeCapacity(@field(extra, field.name));
    }

    return len;
}

pub fn addValue(mg: *MirGen, val: Value) !u32 {
    const len = @intCast(u32, mg.values.items.len);
    try mg.values.append(mg.gpa, val);
    return len;
}

pub fn resolveTy(mg: *MirGen, ref: Mir.Ref) !Type {
    if (Mir.refToIndex(ref)) |index| {
        return switch (mg.insts.items(.tag)[index]) {
            .constant => mg.insts.items(.data)[index].ty_pl.ty,
            .add, .sub,
            .mul, .div, .mod => mg.resolveTy(mg.insts.items(.data)[index].bin_op.lref),
            .eq, .neq, .geq,
            .leq, .gt, .lt => mg.resolveTy(mg.insts.items(.data)[index].bin_op.lref),
            .alloc => mg.resolveTy(mg.insts.items(.data)[index].op_pl.op),
            else => Error.NotImplemented,
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => .{ .tag = .comptime_int },
            .void_val => .{ .tag = .void },
            else => Error.NotImplemented,
        };
    }
}

pub fn getTy(_: *MirGen, ref: HirRef) Type {
    return .{
        .tag = switch (ref) {
            .i8_ty => .i8,
            .u8_ty => .u8,
            .i16_ty => .i16,
            .u16_ty => .u16,
            .i32_ty => .i32,
            .u32_ty => .u32,
            .i64_ty => .i64,
            .u64_ty => .u64,
            .f32_ty => .f32,
            .f64_ty => .f64,
            .bool_ty => .u1,
        }
    };
}

pub fn walkToplevel(mg: *MirGen) !void {
    const toplevel = mg.hir.insts.items(.data)[mg.hir.insts.len - 1];
    const toplevel_pl = toplevel.pl_node.pl;
    const data = mg.hir.extraData(toplevel_pl, hir.Inst.Toplevel);

    const scratch_top = mg.scratch.items.len;
    defer mg.scratch.shrinkRetainingCapacity(scratch_top);

    try mg.map.ensureSliceCapacity(mg.arena, mg.hir.extra_data[toplevel_pl + 1..toplevel_pl + 1 + data.len]);
    var extra_index: u32 = 0;
    while (extra_index < data.len) : (extra_index += 1) {
        const hir_index = mg.hir.extra_data[toplevel_pl + 1 + extra_index];
        switch (mg.hir.insts.items(.tag)[hir_index]) {
            .int => {
                const value = try mg.addValue(.{ .int = mg.hir.insts.items(.data)[hir_index].int });
                const index = try mg.addInst(.{
                    .tag = .constant,
                    .data = .{
                        .ty_pl = .{
                            .ty = .{ .tag = .comptime_int },
                            .pl = value,
                        }
                    }
                });
                try mg.scratch.append(mg.arena, index);
                mg.map.putAssumeCapacity(hir_index, Mir.indexToRef(index));
            },
            .float => {
                const value = try mg.addValue(.{
                    .float = mg.hir.insts.items(.data)[hir_index].float
                });
                const index = try mg.addInst(.{
                    .tag = .constant,
                    .data = .{
                        .ty_pl = .{
                            .ty = .{ .tag = .comptime_float },
                            .pl = value,
                        }
                    }
                });
                try mg.scratch.append(mg.arena, index);
                mg.map.putAssumeCapacity(hir_index, Mir.indexToRef(index));
            },
            .validate_ty => {
                const pl = mg.hir.insts.items(.data)[hir_index].pl_node.pl;
                const validate_ty = mg.hir.extraData(pl, hir.Inst.ValidateTy);
                const ref = mg.map.resolveRef(validate_ty.ref);
                const ty = try mg.resolveTy(ref);
                const coerces = switch (ty.tag) {
                    .comptime_int => true,
                    .comptime_float => true,
                    else => false,
                };
                std.debug.assert(coerces);
                mg.map.putAssumeCapacity(hir_index, ref);
            },
            .fn_decl => {
                const pl = mg.hir.insts.items(.data)[hir_index].pl_node.pl;
                const fn_decl = mg.hir.extraData(pl, hir.Inst.FnDecl);

                var analyzer = Analyzer {
                    .mg = mg,
                    .map = MirMap.init(&mg.map),
                    .arena = mg.arena,
                    .scratch = .{},
                };
                const index = try analyzer.analyzeBody(fn_decl.body);
                // try analyzer.analyzeBody(fn_decl.body);
                try mg.scratch.append(mg.arena, index);
                // mg.map.putAssumeCapacity(hir_index, Mir.indexToRef(index));
            },
            .alloc => {
                const op = mg.hir.insts.items(.data)[hir_index].un_node.operand;
                const ref = mg.map.resolveRef(op);
                const ty = try mg.resolveTy(ref);
                const index = try mg.addInst(.{
                    .tag = .alloc,
                    .data = .{ .ty = ty },
                });
                try mg.scratch.append(mg.arena, index);
                mg.map.putAssumeCapacity(hir_index, Mir.indexToRef(index));
            },
            // else => {},
            else => std.debug.print("unimplemented: {}\n", .{mg.hir.insts.items(.tag)[hir_index]}),
        }
    }

    const insts = mg.scratch.items[scratch_top..];
    const module_data = try mg.addExtra(Mir.Module {
        .len = @intCast(u32, insts.len),
    });
    try mg.extra.appendSlice(mg.gpa, insts);
    const module = try mg.addInst(.{
        .tag = .module,
        .data = .{ .pl = module_data },
    });
    _ = module;
}
    //
    // fn call(mg: *MirGen, inst: hir.Inst) !Mir.Ref {
    //     const hir_pl = inst.data.pl_node.pl;
    //     const hir_call = mg.hir.extraData(hir_pl, Hir.Inst.Call);
    //
    //     const pl = mg.addExtra(mir.Inst.Call {
    //         .args_len = hir_call.args_end - hir_call.args_start,
    //     });
    //     const args = mg.hir.extra_data[hir_call.args_start..hir_call.args_end];
    //     try mg.extra.appendSlice(args);
    //
    //     return try mg.addInst(.{
    //         .tag = .call,
    //         .data = .{
    //             .op_pl = .{
    //                 .op = mir.Inst.indexToRef(0), // TODO
    //                 .pl = pl,
    //             }
    //         }
    //     });
    // }
    //
    // fn binary(mg: *MirGen, inst: hir.Inst) !Mir.Ref {
    //     const hir_pl = inst.data.pl.pl_node;
    //     const hir_bin = mg.hir.extraData(hir_pl, Hir.Inst.Binary);
    //     const lref = mg.resolveInst(hir_bin.lref);
    //     const rref = mg.resolveInst(hir_bin.rref);
    //     const lty = mg.resolveTy(lref);
    //     const rty = mg.resolveTy(rref);
    //
    //     if ((lty.tag == .comptime_int) and (rty.tag == .comptime_int)) {
    //         const lpl = mg.insts[mir.Inst.refToIndex(lref)].data.ty_pl.pl;
    //         const rpl = mg.insts[mir.Inst.refToIndex(rref)].data.ty_pl.pl;
    //         const lval = mg.values[lpl];
    //         const rval = mg.values[rpl];
    //         return mg.addInst(.{
    //             .tag = .constant,
    //             .data = .{
    //                 .ty_pl = .{
    //                     .ty = .comptime_int,
    //                     .pl = try mg.addValue(.{ .int = lval + rval }),
    //                 }
    //             }
    //         });
    //     } else if (lty.tag == .comptime_int) {
    //         const lpl = mg.insts[mir.Inst.refToIndex(lref)].data.ty_pl.pl;
    //         const lval = mg.values[lpl];
    //         const tv = TypedValue {
    //             .ty = .{ .tag = .comptime_int },
    //             .val = .{ .int = @bitCast(u64, lval) },
    //         };
    //
    //         if (tv.coerce(rty)) |result| {
    //             const ref = mg.addInst(.{
    //                 .tag = .constant,
    //                 .data = .{
    //                     .ty_pl = .{
    //                         .ty = result.ty,
    //                         .pl = try mg.addValue(.{ .int = result.val }),
    //                     }
    //                 },
    //             });
    //             return mg.addInst(.{
    //                 .tag = switch (inst.tag) {
    //                     .add => .add,
    //                     .sub => .sub,
    //                     .mul => .mul,
    //                     .div => .div,
    //                     .eq => .eq,
    //                     .neq => .neq,
    //                     else => unreachable,
    //                 },
    //                 .data = .{ .bin_op = .{ .lref = lref, .rref = ref } },
    //             });
    //         }
    //     } else if (rty.tag == .comptime_int) {
    //         const rpl = mg.insts[mir.Inst.refToIndex(rref)].data.ty_pl.pl;
    //         const rval = mg.values[rpl];
    //         const tv = TypedValue {
    //             .ty = .{ .tag = .comptime_int },
    //             .val = .{ .int = @bitCast(u64, rval) },
    //         };
    //
    //         if (tv.coerce(lty)) |result| {
    //             const ref = mg.addInst(.{
    //                 .tag = .constant,
    //                 .data = .{
    //                     .ty_pl = .{
    //                         .ty = result.ty,
    //                         .pl = try mg.addValue(.{ .int = result.val }),
    //                     }
    //                 },
    //             });
    //             return mg.addInst(.{
    //                 .tag = switch (inst.tag) {
    //                     .add => .add,
    //                     .sub => .sub,
    //                     .mul => .mul,
    //                     .div => .div,
    //                     .eq => .eq,
    //                     .neq => .neq,
    //                     else => unreachable,
    //                 },
    //                 .data = .{ .bin_op = .{ .lref = ref, .lref = rref } },
    //             });
    //         }
    //     } else {
    //         switch (lty.tag) {
    //             .u1, .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
    //                 if (lty.tag != rty.tag) return Error.TypeError;
    //                 return mg.addInst(.{
    //                     .tag = switch (inst.tag) {
    //                         .add => .add,
    //                         .sub => .sub,
    //                         .mul => .mul,
    //                         .div => .div,
    //                         .eq => .eq,
    //                         .neq => .neq,
    //                         else => unreachable,
    //                     },
    //                     .data = .{ .bin_op = .{ .lref = lref, .rref = rref } },
    //                 });
    //             },
    //             else => return Error.TypeError,
    //         }
    //     }
    // }
    //
    // fn validate_ty(mg: *MirGen, inst: hir.Inst) !Mir.Ref {
    //     const data = mg.hir.extraData(inst.data.pl_node.pl, hir.Inst.ValidateTy);
    //     const ref = mg.resolveInst(data.ref);
    //     const expected = mg.getTy(data.ty);
    //     const found = mg.resolveTy(ref);
    //
    //     if (expected == found) {
    //         return ref;
    //     } else {
    //         return Error.TypeError;
    //     }
    // }
