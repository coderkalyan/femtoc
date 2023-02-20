const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Analyzer = @import("Analyzer.zig");
const MirMap = @import("MirMap.zig");

const math = std.math;

const Allocator = std.mem.Allocator;
const MirGen = @This();
const Value = Mir.Value;
const TypedValue = Mir.TypedValue;
const Error = error { TypeError, NotImplemented };

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

pub fn walkToplevel(mg: *MirGen) !void {
    const toplevel = mg.hir.insts.items(.data)[mg.hir.insts.len - 1];
    const toplevel_pl = toplevel.pl_node.pl;
    const data = mg.hir.extraData(toplevel_pl, Hir.Inst.Module);

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
                const validate_ty = mg.hir.extraData(pl, Hir.Inst.ValidateTy);
                const ref = mg.map.resolveRef(validate_ty.ref);
                const found_ty = try mg.resolveTy(ref);
                const expected_ty = validate_ty.ty.toType();

                if (found_ty.tag == expected_ty.tag) {
                    mg.map.putAssumeCapacity(hir_index, ref);
                } else {
                    const coerce_ref = switch (found_ty.tag) {
                        .comptime_int => ref: {
                            const value_index = Mir.refToIndex(ref).?;
                            const int_value = mg.hir.insts.items(.data)[value_index].int;
                            const value = try mg.addValue(.{ .int = int_value });
                            const index = try mg.addInst(.{
                                .tag = .constant,
                                .data = .{
                                    .ty_pl = .{
                                        .ty = expected_ty,
                                        .pl = value,
                                    }
                                }
                            });
                            try mg.scratch.append(mg.arena, index);
                            break :ref Mir.indexToRef(index);
                        },
                        else => return Error.TypeError,
                    };
                    mg.map.putAssumeCapacity(hir_index, coerce_ref);
                }
            },
            .fn_decl => {
                const pl = mg.hir.insts.items(.data)[hir_index].pl_node.pl;
                const fn_decl = mg.hir.extraData(pl, Hir.Inst.FnDecl);

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
    // fn call(mg: *MirGen, inst: Hir.Inst) !Mir.Ref {
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
    // fn binary(mg: *MirGen, inst: Hir.Inst) !Mir.Ref {
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
