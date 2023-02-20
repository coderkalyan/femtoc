const std = @import("std");
const hir = @import("hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const TypedValue = @import("typing.zig").TypedValue;
const MirGen = @import("MirGen.zig");

const Allocator = std.mem.Allocator;
const MirMap = MirGen.MirMap;
const Analyzer = @This();

mg: *MirGen,
map: MirMap,
scratch: std.ArrayListUnmanaged(u32),
arena: Allocator,

pub fn analyzeBody(a: *Analyzer, inst: hir.Inst.Index) !Mir.Index {
    const pl = a.mg.hir.insts.items(.data)[inst].pl_node.pl;
    const block = a.mg.hir.extraData(pl, hir.Inst.Block);

    const scratch_top = a.scratch.items.len;
    defer a.scratch.shrinkRetainingCapacity(scratch_top);

    const hir_insts = a.mg.hir.extra_data[pl + 1..pl + 1 + block.len];
    try a.map.ensureSliceCapacity(a.mg.arena, hir_insts);
    for (hir_insts) |index| {
        try a.analyzeInst(index);
    }

    const insts = a.scratch.items[scratch_top..];
    const function_data = try a.mg.addExtra(Mir.Function {
        .len = @intCast(u32, insts.len),
    });
    try a.mg.extra.appendSlice(a.mg.gpa, insts);
    const function = try a.mg.addInst(.{
        .tag = .function,
        .data = .{ .pl = function_data },
    });

    return function;
    // std.debug.print("{any}\n", .{hir_insts});
}

pub fn analyzeInst(a: *Analyzer, inst: hir.Inst.Index) !void {
    const data = a.mg.hir.insts.items(.data)[inst];
    switch (a.mg.hir.insts.items(.tag)[inst]) {
        .int => {
            const value = try a.mg.addValue(.{ .int = data.int });
            const index = try a.mg.addInst(.{
                .tag = .constant,
                .data = .{
                    .ty_pl = .{
                        .ty = .{ .tag = .comptime_int },
                        .pl = value,
                    }
                },
            });
            a.map.putAssumeCapacity(inst, Mir.indexToRef(index));
            try a.scratch.append(a.arena, index);
        },
        .float => {
            const value = try a.mg.addValue(.{ .float = data.float });
            const index = try a.mg.addInst(.{
                .tag = .constant,
                .data = .{
                    .ty_pl = .{
                        .ty = .{ .tag = .comptime_float },
                        .pl = value,
                    }
                },
            });
            a.map.putAssumeCapacity(inst, Mir.indexToRef(index));
            try a.scratch.append(a.arena, index);
        },
        .validate_ty => {
            const validate_ty = a.mg.hir.extraData(data.pl_node.pl, hir.Inst.ValidateTy);
            const ref = a.map.resolveRef(validate_ty.ref);
            const found_ty = try a.mg.resolveTy(ref);
            const expected_ty = a.mg.getTy(validate_ty.ty);

            if (found_ty.tag == expected_ty.tag) {
                a.mg.map.putAssumeCapacity(inst, ref);
            } else {
                const coerce_ref = switch (found_ty.tag) {
                    .comptime_int => ref: {
                        const int_value: u64 = if (Mir.refToIndex(ref)) |value_index|
                            a.mg.hir.insts.items(.data)[value_index].int
                        else switch (ref) {
                            .zero_val => 0,
                            .one_val => 1,
                            else => return error.InvalidRef,
                        };
                        const value = try a.mg.addValue(.{ .int = int_value });
                        const index = try a.mg.addInst(.{
                            .tag = .constant,
                            .data = .{
                                .ty_pl = .{
                                    .ty = expected_ty,
                                    .pl = value,
                                }
                            }
                        });
                        try a.mg.scratch.append(a.mg.arena, index);
                    break :ref Mir.indexToRef(index);
                    },
                    .comptime_float => ref: {
                        const float_value: f64 = if (Mir.refToIndex(ref)) |value_index|
                            a.mg.hir.insts.items(.data)[value_index].float
                        else
                            return error.InvalidRef;
                        const value = try a.mg.addValue(.{ .float = float_value });
                        const index = try a.mg.addInst(.{
                            .tag = .constant,
                            .data = .{
                                .ty_pl = .{
                                    .ty = expected_ty,
                                    .pl = value,
                                }
                            }
                        });
                        try a.mg.scratch.append(a.mg.arena, index);
                    break :ref Mir.indexToRef(index);
                    },
                    else => return error.TypeError,
                };
                a.mg.map.putAssumeCapacity(inst, coerce_ref);
            }
        },
        .alloc => {
            const ref = a.map.resolveRef(data.un_node.operand);
            const ty = try a.mg.resolveTy(ref);
            const index = try a.mg.addInst(.{
                .tag = .alloc,
                .data = .{ .ty = ty },
            });
            a.map.putAssumeCapacity(inst, Mir.indexToRef(index));
            try a.scratch.append(a.arena, index);
        },
        .load_inline => {
            const ref = a.mg.hir.resolution_map.get(data.pl_node.pl).?;
            // std.debug.print("ref: {}\n", .{ref});
            a.map.putAssumeCapacity(inst, a.map.resolveRef(ref));
        },
        .load => {
            const ref = a.mg.hir.resolution_map.get(data.pl_node.pl).?;
            const index = try a.mg.addInst(.{
                .tag = .load,
                .data = .{ .un_op = a.map.resolveRef(ref) },
            });
            a.map.putAssumeCapacity(inst, Mir.indexToRef(index));
            try a.scratch.append(a.arena, index);
        },
        .store => {
            const store = a.mg.hir.extraData(data.pl_node.pl, hir.Inst.Store);
            const addr = a.map.resolveRef(hir.Inst.indexToRef(store.addr));
            const val = a.map.resolveRef(store.val);
            const index = try a.mg.addInst(.{
                .tag = .store,
                .data = .{
                    .bin_op = .{ .lref = addr, .rref = val }
                },
            });
            a.map.putAssumeCapacity(inst, Mir.indexToRef(index));
            try a.scratch.append(a.arena, index);
        },
        else => {
            std.debug.print("unimplemented: {}\n", .{a.mg.hir.insts.items(.tag)[inst]});
            return;
            // return Mir.indexToRef(0);
        },
    }
}


