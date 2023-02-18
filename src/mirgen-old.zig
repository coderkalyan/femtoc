const std = @import("std");
const hir = @import("hir.zig");
const mir = @import("mir.zig");
const typing = @import("typing.zig");

const math = std.math;
const mem = std.mem;

const Hir = hir.Hir;
const Allocator = std.mem.Allocator;
const HirRef = hir.Inst.Ref;
const MirRef = mir.Inst.Ref;
const Type = typing.Type;
const TypedValue = typing.TypedValue;
const Value = mir.Value;
const Error = error {TypeError};

// TODO: this was grabbed from zig source, figure out what exactly is going on
pub const InstMap = struct {
    items: []MirRef = &[_]MirRef{},
    start: Hir.Inst.Index = 0,

    pub fn deinit(map: InstMap, allocator: Allocator) void {
        allocator.free(map.items);
    }

    pub fn get(map: InstMap, key: hir.Inst.Index) ?MirRef {
        if (!map.contains(key)) return null;
        return map.items[key - map.start];
    }

    pub fn putAssumeCapacity(map: *InstMap, key: hir.Inst.Index, ref: MirRef) void {
        map.items[key - map.start] = ref;
    }

    pub fn putAssumeCapacityNoClobber(map: *InstMap, key: hir.Inst.Index, ref: MirRef) void {
        std.debug.assert(!map.contains(key));
        map.putAssumeCapacity(key, ref);
    }

    pub fn remove(map: *InstMap, key: hir.Inst.Index) bool {
        if (!map.contains(key)) return false;
        map.items[key - map.start] = .none;
        return true;
    }

    pub fn contains(map: *InstMap, key: hir.Inst.Index) bool {
        return map.items[key - map.start] != .none;
    }

    pub fn ensureSpaceForInstructions(
        map: *InstMap, 
        allocator: mem.Allocator,
        insts: []const hir.Inst.Index,
    ) !void {
        const min_max = mem.minMax(hir.Inst.Index, insts);
        const start = min_max.min;
        const end = min_max.max;
        if (map.start <= start and end < map.items.len + map.start)
            return;

        const old_start = if (map.items.len == 0) start else map.start;
        var better_capacity = map.items.len;
        var better_start = old_start;
        while (true) {
            const extra_capacity = better_capacity / 2 + 16;
            better_capacity += extra_capacity;
            better_start -|= @intCast(hir.Inst.Index, extra_capacity / 2);
            if (better_start <= start and end < better_capacity + better_start)
                break;
        }

        const start_diff = old_start - better_start;
        const new_items = try allocator.alloc(MirRef, better_capacity);
        mem.set(MirRef, new_items[0..start_diff], .none);
        mem.copy(MirRef, new_items[start_diff..], map.items);
        mem.set(MirRef, new_items[start_diff + map.items.len..], .none);

        allocator.free(map.items);
        map.items = new_items;
        map.start = @intCast(hir.Inst.Index, better_start);
    }
};

const Block = struct {
    parent: ?*Block,
    // single generator for all (child) blocks
    mg: *MirGen,
};

pub const MirGen = struct {
    gpa: Allocator,
    arena: Allocator,

    hir: *const Hir,
    insts: std.MultiArrayList(mir.Inst),
    extra: std.ArrayListUnmanaged(u32),
    values: std.ArrayListUnmanaged(Value),
    scratch: std.ArrayListUnmanaged(u32),

    fn addInst(mg: *MirGen, inst: mir.Inst) !MirRef {
        const result = @intCast(mir.Inst.Index, mg.insts.len);
        try mg.insts.append(inst);
        return mir.Inst.indexToRef(result);
    }

    fn addExtra(mg: *MirGen, extra: anytype) Allocator.Error!u32 {
        const fields = std.meta.fields(@TypeOf(extra));
        try mg.extra.ensureUnusedCapacity(fields.len);
        const len = @intCast(u32, mg.extra.items.len);
        inline for (fields) |field| {
            comptime std.debug.assert(field.field_type == u32);
            mg.extra.appendAssumeCapacity(@field(extra, field.name));
        }

        return len;
    }

    fn addValue(mg: *MirGen, val: Value) Allocator.Error!u32 {
        const len = @intCast(u32, mg.values.items.len);
        mg.values.append(val);
        return len;
    }

    fn resolveTy(mg: *MirGen, ref: MirRef) !Type {
        if (mir.Inst.refIsIndex(ref)) {
            const index = try mir.Inst.refToIndex(ref);
            return switch (mg.inst.items(.tag)[index]) {
                .constant => mg.inst.items(.data)[index].ty_pl.ty,
                .add, .sub,
                .mul, .div, .mod => mg.resolveTy(mg.inst.items(.data)[index].bin_op.lref),
                .eq, .neq, .geq,
                .leq, .gt, .lt => mg.resolveTy(mg.inst.items(.data)[index].bin_op.lref),
                .alloc => mg.resolveTy(mg.inst.items(.data)[index].op_pl.op),
                else => unreachable,
            };
        } else {
            return switch (ref) {
                .izero_val, .ione_val => .{ .tag = .comptime_int },
                .fzero_val, .fone_val => .{ .tag = .comptime_float },
                else => unreachable,
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

    pub fn genFunction(mg: *MirGen, inst: hir.Inst) !void {
        const decl = inst.data.pl_node.pl;
        const data = mg.hir.extraData(decl, hir.Inst.FnDecl);
        std.debug.print("{}\n", .{data});

        try mg.genBlock(data.body);
    }

    pub fn genBlock(mg: *MirGen, hirref: HirRef) !void {
        const block = mg.hir.inst[try hir.Inst.refToIndex(hirref)].data.pl_node.pl;
        const data = mg.hir.extraData(block, hir.Inst.Block);

        // std.debug.print("{}\n", .{data});
        var extra_index: u32 = 0;
        while (extra_index < data.len) : (extra_index += 1) {
            const inst = mg.hir.inst[mg.hir.extra_data[block + extra_index]];
            const ref = switch (inst.tag) {
                .int => mg.int(inst),
                .float => mg.float(inst),
                .add, .sub, .mul, .div, .mod,
                .eq, .neq, .geq, .leq,
                .gt, .lt => mg.binary(inst),
                .alloc => mg.alloc(inst),
                .store => mg.store(inst),
                .load => mg.load(inst),
                else => unreachable,
            };

            std.debug.print("{}\n", .{ref});
            // mg.scratch.append()
            // std.debug.print("{}\n", .{stmt});
        }
    }
    // fn param(mg: *MirGen, inst: hir.Inst) !void {
    //
    // }

    fn int(mg: *MirGen, inst: hir.Inst) !MirRef {
        return mg.addInst(.{
            .tag = .constant,
            .data = .{
                .ty_pl = .{
                    .ty = Type.initTag(.comptime_int),
                    .pl = try mg.addValue(.{ .int = inst.data.int }),
                }
            }
        });
    }

    fn float(mg: *MirGen, inst: hir.Inst) !MirRef {
        return mg.addInst(.{
            .tag = .constant,
            .data = .{
                .ty_pl = .{
                    .ty = Type.initTag(.comptime_float),
                    .pl = try mg.addValue(.{ .float = inst.data.float }),
                }
            }
        });
    }

    fn call(mg: *MirGen, inst: hir.Inst) !MirRef {
        const hir_pl = inst.data.pl_node.pl;
        const hir_call = mg.hir.extraData(hir_pl, Hir.Inst.Call);

        const pl = mg.addExtra(mir.Inst.Call {
            .args_len = hir_call.args_end - hir_call.args_start,
        });
        const args = mg.hir.extra_data[hir_call.args_start..hir_call.args_end];
        try mg.extra.appendSlice(args);

        return try mg.addInst(.{
            .tag = .call,
            .data = .{
                .op_pl = .{
                    .op = mir.Inst.indexToRef(0), // TODO
                    .pl = pl,
                }
            }
        });
    }

    fn binary(mg: *MirGen, inst: hir.Inst) !MirRef {
        const hir_pl = inst.data.pl.pl_node;
        const hir_bin = mg.hir.extraData(hir_pl, Hir.Inst.Binary);
        const lref = mg.resolveInst(hir_bin.lref);
        const rref = mg.resolveInst(hir_bin.rref);
        const lty = mg.resolveTy(lref);
        const rty = mg.resolveTy(rref);

        if ((lty.tag == .comptime_int) and (rty.tag == .comptime_int)) {
            const lpl = mg.insts[mir.Inst.refToIndex(lref)].data.ty_pl.pl;
            const rpl = mg.insts[mir.Inst.refToIndex(rref)].data.ty_pl.pl;
            const lval = mg.values[lpl];
            const rval = mg.values[rpl];
            return mg.addInst(.{
                .tag = .constant,
                .data = .{
                    .ty_pl = .{
                        .ty = .comptime_int,
                        .pl = try mg.addValue(.{ .int = lval + rval }),
                    }
                }
            });
        } else if (lty.tag == .comptime_int) {
            const lpl = mg.insts[mir.Inst.refToIndex(lref)].data.ty_pl.pl;
            const lval = mg.values[lpl];
            const tv = TypedValue {
                .ty = .{ .tag = .comptime_int },
                .val = .{ .int = @bitCast(u64, lval) },
            };

            if (tv.coerce(rty)) |result| {
                const ref = mg.addInst(.{
                    .tag = .constant,
                    .data = .{
                        .ty_pl = .{
                            .ty = result.ty,
                            .pl = try mg.addValue(.{ .int = result.val }),
                        }
                    },
                });
                return mg.addInst(.{
                    .tag = switch (inst.tag) {
                        .add => .add,
                        .sub => .sub,
                        .mul => .mul,
                        .div => .div,
                        .eq => .eq,
                        .neq => .neq,
                        else => unreachable,
                    },
                    .data = .{ .bin_op = .{ .lref = lref, .rref = ref } },
                });
            }
        } else if (rty.tag == .comptime_int) {
            const rpl = mg.insts[mir.Inst.refToIndex(rref)].data.ty_pl.pl;
            const rval = mg.values[rpl];
            const tv = TypedValue {
                .ty = .{ .tag = .comptime_int },
                .val = .{ .int = @bitCast(u64, rval) },
            };

            if (tv.coerce(lty)) |result| {
                const ref = mg.addInst(.{
                    .tag = .constant,
                    .data = .{
                        .ty_pl = .{
                            .ty = result.ty,
                            .pl = try mg.addValue(.{ .int = result.val }),
                        }
                    },
                });
                return mg.addInst(.{
                    .tag = switch (inst.tag) {
                        .add => .add,
                        .sub => .sub,
                        .mul => .mul,
                        .div => .div,
                        .eq => .eq,
                        .neq => .neq,
                        else => unreachable,
                    },
                    .data = .{ .bin_op = .{ .lref = ref, .lref = rref } },
                });
            }
        } else {
            switch (lty.tag) {
                .u1, .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                    if (lty.tag != rty.tag) return Error.TypeError;
                    return mg.addInst(.{
                        .tag = switch (inst.tag) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .eq => .eq,
                            .neq => .neq,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lref, .rref = rref } },
                    });
                },
                else => return Error.TypeError,
            }
        }
    }

    fn validate_ty(mg: *MirGen, inst: hir.Inst) !MirRef {
        const data = mg.hir.extraData(inst.data.pl_node.pl, hir.Inst.ValidateTy);
        const ref = mg.resolveInst(data.ref);
        const expected = mg.getTy(data.ty);
        const found = mg.resolveTy(ref);

        if (expected == found) {
            return ref;
        } else {
            return Error.TypeError;
        }
    }

    // fn alloc(mg: *MirGen, inst: hir.Inst) !MirRef {
    //
    // }
};
