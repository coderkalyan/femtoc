const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const TypedValue = @import("typing.zig").TypedValue;
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");

const Allocator = std.mem.Allocator;
const Analyzer = @This();
const Value = Mir.Value;

mg: *MirGen,
map: MirMap,
hir: *const Hir,
instructions: std.MultiArrayList(Mir.Inst),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(Value),
scratch: std.ArrayListUnmanaged(u32),
gpa: Allocator,
arena: Allocator,

const Block = struct {
    parent: ?*Block,
    analyzer: *Analyzer,
    instructions: std.ArrayListUnmanaged(Mir.Index),

    pub fn addInst(b: *Block, inst: Mir.Inst) !Mir.Index {
        const analyzer = b.analyzer;
        const index = @intCast(Mir.Index, analyzer.instructions.len);
        try analyzer.instructions.append(analyzer.gpa, inst);
        try b.instructions.append(analyzer.arena, index);
        return index;
    }

    pub fn addExtra(b: *Block, extra: anytype) !u32 {
        const analyzer = b.analyzer;
        if (@TypeOf(extra) == u32) {
            analyzer.extra.append(analyzer.gpa, extra);
            return analyzer.extra.items.len - 1;
        } else {
            const fields = std.meta.fields(@TypeOf(extra));
            try analyzer.extra.ensureUnusedCapacity(analyzer.gpa, fields.len);
            const len = @intCast(u32, analyzer.extra.items.len);
            inline for (fields) |field| {
                comptime std.debug.assert(field.field_type == u32);
                analyzer.extra.appendAssumeCapacity(@field(extra, field.name));
            }
            return len;
        }
    }

    pub fn addValue(b: *Block, val: Value) !u32 {
        const analyzer = b.analyzer;
        const len = @intCast(u32, analyzer.values.items.len);
        try analyzer.values.append(analyzer.gpa, val);
        return len;
    }

    pub fn addConstant(b: *Block, ty: Type, val: Value) !u32 {
        const value = try b.addValue(val);
        const index = try b.addInst(.{
            .tag = .constant,
            .data = .{
                .ty_pl = .{
                    .ty = ty,
                    .pl = value,
                }
            }
        });
        return index;
    }
};

pub fn analyzeBody(analyzer: *Analyzer, inst: Hir.Index) !Mir {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const block_data = analyzer.hir.extraData(pl, Hir.Inst.Block);

    const block = &Block {
        .parent = null,
        .analyzer = analyzer,
        .instructions = .{},
    };

    const hir_insts = analyzer.hir.extra_data[pl + 1..pl + 1 + block_data.len];
    try analyzer.map.ensureSliceCapacity(analyzer.arena, hir_insts);
    for (hir_insts) |index| {
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .alloc => try analyzer.alloc(block, index),
            .load => try analyzer.load(block, index),
            .load_inline => try analyzer.loadInline(block, index),
            .store => try analyzer.store(block, index),
            .validate_ty => try analyzer.validateTy(block, index),
            else => Mir.indexToRef(0),
        };
        analyzer.map.putAssumeCapacity(index, ref);
    }

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
    };
}

pub fn resolveTy(analyzer: *Analyzer, b: *Block, ref: Mir.Ref) !Type {
    if (Mir.refToIndex(ref)) |index| {
        const data = analyzer.instructions.items(.data)[index];
        return switch (analyzer.instructions.items(.tag)[index]) {
            .constant => data.ty_pl.ty,
            .add, .sub, .mul, .div, .mod => analyzer.resolveTy(b, data.bin_op.lref),
            .eq, .neq, .geq,
            .leq, .gt, .lt => analyzer.resolveTy(b, data.bin_op.lref),
            .alloc => data.ty,
            else => error.NotImplemented,
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => .{ .tag = .comptime_int },
            .void_val => .{ .tag = .void },
            else => error.NotImplemented,
        };
    }
}

pub fn integer(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    _ = analyzer;
    const data = b.analyzer.hir.insts.items(.data)[inst];
    const index = try b.addConstant(Type.initTag(.comptime_int), .{ .int = data.int });
    return Mir.indexToRef(index);
}

fn float(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = b.analyzer.hir.insts.items(.data)[inst];
    _ = analyzer;
    const index = try b.addConstant(Type.initTag(.comptime_float), .{ .float = data.float });
    return Mir.indexToRef(index);
}

fn loadInline(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    _ = b;
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.hir.resolution_map.get(data.pl_node.pl).?;
    return analyzer.map.resolveRef(ref);
}

fn load(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addInst(.{
        .tag = .load,
        .data = .{ .un_op = analyzer.map.resolveRef(data.un_node.operand) },
    });
    return Mir.indexToRef(index);
}

fn store(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const store_data = analyzer.mg.hir.extraData(data.pl_node.pl, Hir.Inst.Store);
    const addr = analyzer.map.resolveRef(Hir.Inst.indexToRef(store_data.addr));
    const val = analyzer.map.resolveRef(store_data.val);
    const index = try b.addInst(.{
        .tag = .store,
        .data = .{ .bin_op = .{ .lref = addr, .rref = val } },
    });
    return Mir.indexToRef(index);
}

fn alloc(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_node.operand);
    const ty = try analyzer.resolveTy(b, ref);
    const index = try b.addInst(.{
        .tag = .alloc,
        .data = .{ .ty = ty },
    });
    return Mir.indexToRef(index);
}

fn validateTy(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const validate_ty = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.ValidateTy);
    const ref = analyzer.map.resolveRef(validate_ty.ref);
    const found_ty = try analyzer.resolveTy(b, ref);
    const expected_ty = validate_ty.ty.toType();

    if (found_ty.tag == expected_ty.tag) {
        return ref;
    } else {
        // std.debug.print("ty: {} {}\n", .{expected_ty.tag, found_ty.tag});
        // std.debug.print("val: {}\n", .{});
        switch (found_ty.tag) {
            .comptime_int => {
                const int_value: u64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].int;
                } else switch (ref) {
                    .zero_val => return Mir.Ref.zero_val,
                    .one_val => return Mir.Ref.one_val,
                    else => return error.InvalidRef,
                };
                const index = try b.addConstant(expected_ty, .{ .int = int_value });
                return Mir.indexToRef(index);
            },
            .comptime_float => {
                const float_value: f64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].float;
                } else {
                    return error.InvalidRef;
                };
                const index = try b.addConstant(expected_ty, .{ .float = float_value });
                return Mir.indexToRef(index);
            },
            else => return error.TypeError,
        }
    }
}
