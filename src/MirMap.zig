const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");

const Allocator = std.mem.Allocator;
const MirMap = @This();

// Maps HIR instruction *indices* to MIR instruction references
// we only want to map "real" HIR instructions since HIR refs map
// separately/directly to MIR refs, but for consistency, we output
// MIR refs that can directly be referenced
// the map uses a dynamically allocated slice that fits a range
// or ranges of instruction indices as tightly as possible. When a new
// range is inserted, the table finds the tightest "start offset" such that
// index 0 in the table maps to the lowest instruction index, and provides
// the required size
parent: ?*MirMap,
start: Hir.Index,
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

// ensures that the map has space to insert the specified indices, without
// deleting any existing *valid* entries. entries that are null
// (not populated or deleted) are removed
// providing an empty instructions slice will shrink the map to fit the
// the existing valid entries as tightly as possible
pub fn ensureSliceCapacity(m: *MirMap, a: Allocator, insts: []const Hir.Index) !void {
    if (insts.len == 0) {
        return;
    }

    var better_start = m.start;
    var found_start: bool = false;
    var max: u32 = 0;
    for (m.map) |entry, i| {
        if (entry) |_| {
            if (!found_start) {
                better_start = @max(better_start, m.start + @intCast(u32, i));
                found_start = true;
            }
        }
        max = @max(max, m.start + @intCast(u32, i));
    }
    if (m.map.len == 0) {
        better_start = std.mem.min(Hir.Index, insts);
        max = std.mem.max(Hir.Index, insts);
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

pub fn putAssumeCapacity(m: *MirMap, inst: Hir.Index, ref: Mir.Ref) void {
    m.map[inst - m.start] = ref;
}

pub fn putAssumeCapacityNoClobber(m: *MirMap, inst: Hir.Index, ref: Mir.Ref) void {
    std.debug.assert(inst - m.start < m.map.len);
    std.debug.assert(m.map[inst - m.start] == null);
    m.map[inst - m.start] = ref;
}

pub fn get(m: *MirMap, inst: Hir.Index) ?Mir.Ref {
    const found = ref: {
        if (inst < m.start) break :ref null;
        if (inst - m.start < m.map.len) {
            break :ref m.map[inst - m.start];
        } else {
            break :ref null;
        }
    };
    if (found) |ref| return ref;
    if (m.parent) |parent| return parent.get(inst);
    return null;
}

pub fn remove(m: *MirMap, inst: Hir.Index) void {
    std.debug.assert(inst - m.start < m.map.len);
    m.map[inst - m.start] = null;
}

pub fn resolveRef(m: *MirMap, ref: Hir.Ref) Mir.Ref {
    return switch (ref) {
        .zero_val => .zero_val,
        .one_val => .one_val,
        .void_val => .void_val,
        .btrue_val => .one_val,
        .bfalse_val => .zero_val,
        .u8_ty => .u8_ty,
        .u16_ty => .u16_ty,
        .u32_ty => .u32_ty,
        .u64_ty => .u64_ty,
        .i8_ty => .i8_ty,
        .i16_ty => .i16_ty,
        .i32_ty => .i32_ty,
        .i64_ty => .i64_ty,
        .f32_ty => .f32_ty,
        .f64_ty => .f64_ty,
        .bool_ty => .u1_ty,
        .void_ty => .void_ty,
        else => {
            if (m.get(Hir.Inst.refToIndex(ref).?)) |mapping| {
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

test "mirmap" {
    var m = MirMap.init(null);
    defer m.deinit(std.testing.allocator);

    try m.ensureSliceCapacity(std.testing.allocator, &[_]Hir.Index{3, 4, 5, 8, 9, 10});
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

    try m.ensureSliceCapacity(std.testing.allocator, &[_]Hir.Index{11, 12, 13});
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

    try m.ensureSliceCapacity(std.testing.allocator, &[_]Hir.Index{14, 15});
    try std.testing.expectEqual(m.get(14), null);
    try std.testing.expectEqual(m.get(15), null);
    try m.ensureSliceCapacity(std.testing.allocator, &[_]Hir.Index{});
    try std.testing.expectEqual(m.get(14), null);
    try std.testing.expectEqual(m.get(15), null);
}
