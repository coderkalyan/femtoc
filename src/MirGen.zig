const std = @import("std");
const hir = @import("hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;

const math = std.math;

const Allocator = std.mem.Allocator;
const MirGen = @This();
const Hir = hir.Hir;
const HirRef = hir.Inst.Ref;
const Value = Mir.Value;
const TypedValue = Mir.TypedValue;
const Error = error { TypeError };

pub const MirMap = struct {
    start: hir.Inst.Index,
    map: []?Mir.Ref,

    pub fn init() MirMap {
        return MirMap {
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
