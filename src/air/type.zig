const std = @import("std");
const InternPool = @import("../InternPool.zig");
const math = std.math;
const Allocator = std.mem.Allocator;

pub const Type = union(enum) {
    void,
    comptime_int: struct { sign: Sign },
    int: struct { sign: Sign, width: u8 },
    comptime_float,
    float: struct { width: u8 },
    pointer: struct { pointee: InternPool.Index },
    many_pointer: struct { pointee: InternPool.Index },
    slice: struct { element: InternPool.Index },
    array: struct { element: InternPool.Index, count: u32 },
    function: struct {
        params: InternPool.ExtraSlice,
        @"return": InternPool.Index,
    },

    pub const Sign = enum {
        signed,
        unsigned,
    };

    pub fn fromInterned(pool: *InternPool, index: InternPool.Index) Type {
        const item = pool.items.get(@intFromEnum(index));
        const data = item.data;
        return switch (item.tag) {
            .void_type => .{ .void = {} },
            .comptime_uint_type => .{ .comptime_int = .{ .sign = .unsigned } },
            .comptime_sint_type => .{ .comptime_int = .{ .sign = .signed } },
            .uint_type => .{ .int = .{ .sign = .unsigned, .width = @intCast(data) } },
            .sint_type => .{ .int = .{ .sign = .signed, .width = @intCast(data) } },
            .comptime_float_type => .{ .comptime_float = {} },
            .float_type => .{ .float = .{ .width = @intCast(data) } },
            .pointer_type => .{ .pointer = .{ .pointee = @enumFromInt(data) } },
            .many_pointer_type => .{ .many_pointer = .{ .pointee = @enumFromInt(data) } },
            .slice_type => .{ .slice = .{ .element = @enumFromInt(data) } },
            .array_type => {
                const array = pool.extraData(InternPool.Array, data);
                return .{ .array = .{ .element = array.element, .count = array.count } };
            },
            .function_type => {
                const function = pool.extraData(InternPool.FunctionType, data);
                return .{ .function = .{
                    .params = .{ .start = function.params_start, .end = function.params_end },
                    .@"return" = function.@"return",
                } };
            },
            else => unreachable,
        };
    }

    pub fn maxInt(ty: Type) u64 {
        std.debug.assert(@as(std.meta.Tag(Type), ty) == .int);
        const width = ty.int.width;
        std.debug.assert(width <= 64);
        if (ty.int.sign == .unsigned) {
            if (width == 64) return std.math.maxInt(u64);
            return @shlExact(@as(u64, @intCast(1)), @intCast(width)) - 1;
        } else {
            return @shlExact(@as(u64, @intCast(1)), @intCast(width - 1)) - 1;
        }
    }

    pub fn minInt(ty: Type) i64 {
        std.debug.assert(@as(std.meta.Tag(Type), ty) == .int);
        std.debug.assert(ty.int.width <= 64);
        const width: u6 = @intCast(ty.int.width - 1);
        if (ty.int.sign == .unsigned) {
            return 0;
        } else {
            return (@as(i64, @intCast(-1)) << width);
        }
    }
};

// pub fn kind(ty: Type, pool: *InternPool) Kind {
//     const i = @intFromEnum(ty.ip_index);
//     switch (pool.items.items(.tags)[i]) {
//         .comptime_uint_type =>
//     }
// }
// pub const Pointer = struct {
//     pub fn init(pool: *InternPool, pointee: Type) !Type {
//         // try pool.
//         const pointer = try allocator.create(Pointer);
//         pointer.* = .{ .pointee = pointee };
//         std.debug.print("creating pointer: {x}\n", .{@intFromPtr(&pointer.base)});
//         return .{ .extended = &pointer.base };
//     }
// };
//
// pub const ManyPointer = struct {
//     const base_kind: Kind = .many_pointer;
//     base: Extended = .{ .kind = base_kind },
//     pointee: Type,
//
//     pub fn init(allocator: Allocator, pointee: Type) !Type {
//         const pointer = try allocator.create(ManyPointer);
//         pointer.* = .{ .pointee = pointee };
//         return .{ .extended = &pointer.base };
//     }
// };
//
// pub const Array = struct {
//     const base_kind: Kind = .array;
//     base: Extended = .{ .kind = base_kind },
//     element: Type,
//     count: u32,
//
//     pub fn init(allocator: Allocator, element: Type, count: u32) !Type {
//         const array = try allocator.create(Array);
//         array.* = .{ .element = element, .count = count };
//         return .{ .extended = &array.base };
//     }
// };
//
// pub const Slice = struct {
//     const base_kind: Kind = .slice;
//     base: Extended = .{ .kind = base_kind },
//     element: Type,
//
//     pub fn init(allocator: Allocator, element: Type) !Type {
//             const slice = try allocator.create(Slice);
//             slice.* = .{ .element = element };
//             return .{ .extended = &slice.base };
//         }
//     };
//
//     pub const ComptimeArray = struct {
//         const base_kind: Kind = .comptime_array;
//         base: Extended = .{ .kind = base_kind },
//         count: u32,
//
//         pub fn init(element: Type) Array {
//             return .{ .element = element };
//         }
//     };
//
//     pub const Structure = struct {
//         const base_kind: Kind = .structure;
//         base: Extended = .{ .kind = base_kind },
//         members: []Type,
//
//         pub fn init(members: []Type) Structure {
//             return .{ .members = members };
//         }
//     };
//
//     pub const Function = struct {
//         const base_kind: Kind = .function;
//         base: Extended = .{ .kind = base_kind },
//         param_types: []Type,
//         return_type: Type,
//     };
//
//     pub inline fn isBasic(ty: Type) bool {
//         const bits: u64 = @bitCast(ty.basic);
//         return bits < basic_length;
//     }
//
//     pub fn kind(ty: Type) Kind {
//         if (ty.isBasic()) return ty.basic.kind;
//         return ty.extended.kind;
//     }
//
//     fn intSize(width: u32) usize {
//         var s: usize = 1;
//         while (s * 8 < width) {
//             s *= 2;
//         }
//
//         return s;
//     }
//
//     pub fn eql(self: Type, other: Type) bool {
//         const self_bits: u64 = @bitCast(self.basic);
//         const other_bits: u64 = @bitCast(other.basic);
//         if (self_bits == other_bits) return true;
//
//         const self_kind = self.kind();
//         const other_kind = other.kind();
//         switch (self_kind) {
//             .uint,
//             .sint,
//             .float,
//             .comptime_uint,
//             .comptime_sint,
//             .comptime_float,
//             .comptime_array, // TODO
//             => return false,
//             .pointer => {
//                 if (other_kind != .pointer) return false;
//                 const self_ptr = self.extended.cast(Type.Pointer).?;
//                 const other_ptr = other.extended.cast(Type.Pointer).?;
//                 return self_ptr.pointee.eql(other_ptr.pointee);
//             },
//             .many_pointer => {
//                 if (other_kind != .many_pointer) return false;
//                 const self_ptr = self.extended.cast(Type.ManyPointer).?;
//                 const other_ptr = other.extended.cast(Type.ManyPointer).?;
//                 std.debug.print("many ptr: {} {}\n", .{ self_ptr.pointee.basic, other_ptr.pointee.basic });
//                 return self_ptr.pointee.eql(other_ptr.pointee);
//             },
//             .slice => {
//                 if (other_kind != .slice) return false;
//                 const self_slice = self.extended.cast(Type.Slice).?;
//                 const other_slice = other.extended.cast(Type.Slice).?;
//                 return self_slice.element.eql(other_slice.element);
//             },
//             .array => {
//                 if (other_kind != .array) return false;
//                 const self_array = self.extended.cast(Type.Array).?;
//                 const other_array = other.extended.cast(Type.Array).?;
//                 std.debug.print("eql array: {} {}\n", .{ self_array.element.basic, other_array.element.basic });
//                 if (!self_array.element.eql(other_array.element)) return false;
//                 if (self_array.count != other_array.count) return false;
//                 return true;
//             },
//             else => unreachable,
//         }
//     }
//
//     pub fn size(ty: Type) usize {
//         return switch (ty.kind()) {
//             .void => 0,
//             .comptime_uint, .comptime_sint, .comptime_float => unreachable,
//             .uint, .sint, .float => switch (ty.basic.width) {
//                 inline 1...127 => |width| comptime intSize(width),
//                 else => unreachable,
//             },
//             .pointer => 8, // TODO: architecture dependent
//             .array => {
//                 const array_type = ty.extended.cast(Type.Array).?;
//                 return array_type.element.size() * array_type.count;
//             },
//             .structure,
//             .function,
//             => unreachable, // TODO
//             else => unreachable,
//         };
//     }
//
//     pub fn initLiteral(comptime _kind: Type.Kind, width: u8) Type {
//         // TODO: replace size guard with bigint and special floats
//         switch (_kind) {
//             .uint, .sint => std.debug.assert(width <= 127),
//             .float => std.debug.assert(width == 32 or width == 64),
//             .comptime_uint, .comptime_sint, .comptime_float => std.debug.assert(width == 64),
//             .void => std.debug.assert(width == 0),
//             else => unreachable,
//         }
//
//         return .{ .basic = .{ .kind = _kind, .width = @truncate(width) } };
//     }
//
//     pub fn alignment(ty: Type) usize {
//         return switch (ty.kind()) {
//             .void => 0,
//             .uint, .sint, .float => (ty.basic.width + 7) / 8,
//             .comptime_uint, .comptime_sint, .comptime_float => 8,
//             .pointer => 8, // TODO: architecture dependent
//             .function, .array => unreachable,
//             .structure => {
//                 var max: usize = 0;
//                 const structure = ty.extended.cast(Type.Structure).?;
//                 for (structure.members) |member| max = @max(max, member.alignment());
//                 return max;
//             },
//         };
//     }
//
//     pub fn maxInt(ty: Type) u64 {
//         std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
//         std.debug.assert(ty.basic.width <= 64);
//         const width = ty.basic.width;
//         if (ty.basic.kind == .uint) {
//             if (width == 64) return std.math.maxInt(u64);
//             return @shlExact(@as(u64, @intCast(1)), @intCast(width)) - 1;
//         } else {
//             return @shlExact(@as(u64, @intCast(1)), @intCast(width - 1)) - 1;
//         }
//     }
//
//     pub fn minInt(ty: Type) i64 {
//         std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
//         std.debug.assert(ty.basic.width <= 64);
//         const width: u6 = @intCast(ty.basic.width - 1);
//         if (ty.basic.kind == .uint) {
//             return 0;
//         } else {
//             return (@as(i64, @intCast(-1)) << width);
//         }
//     }
//
//     pub fn intSign(ty: Type) bool {
//         std.debug.assert(ty.isBasic());
//         return ty.basic.kind == .sint or ty.basic.kind == .comptime_sint;
//     }
//
//     pub const Common = .{
//         .void_type = Type.initLiteral(.void, 0),
//         .comptime_uint = Type.initLiteral(.comptime_uint, 64),
//         .comptime_sint = Type.initLiteral(.comptime_sint, 64),
//         .comptime_float = Type.initLiteral(.comptime_float, 64),
//         .u1_type = Type.initLiteral(.uint, 1),
//         .u8_type = Type.initLiteral(.uint, 8),
//         .u16_type = Type.initLiteral(.uint, 16),
//         .u32_type = Type.initLiteral(.uint, 32),
//         .u64_type = Type.initLiteral(.uint, 64),
//         .i8_type = Type.initLiteral(.sint, 8),
//         .i16_type = Type.initLiteral(.sint, 16),
//         .i32_type = Type.initLiteral(.sint, 32),
//         .i64_type = Type.initLiteral(.sint, 64),
//         .f32_type = Type.initLiteral(.float, 32),
//         .f64_type = Type.initLiteral(.float, 64),
//     };
// };
//
// test "integers" {
//     const u4_type = Type.initLiteral(.uint, 4);
//     try std.testing.expectEqual(u4_type.basic.kind, .uint);
//     try std.testing.expectEqual(u4_type.basic.width, 4);
//     try std.testing.expect(u4_type.isBasic());
//     try std.testing.expectEqual(u4_type.kind(), .uint);
//     try std.testing.expectEqual(u4_type.minInt(), 0);
//     try std.testing.expectEqual(u4_type.maxInt(), 15);
//     try std.testing.expectEqual(u4_type.size(), 1);
//     try std.testing.expectEqual(u4_type.alignment(), 1);
//
//     const u8_type = Type.initLiteral(.uint, 8);
//     try std.testing.expectEqual(u8_type.basic.kind, .uint);
//     try std.testing.expectEqual(u8_type.basic.width, 8);
//     try std.testing.expect(u8_type.isBasic());
//     try std.testing.expectEqual(u8_type.kind(), .uint);
//     try std.testing.expectEqual(u8_type.minInt(), 0);
//     try std.testing.expectEqual(u8_type.maxInt(), 255);
//     try std.testing.expectEqual(u8_type.size(), 1);
//     try std.testing.expectEqual(u8_type.alignment(), 1);
//     try std.testing.expect(u8_type.eql(Type.Common.u8_type));
//
//     const i14_type = Type.initLiteral(.sint, 14);
//     try std.testing.expectEqual(i14_type.basic.kind, .sint);
//     try std.testing.expectEqual(i14_type.basic.width, 14);
//     try std.testing.expect(i14_type.isBasic());
//     try std.testing.expectEqual(i14_type.kind(), .sint);
//     try std.testing.expectEqual(i14_type.minInt(), -8192);
//     try std.testing.expectEqual(i14_type.maxInt(), 8191);
//     try std.testing.expectEqual(i14_type.size(), 2);
//     try std.testing.expectEqual(i14_type.alignment(), 2);
//
//     const u32_type = Type.initLiteral(.uint, 32);
//     try std.testing.expectEqual(u32_type.basic.kind, .uint);
//     try std.testing.expectEqual(u32_type.basic.width, 32);
//     try std.testing.expect(u32_type.isBasic());
//     try std.testing.expectEqual(u32_type.kind(), .uint);
//     try std.testing.expectEqual(u32_type.minInt(), 0);
//     try std.testing.expectEqual(u32_type.maxInt(), 4294967295);
//     try std.testing.expectEqual(u32_type.size(), 4);
//     try std.testing.expectEqual(u32_type.alignment(), 4);
//     try std.testing.expect(u32_type.eql(Type.Common.u32_type));
//
//     const i64_type = Type.initLiteral(.sint, 64);
//     try std.testing.expectEqual(i64_type.basic.kind, .sint);
//     try std.testing.expectEqual(i64_type.basic.width, 64);
//     try std.testing.expect(i64_type.isBasic());
//     try std.testing.expectEqual(i64_type.kind(), .sint);
//     const i64_min: i64 = @bitCast(@as(u64, 0x8000_0000_0000_0000));
//     const i64_max: i64 = 0x7fff_ffff_ffff_ffff;
//     try std.testing.expectEqual(i64_type.minInt(), i64_min);
//     try std.testing.expectEqual(i64_type.maxInt(), i64_max);
//     try std.testing.expectEqual(i64_type.size(), 8);
//     try std.testing.expectEqual(i64_type.alignment(), 8);
//     try std.testing.expect(i64_type.eql(Type.Common.i64_type));
//
//     const uctime_type = Type.Common.comptime_uint;
//     try std.testing.expectEqual(uctime_type.basic.kind, .comptime_uint);
//     try std.testing.expectEqual(uctime_type.basic.width, 64);
//     try std.testing.expect(uctime_type.isBasic());
//     try std.testing.expectEqual(uctime_type.kind(), .comptime_uint);
//
//     const sctime_type = Type.Common.comptime_sint;
//     try std.testing.expectEqual(sctime_type.basic.kind, .comptime_sint);
//     try std.testing.expectEqual(sctime_type.basic.width, 64);
//     try std.testing.expect(sctime_type.isBasic());
//     try std.testing.expectEqual(sctime_type.kind(), .comptime_sint);
// }
//
// test "floats" {
//     const f32_type = Type.initLiteral(.float, 32);
//     try std.testing.expectEqual(f32_type.basic.kind, .float);
//     try std.testing.expectEqual(f32_type.basic.width, 32);
//     try std.testing.expect(f32_type.isBasic());
//     try std.testing.expectEqual(f32_type.kind(), .float);
//     try std.testing.expectEqual(f32_type.size(), 4);
//     try std.testing.expectEqual(f32_type.alignment(), 4);
//
//     const f64_type = Type.initLiteral(.float, 64);
//     try std.testing.expectEqual(f64_type.basic.kind, .float);
//     try std.testing.expectEqual(f64_type.basic.width, 64);
//     try std.testing.expect(f64_type.isBasic());
//     try std.testing.expectEqual(f64_type.kind(), .float);
//     try std.testing.expectEqual(f64_type.size(), 8);
//     try std.testing.expectEqual(f64_type.alignment(), 8);
//
//     const fctime_type = Type.Common.comptime_float;
//     try std.testing.expectEqual(fctime_type.basic.kind, .comptime_float);
//     try std.testing.expectEqual(fctime_type.basic.width, 64);
//     try std.testing.expect(fctime_type.isBasic());
//     try std.testing.expectEqual(fctime_type.kind(), .comptime_float);
// }
//
// test "void" {
//     const void_type = Type.Common.void_type;
//     try std.testing.expectEqual(void_type.basic.kind, .void);
//     try std.testing.expectEqual(void_type.basic.width, 0);
//     try std.testing.expect(void_type.isBasic());
//     try std.testing.expectEqual(void_type.kind(), .void);
//     try std.testing.expectEqual(void_type.size(), 0);
// }
//
// // TODO: enable this when enabling structs
// // test "struct size and alignment" {
// //     const ubyte = Type.initInt(8, false);
// //     const ushort = Type.initInt(16, false);
// //     const uint = Type.initInt(32, false);
// //     const ulong = Type.initInt(64, false);
// //
// //     const s1 = &Type.Structure.init(&[_]Type{ubyte}).base;
// //     try std.testing.expectEqual(s1.size(), 1);
// //     try std.testing.expectEqual(s1.alignment(), 1);
// //
// //     const s2 = &Type.Structure.init(&[_]Type{ushort}).base;
// //     try std.testing.expectEqual(s2.size(), 2);
// //     try std.testing.expectEqual(s2.alignment(), 2);
// //
// //     const s3 = &Type.Structure.init(&[_]Type{uint}).base;
// //     try std.testing.expectEqual(s3.size(), 4);
// //     try std.testing.expectEqual(s3.alignment(), 4);
// //
// //     const s4 = &Type.Structure.init(&[_]Type{ uint, uint, uint }).base;
// //     try std.testing.expectEqual(s4.size(), 12);
// //     try std.testing.expectEqual(s4.alignment(), 4);
// //
// //     const s5 = &Type.Structure.init(&[_]Type{ ubyte, ushort, uint, ubyte }).base;
// //     try std.testing.expectEqual(s5.size(), 12);
// //     try std.testing.expectEqual(s5.alignment(), 4);
// //
// //     const s6 = &Type.Structure.init(&[_]Type{ ubyte, ushort, ulong, ubyte, uint }).base;
// //     try std.testing.expectEqual(s6.size(), 24);
// //     try std.testing.expectEqual(s6.alignment(), 8);
// // }
