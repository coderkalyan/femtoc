const std = @import("std");
const InternPool = @import("../InternPool.zig");

const TypedValue = @This();

ty: InternPool.Index,
val: Value,

// represents a comptime-known value which is either the result of
// a literal in code (like an integer or string literal) or comptime analysis
pub const Value = union(enum) {
    none,
    integer: u64,
    float: f64,
    array: InternPool.ExtraSlice,
    body: InternPool.AirIndex,
};

pub fn fromInterned(pool: *InternPool, index: InternPool.Index) TypedValue {
    const item = pool.items.get(@intFromEnum(index));
    const data = item.data;
    switch (item.tag) {
        .none_tv => return .{ .ty = @enumFromInt(data), .val = .{ .none = {} } },
        .integer_tv => {
            const integer = pool.extraData(InternPool.IntegerTypedValue, data);
            return .{
                .ty = integer.ty,
                .val = .{ .integer = pool.values.items[@intFromEnum(integer.value)] },
            };
        },
        .float_tv => {
            const float = pool.extraData(InternPool.FloatTypedValue, data);
            return .{
                .ty = float.ty,
                .val = .{ .float = @bitCast(pool.values.items[@intFromEnum(float.value)]) },
            };
        },
        .array_tv => {
            const array = pool.extraData(InternPool.ArrayTypedValue, data);
            return .{
                .ty = array.ty,
                .val = .{ .array = .{ .start = array.elements_start, .end = array.elements_end } },
            };
        },
        .body_tv => {
            const body = pool.extraData(InternPool.BodyTypedValue, data);
            return .{
                .ty = body.ty,
                .val = .{ .body = body.body },
            };
        },
        else => unreachable,
    }
}

pub const none: TypedValue = .{ .ty = .void_type, .val = .{ .none = {} } };
pub const u1_false: TypedValue = .{ .ty = .u1_type, .val = .{ .integer = 0 } };
pub const u1_true: TypedValue = .{ .ty = .u1_type, .val = .{ .integer = 1 } };
pub const u8_zero: TypedValue = .{ .ty = .u8_type, .val = .{ .integer = 0 } };
pub const u8_one: TypedValue = .{ .ty = .u8_type, .val = .{ .integer = 1 } };
pub const u32_zero: TypedValue = .{ .ty = .u32_type, .val = .{ .integer = 0 } };
pub const u32_one: TypedValue = .{ .ty = .u32_type, .val = .{ .integer = 1 } };
pub const u64_zero: TypedValue = .{ .ty = .u64_type, .val = .{ .integer = 0 } };
pub const u64_one: TypedValue = .{ .ty = .u64_type, .val = .{ .integer = 1 } };
