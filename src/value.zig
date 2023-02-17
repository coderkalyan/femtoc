const std = @import("std");
const typing = @import("typing.zig");

const Type = typing.Type;

pub const Value = union {
    int: u64,
    float: f64,
    payload: *Payload,

    pub const Payload = struct {
        tag: Tag,

        pub const Tag = enum {

        };
    };

    pub const ints = [_]type{
        .{.i1, i1},
        .{.i8, i8},
        .{.u8, u8},
        .{.i16, i16},
        .{.u16, u16},
        .{.i32, i32},
        .{.u32, u32},
        .{.i64, i64},
        .{.u64, u64},
    };
};

pub const TypedValue = struct {
    ty: Type,
    val: Value,

    pub fn coerce(tv: *TypedValue, ty: Type) ?TypedValue {
        switch (tv.ty.tag) {
            .comptime_int => {
                inline for (Type.ints) |int| {
                    const val = tv.val.int;
                    if (ty.tag == int[0]) {
                        if ((val >= std.math.minInt(int[1])) and (val <= std.math.maxInt(int[1]))) {
                            return @intCast(int[1], val);
                        } else {
                            return null;
                        }
                    }
                }
            }
        }
    }
};

fn tvInt(val: anytype) TypedValue {
    return TypedValue {
        .ty = .{ .tag = .comptime_int },
        .val = .{ .int = @bitCast(u64, val) },
    };
}

// fn testIntCoerce(val: anytype) !void {
//     try std.testing.expectEqual(tvInt(val).coerce())
// }

// test "comptime_int coersion" {
//     try std.testing.expectEqual(tvInt(123).coerce(123));
// }
