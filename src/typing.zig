const std = @import("std");

pub const Type = extern union {
    tag: Tag,
    payload: *Payload,

    pub const tagged_length = 4096;

    pub const Tag = enum(u32) {
        void,

        comptime_int,
        u1,
        i8,
        u8,
        i16,
        u16,
        i32,
        u32,
        i64,
        u64,

        comptime_float,
        f32,
        f64,
    };

    pub const Payload = struct {
        tag: PlTag,

        pub const PlTag = enum {
            array,
        };

        pub fn size(p: *Payload) usize {
            _ = p;
            return 0;
        }
    };

    pub fn initTag(tag: Tag) Type {
        return Type {
            .tag = tag,
        };
    }

    pub fn size(t: *const Type) usize {
        if (@enumToInt(t.tag) < tagged_length) {
            return switch (t.tag) {
                .void => 0,

                .comptime_uint => 8,
                .comptime_sint => 8,
                .u1 => 1,
                .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32 => 4,
                .i64, .u64 => 8,

                .comptime_float => 8,
                .f32 => 4,
                .f64 => 8,
            };
        } else {
            return t.payload.size();
        }
    }
};

test "primitives size" {
    try std.testing.expectEqual(Type.initTag(.void).size(), 0);

    try std.testing.expectEqual(Type.initTag(.u1).size(), 1);
    try std.testing.expectEqual(Type.initTag(.u8).size(), 1);
    try std.testing.expectEqual(Type.initTag(.i8).size(), 1);
    try std.testing.expectEqual(Type.initTag(.u16).size(), 2);
    try std.testing.expectEqual(Type.initTag(.i16).size(), 2);
    try std.testing.expectEqual(Type.initTag(.u32).size(), 4);
    try std.testing.expectEqual(Type.initTag(.i32).size(), 4);
    try std.testing.expectEqual(Type.initTag(.u64).size(), 8);
    try std.testing.expectEqual(Type.initTag(.i64).size(), 8);

    try std.testing.expectEqual(Type.initTag(.f32).size(), 4);
    try std.testing.expectEqual(Type.initTag(.f64).size(), 8);
}
