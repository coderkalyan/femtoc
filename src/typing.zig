const std = @import("std");

pub const Error = error { UnspecificType };

pub const Type = extern union {
    tag: Tag,
    payload: *Payload,

    pub const tagged_length = 4096;

    pub const Tag = enum(u64) {
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
        class: Class,

        pub const Class = enum {
            pointer,
            structure,
        };

        pub inline fn cast(base: *const Payload, comptime T: type) ?*const T {
            return @fieldParentPtr(T, "base", base);
        }

        pub fn size(p: *const Payload) !usize {
            switch (p.class) {
                .pointer => return 8,
                .structure => {
                    var total: usize = 0;
                    var max: usize = 0;
                    const structure = p.cast(Structure).?;
                    for (structure.members) |member| {
                        const member_alignment = try member.alignment();
                        max = @max(max, member_alignment);
                        if (((total / member_alignment) * member_alignment) != total)
                            total = ((total / member_alignment) + 1) * member_alignment;
                        total += try member.size();
                    }
                    if (((total / max) * max) != total)
                        total = ((total / max) + 1) * max;
                    return total;
                }
            }
        }

        pub fn alignment(p: *const Payload) !usize {
            switch (p.class) {
                .pointer => {
                    // TODO: this depends on architecture
                    return 8;
                },
                .structure => {
                    var max: usize = 0;
                    const structure = p.cast(Structure).?;
                    for (structure.members) |member| {
                        max = @max(max, try member.alignment());
                    }
                    return max;
                }
            }
        }
    };

    pub const Pointer = struct {
        const base_class: Payload.Class = .pointer;
        base: Payload = .{ .class = base_class },
        pointee: Type,

        pub fn init(pointee: Type) Pointer {
            return .{ .pointee = pointee };
        }
    };

    pub const Structure = struct {
        const base_class: Payload.Class = .structure;
        base: Payload = .{ .class = base_class },
        members: []Type,

        pub fn init(members: []Type) Structure {
            return .{ .members = members };
        }
    };

    pub fn initTag(tag: Tag) Type {
        return Type {
            .tag = tag,
        };
    }

    pub inline fn isTag(ty: Type) bool {
        return @ptrToInt(ty.payload) < tagged_length;
    }

    pub fn size(t: *const Type) !usize {
        if (@enumToInt(t.tag) < tagged_length) {
            return switch (t.tag) {
                .void => 0,
                .u1 => 1,
                .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32 => 4,
                .i64, .u64 => 8,
                .f32 => 4,
                .f64 => 8,
                else => Error.UnspecificType,
            };
        } else {
            return t.payload.size();
        }
    }

    pub fn alignment(t: Type) !usize {
        if (t.isTag()) {
            return switch (t.tag) {
                .void => 0,
                .u1, .i8, .u8 => 1,
                .i16, .u16 => 2,
                .i32, .u32 => 4,
                .i64, .u64 => 8,
                .f32 => 4,
                .f64 => 8,
                else => Error.UnspecificType,
            };
        } else {
            return t.payload.alignment();
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

    try std.testing.expectError(Error.UnspecificType, Type.initTag(.comptime_int).size());
    try std.testing.expectError(Error.UnspecificType, Type.initTag(.comptime_float).size());
}

test "primitive alignment" {
    try std.testing.expectEqual(Type.initTag(.void).alignment(), 0);

    try std.testing.expectEqual(Type.initTag(.u1).alignment(), 1);
    try std.testing.expectEqual(Type.initTag(.u8).alignment(), 1);
    try std.testing.expectEqual(Type.initTag(.i8).alignment(), 1);
    try std.testing.expectEqual(Type.initTag(.u16).alignment(), 2);
    try std.testing.expectEqual(Type.initTag(.i16).alignment(), 2);
    try std.testing.expectEqual(Type.initTag(.u32).alignment(), 4);
    try std.testing.expectEqual(Type.initTag(.i32).alignment(), 4);
    try std.testing.expectEqual(Type.initTag(.u64).alignment(), 8);
    try std.testing.expectEqual(Type.initTag(.i64).alignment(), 8);

    try std.testing.expectEqual(Type.initTag(.f32).alignment(), 4);
    try std.testing.expectEqual(Type.initTag(.f64).alignment(), 8);

    try std.testing.expectError(Error.UnspecificType, Type.initTag(.comptime_int).alignment());
    try std.testing.expectError(Error.UnspecificType, Type.initTag(.comptime_float).alignment());
}

test "struct size and alignment" {
    const ubyte = Type.initTag(.u8);
    const ushort = Type.initTag(.u16);
    const uint = Type.initTag(.u32);
    const ulong = Type.initTag(.u64);

    const s1 = &Type.Structure.init(&[_]Type{ubyte}).base;
    try std.testing.expectEqual(s1.size(), 1);
    try std.testing.expectEqual(s1.alignment(), 1);

    const s2 = &Type.Structure.init(&[_]Type{ushort}).base;
    try std.testing.expectEqual(s2.size(), 2);
    try std.testing.expectEqual(s2.alignment(), 2);

    const s3 = &Type.Structure.init(&[_]Type{uint}).base;
    try std.testing.expectEqual(s3.size(), 4);
    try std.testing.expectEqual(s3.alignment(), 4);

    const s4 = &Type.Structure.init(&[_]Type{uint, uint, uint}).base;
    try std.testing.expectEqual(s4.size(), 12);
    try std.testing.expectEqual(s4.alignment(), 4);

    const s5 = &Type.Structure.init(&[_]Type{ubyte, ushort, uint, ubyte}).base;
    try std.testing.expectEqual(s5.size(), 12);
    try std.testing.expectEqual(s5.alignment(), 4);

    const s6 = &Type.Structure.init(&[_]Type{ubyte, ushort, ulong, ubyte, uint}).base;
    try std.testing.expectEqual(s6.size(), 24);
    try std.testing.expectEqual(s6.alignment(), 8);
}
