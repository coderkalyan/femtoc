const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;

pub const Error = error { UnspecificType };

// always pass Type by value
pub const Type = extern union {
    basic: Basic,
    extended: *Payload,

    pub const basic_length = 4096;

    pub const Kind = enum(u5) {
        void,

        comptime_uint,
        comptime_sint,
        comptime_float,
        uint,
        sint,
        float,

        pointer,
        function,
        structure,
    };

    pub const Basic = packed struct {
        // performance hack borrowed from zigc:
        // first page of ram is unmapped, so 0..4095 are invalid pointer values
        // thus, anything we can fit inside 12 bits can be assumed to be basic
        kind: Kind,
        width: u7,
        // and we pad the rest of the 64 - 12 = 52 bits
        padding: u52 = 0,
    };

    pub const Payload = struct {
        kind: Kind,

        pub inline fn cast(base: *const Payload, comptime T: type) ?*const T {
            return @fieldParentPtr(T, "base", base);
        }

        pub fn size(p: *const Payload) !usize {
            switch (p.kind) {
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
                },
                .function => return error.NotImplemented,
            }
        }

        pub fn alignment(p: *const Payload) !usize {
            switch (p.kind) {
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
                },
                .function => return error.NotImplemented,
            }
        }
    };

    pub const Pointer = struct {
        const base_kind: Kind = .pointer;
        base: Payload = .{ .kind = base_kind },
        pointee: Type,

        pub fn init(pointee: Type) Pointer {
            return .{ .pointee = pointee };
        }
    };

    pub const Structure = struct {
        const base_kind: Kind = .structure;
        base: Payload = .{ .kind = base_kind },
        members: []Type,

        pub fn init(members: []Type) Structure {
            return .{ .members = members };
        }
    };

    pub const Function = struct {
        const base_kind: Kind = .function;
        base: Payload = .{ .kind = base_kind },
        params: []Type,
        return_ty: Type,

        pub fn init(gpa: Allocator, return_ty: Type, params: []Type) !Type {
            const function = try gpa.create(Function);
            function.* = Function { .params = params, .return_ty = return_ty };
            return .{ .extended = &function.base };
        }
    };

    pub inline fn isBasic(ty: Type) bool {
        return @bitCast(u64, ty) < basic_length;
    }

    pub fn kind(ty: Type) Kind {
        if (ty.isBasic()) return ty.basic.kind;
        return ty.extended.kind;
    }

    pub fn initVoid() Type {
        return .{ .basic = .{ .kind = .void, .width = 0 } };
    }

    pub fn initInt(width: u8, sign: bool) Type {
        std.debug.assert(width < std.math.maxInt(u7)); // TODO: large integers
        return .{
            .basic = .{ .kind = if (sign) .sint else .uint, .width = @truncate(u7, width) }
        };
    }

    pub fn initFloat(width: u8) Type {
        std.debug.assert(width == 32 or width == 64); // TODO: other special floats
        return .{ .basic = .{ .kind = .float, .width = @truncate(u7, width) } };
    }

    pub fn initComptimeInt(sign: bool) Type {
        return .{ 
            .basic = .{ .kind = if (sign) .comptime_sint else .comptime_uint, .width = 64 }
        };
    }

    pub fn initComptimeFloat() Type {
        return .{ .basic = .{ .kind = .comptime_float, .width = 64 } };
    }

    pub fn size(ty: Type) usize {
        return switch (ty.kind()) {
            .void => 0,
            .uint, .sint, .float => (ty.basic.width + 7) / 8,
            .comptime_uint, .comptime_sint, .comptime_float => 8,
            .pointer => 8,
            .function, .structure => unreachable,
        };
    }

    pub fn maxInt(ty: Type) u64 {
        std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
        std.debug.assert(ty.basic.width <= 64);
        const width = ty.basic.width;
        if (ty.basic.kind == .uint) {
            if (width == 64) return std.math.maxInt(u64);
            return @shlExact(@intCast(u64, 1), @intCast(u6, width)) - 1;
        } else {
            return @shlExact(@intCast(u64, 1), @intCast(u6, width - 1)) - 1;
        }
    }

    pub fn minInt(ty: Type) i64 {
        std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
        std.debug.assert(ty.basic.width <= 64);
        const width = @intCast(u6, ty.basic.width - 1);
        if (ty.basic.kind == .uint) {
            return 0;
        } else {
            return (@intCast(i64, -1) << width);
        }
    }

    pub fn intSign(ty: Type) bool {
        std.debug.assert(ty.isBasic());
        return ty.basic.kind == .sint or ty.basic.kind == .comptime_sint;
    }

    pub fn isComptimeInt(ty: Type) bool {
        std.debug.assert(ty.isBasic());
        return ty.basic.kind == .comptime_uint or ty.basic.kind == .comptime_sint;
    }

    pub fn isFixedInt(ty: Type) bool {
        std.debug.assert(ty.isBasic());
        return ty.basic.kind == .uint or ty.basic.kind == .sint;
    }

    pub fn isInt(ty: Type) bool {
        return ty.isComptimeInt() or ty.isFixedInt();
    }

    // pub fn alignment(t: Type) !usize {
    //     if (t.isTag()) {
    //         return switch (t.tag) {
    //             .void => 0,
    //             .u1, .i8, .u8 => 1,
    //             .i16, .u16 => 2,
    //             .i32, .u32 => 4,
    //             .i64, .u64 => 8,
    //             .f32 => 4,
    //             .f64 => 8,
    //             else => Error.UnspecificType,
    //         };
    //     } else {
    //         return t.payload.alignment();
    //     }
    // }
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
