const std = @import("std");
const math = std.math;
const Allocator = std.mem.Allocator;

pub const Error = error{UnspecificType};

// always pass Type by value
pub const Type = extern union {
    basic: Basic,
    extended: *Extended,

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

    pub const Extended = struct {
        kind: Kind,

        pub inline fn cast(base: *const Extended, comptime T: type) ?*const T {
            return @fieldParentPtr(T, "base", base);
        }

        // pub fn size(p: *const Extended) !usize {
        //     switch (p.kind) {
        //         .void, .comptime_uint, .comptime_sint, .comptime_float, .uint, .sint, .float => unreachable,
        //         .pointer => return 8,
        //         .structure => {
        //             var total: usize = 0;
        //             var max: usize = 0;
        //             const structure = p.cast(Structure).?;
        //             for (structure.members) |member| {
        //                 const member_alignment = member.alignment();
        //                 max = @max(max, member_alignment);
        //                 if (((total / member_alignment) * member_alignment) != total)
        //                     total = ((total / member_alignment) + 1) * member_alignment;
        //                 total += member.size();
        //             }
        //             if (((total / max) * max) != total)
        //                 total = ((total / max) + 1) * max;
        //             return total;
        //         },
        //         .function => return error.NotImplemented,
        //     }
        // }
        //
        // pub inline fn alignment(p: *const Extended) usize {
        //     return @as(Type, @bitCast(@intFromPtr(p))).alignment();
        // }
    };

    pub const Pointer = struct {
        const base_kind: Kind = .pointer;
        base: Extended = .{ .kind = base_kind },
        pointee: Type,

        pub fn init(pointee: Type) Pointer {
            return .{ .pointee = pointee };
        }
    };

    pub const Structure = struct {
        const base_kind: Kind = .structure;
        base: Extended = .{ .kind = base_kind },
        members: []Type,

        pub fn init(members: []Type) Structure {
            return .{ .members = members };
        }
    };

    pub const Function = struct {
        const base_kind: Kind = .function;
        base: Extended = .{ .kind = base_kind },
        param_types: []Type,
        return_type: Type,

        pub fn init(gpa: Allocator, return_type: Type, param_types: []Type) !Type {
            const function = try gpa.create(Function);
            function.* = Function{ .param_types = param_types, .return_type = return_type };
            return .{ .extended = &function.base };
        }
    };

    pub inline fn isBasic(ty: Type) bool {
        const bits: u64 = @bitCast(ty);
        return bits < basic_length;
    }

    pub fn kind(ty: Type) Kind {
        if (ty.isBasic()) return ty.basic.kind;
        return ty.extended.kind;
    }

    pub fn size(ty: Type) usize {
        return switch (ty.kind()) {
            .void => 0,
            .comptime_uint, .comptime_sint, .comptime_float => unreachable,
            .uint, .sint, .float => {
                const bytes = (ty.basic.width + 7) / 8;
                const ans: usize = 1;
                return ans << std.math.ceilPowerOfTwo(u6, bytes);
            },
            .pointer => 8, // TODO: architecture dependent
            .structure, .function => unreachable, // TODO
        };
    }

    pub fn initVoid() Type {
        return .{ .basic = .{ .kind = .void, .width = 0 } };
    }

    pub fn initLiteral(comptime _kind: Type.Kind, width: u8) Type {
        // TODO: replace size guard with bigint and special floats
        switch (_kind) {
            .uint, .sint => std.debug.assert(width <= 127),
            .float => std.debug.assert(width == 32 or width == 64),
            else => unreachable,
        }

        return .{ .basic = .{ .kind = _kind, .width = @truncate(width) } };
    }

    pub fn initLiteralImplicitWidth(comptime _kind: Type.Kind) Type {
        const width = switch (_kind) {
            .comptime_uint, .comptime_sint, .comptime_float => 64,
            .void => 0,
            else => unreachable,
        };

        return initLiteral(_kind, width);
    }

    // pub fn initInt(width: u8, sign: bool) Type {
    //     std.debug.assert(width < std.math.maxInt(u7)); // TODO: large integers
    //     return .{ .basic = .{ .kind = if (sign) .sint else .uint, .width = @truncate(width) } };
    // }
    //
    // pub fn initFloat(width: u8) Type {
    //     std.debug.assert(width == 32 or width == 64); // TODO: other special floats
    //     return .{ .basic = .{ .kind = .float, .width = @truncate(width) } };
    // }
    //
    // pub fn initComptimeInt(sign: bool) Type {
    //     return .{ .basic = .{ .kind = if (sign) .comptime_sint else .comptime_uint, .width = 64 } };
    // }
    //
    // pub fn initComptimeFloat() Type {
    //     return .{ .basic = .{ .kind = .comptime_float, .width = 64 } };
    // }

    pub fn alignment(ty: Type) usize {
        return switch (ty.kind()) {
            .void => 0,
            .uint, .sint, .float => (ty.basic.width + 7) / 8,
            .comptime_uint, .comptime_sint, .comptime_float => 8,
            .pointer => 8, // TODO: architecture dependent
            .function => unreachable,
            .structure => {
                var max: usize = 0;
                const structure = ty.extended.cast(Type.Structure).?;
                for (structure.members) |member| max = @max(max, member.alignment());
                return max;
            },
        };
    }

    pub fn maxInt(ty: Type) u64 {
        std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
        std.debug.assert(ty.basic.width <= 64);
        const width = ty.basic.width;
        if (ty.basic.kind == .uint) {
            if (width == 64) return std.math.maxInt(u64);
            return @shlExact(@as(u64, @intCast(1)), @intCast(width)) - 1;
        } else {
            return @shlExact(@as(u64, @intCast(1)), @intCast(width - 1)) - 1;
        }
    }

    pub fn minInt(ty: Type) i64 {
        std.debug.assert(ty.kind() == .uint or ty.kind() == .sint);
        std.debug.assert(ty.basic.width <= 64);
        const width: u6 = @intCast(ty.basic.width - 1);
        if (ty.basic.kind == .uint) {
            return 0;
        } else {
            return (@as(i64, @intCast(-1)) << width);
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
    try std.testing.expectEqual(Type.initVoid().size(), 0);

    try std.testing.expectEqual(Type.initInt(1, false).size(), 1);
    try std.testing.expectEqual(Type.initInt(8, false).size(), 1);
    try std.testing.expectEqual(Type.initInt(8, true).size(), 1);
    try std.testing.expectEqual(Type.initInt(16, false).size(), 2);
    try std.testing.expectEqual(Type.initInt(16, true).size(), 2);
    try std.testing.expectEqual(Type.initInt(32, false).size(), 4);
    try std.testing.expectEqual(Type.initInt(32, true).size(), 4);
    try std.testing.expectEqual(Type.initInt(64, false).size(), 8);
    try std.testing.expectEqual(Type.initInt(64, true).size(), 8);

    try std.testing.expectEqual(Type.initFloat(32).size(), 4);
    try std.testing.expectEqual(Type.initFloat(64).size(), 8);

    try std.testing.expectEqual(Type.initComptimeInt(false).size(), 8);
    try std.testing.expectEqual(Type.initComptimeInt(true).size(), 8);
    try std.testing.expectEqual(Type.initComptimeFloat().size(), 8);
}

test "primitive alignment" {
    try std.testing.expectEqual(Type.initVoid().alignment(), 0);

    try std.testing.expectEqual(Type.initInt(1, false).alignment(), 1);
    try std.testing.expectEqual(Type.initInt(8, false).alignment(), 1);
    try std.testing.expectEqual(Type.initInt(8, true).alignment(), 1);
    try std.testing.expectEqual(Type.initInt(16, false).alignment(), 2);
    try std.testing.expectEqual(Type.initInt(16, true).alignment(), 2);
    try std.testing.expectEqual(Type.initInt(32, false).alignment(), 4);
    try std.testing.expectEqual(Type.initInt(32, true).alignment(), 4);
    try std.testing.expectEqual(Type.initInt(64, false).alignment(), 8);
    try std.testing.expectEqual(Type.initInt(64, true).alignment(), 8);

    try std.testing.expectEqual(Type.initFloat(32).alignment(), 4);
    try std.testing.expectEqual(Type.initFloat(64).alignment(), 8);

    try std.testing.expectEqual(Type.initComptimeInt(false).alignment(), 8);
    try std.testing.expectEqual(Type.initComptimeInt(true).alignment(), 8);
    try std.testing.expectEqual(Type.initComptimeFloat().alignment(), 8);
}

// TODO: update tests
// test "struct size and alignment" {
//     const ubyte = Type.initInt(8, false);
//     const ushort = Type.initInt(16, false);
//     const uint = Type.initInt(32, false);
//     const ulong = Type.initInt(64, false);
//
//     const s1 = &Type.Structure.init(&[_]Type{ubyte}).base;
//     try std.testing.expectEqual(s1.size(), 1);
//     try std.testing.expectEqual(s1.alignment(), 1);
//
//     const s2 = &Type.Structure.init(&[_]Type{ushort}).base;
//     try std.testing.expectEqual(s2.size(), 2);
//     try std.testing.expectEqual(s2.alignment(), 2);
//
//     const s3 = &Type.Structure.init(&[_]Type{uint}).base;
//     try std.testing.expectEqual(s3.size(), 4);
//     try std.testing.expectEqual(s3.alignment(), 4);
//
//     const s4 = &Type.Structure.init(&[_]Type{ uint, uint, uint }).base;
//     try std.testing.expectEqual(s4.size(), 12);
//     try std.testing.expectEqual(s4.alignment(), 4);
//
//     const s5 = &Type.Structure.init(&[_]Type{ ubyte, ushort, uint, ubyte }).base;
//     try std.testing.expectEqual(s5.size(), 12);
//     try std.testing.expectEqual(s5.alignment(), 4);
//
//     const s6 = &Type.Structure.init(&[_]Type{ ubyte, ushort, ulong, ubyte, uint }).base;
//     try std.testing.expectEqual(s6.size(), 24);
//     try std.testing.expectEqual(s6.alignment(), 8);
// }
