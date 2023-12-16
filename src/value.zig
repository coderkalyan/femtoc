const std = @import("std");
const Hir = @import("Hir.zig");
const HirGen = @import("HirGen.zig");
const Allocator = std.mem.Allocator;

pub const Value = extern union {
    basic: Basic,
    extended: *Extended,

    const basic_length = 4096;

    pub const Kind = enum(u32) {
        zero,
        one,
        none,
        u32,

        // everything below is a payload
        u64,
        f64,
        function,
        reference,
    };

    pub const Basic = packed struct {
        // performance hack borrowed from zigc:
        // first page of ram is unmapped, so 0..4095 are invalid pointer values
        // thus, anything we can fit inside 12 bits can be assumed to be basic
        // make sure kind isn't too large
        kind: Kind,
        // and we pad the other 32 bits
        padding: u32 = 0,
    };

    pub const Extended = struct {
        kind: Kind,

        pub const Reference = struct {
            base: Extended = .{ .kind = .reference },
            // ref: Decl.Index,
        };

        pub inline fn cast(base: *const Extended, comptime T: type) ?*const T {
            return @fieldParentPtr(T, "base", base);
        }
    };

    pub const U32 = struct {
        base: Extended = .{ .kind = .u32 },
        int: u32,
    };

    pub const U64 = struct {
        base: Extended = .{ .kind = .u64 },
        int: u64,
    };

    pub const F64 = struct {
        base: Extended = .{ .kind = .f64 },
        float: f64,
    };

    pub const Function = struct {
        base: Extended = .{ .kind = .function },
        params: []Hir.Index,
        body: Hir.Index,
    };

    pub inline fn isBasic(val: Value) bool {
        const bits: u64 = @bitCast(val);
        return bits < basic_length;
    }

    pub fn kind(val: Value) Kind {
        if (val.isBasic()) return val.basic.kind;
        return val.extended.kind;
    }

    pub fn toInt(val: Value) u64 {
        switch (val.kind()) {
            .zero => return 0,
            .one => return 1,
            .u32 => {
                const payload = val.extended.cast(Value.U32).?;
                return @intCast(payload.int);
            },
            .u64 => {
                const payload = val.extended.cast(Value.U64).?;
                return payload.int;
            },
            else => unreachable,
        }
    }

    pub fn toFloat(val: Value) f64 {
        const f = val.extended.cast(Value.F64).?;
        return f.float;
    }

    pub const Common = .{
        .zero = Value{ .basic = .{ .kind = .zero } },
        .one = Value{ .basic = .{ .kind = .one } },
    };
};
