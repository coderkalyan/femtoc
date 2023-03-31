const std = @import("std");
const Hir = @import("Hir.zig");
const Type = @import("typing.zig").Type;
const Compilation = @import("Compilation.zig");
const Decl = Compilation.Decl;

pub const Value = extern union {
    // int: u64,
    // float: f64,
    tag: Tag,
    payload: *Payload,

    const tagged_length = 4096;

    pub const Tag = enum(u32) {
        zero,
        one,
        void_val,

        // everything below is a payload
        u32,
        u64,
        f64,
        function,
        reference,
    };

    pub const Payload = struct {
        tag: Tag,

        pub const U32 = struct {
            base: Payload = .{ .tag = .u32 },
            int: u32,
        };

        pub const U64 = struct {
            base: Payload = .{ .tag = .u64 },
            int: u64,
        };

        pub const F64 = struct {
            base: Payload = .{ .tag = .f64 },
            float: f64,
        };

        pub const Function = struct {
            base: Payload = .{ .tag = .function },
            func: *Decl.Function,
        };

        pub const Reference = struct {
            base: Payload = .{ .tag = .reference },
            ref: Decl.Index,
        };

        pub inline fn cast(base: *const Payload, comptime T: type) ?*const T {
            return @fieldParentPtr(T, "base", base);
        }
    };

    pub inline fn kind(val: Value) Tag {
        return if (@bitCast(u64, val) < tagged_length) val.tag else val.payload.tag;
    }

    pub fn toInt(val: *Value) u64 {
        switch (val.kind()) {
            .zero => return 0,
            .one => return 1,
            .u32 => {
                const payload = val.payload.cast(Value.Payload.U32).?;
                return @intCast(u64, payload.int);
            },
            .u64 => {
                const payload = val.payload.cast(Value.Payload.U64).?;
                return payload.int;
            },
            else => unreachable,
        }
    }

    pub fn toFloat(val: Value) f64 {
        const f = val.payload.cast(Value.Payload.F64).?;
        return f.float;
    }
};
