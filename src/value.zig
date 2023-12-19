const std = @import("std");
const Hir = @import("Hir.zig");
const HirGen = @import("HirGen.zig");
const InternPool = @import("InternPool.zig");
const Allocator = std.mem.Allocator;

const ValueHandle = InternPool.ValueHandle;

// represents a comptime-known value which is either the result of
// a literal in code (like an integer or string literal) or comptime analysis
pub const Value = union(enum) {
    none: u64,
    integer: u64,
    float: u64, // f64 not hashable, so we do a bitcast for now
    string: u32,
    function: *Function,
    array: *Array,
    slice: *Slice,

    pub const Function = struct {
        params: []Hir.Index,
        body: Hir.Index,
    };

    pub const Array = struct {
        elements: []ValueHandle,
    };

    pub const Slice = struct {
        ptr: ValueHandle,
        len: ValueHandle,
    };

    pub fn eql(a: Value, b: Value) bool {
        if (@intFromEnum(a) != @intFromEnum(b)) return false;

        switch (a) {
            .none => return false,
            .integer => return a.integer == b.integer,
            .float => return a.float == b.float,
            .string => return a.string == b.string,
            .function => {
                if (a.function != b.function) return false;
                if (a.function.body != b.function.body) return false;
                if (!std.mem.eql(Hir.Index, a.function.params, b.function.params)) return false;
                return true;
            },
            .array => return std.mem.eql(ValueHandle, a.array.elements, b.array.elements),
            .slice => {
                if (a.slice.ptr != b.slice.ptr) return false;
                if (a.slice.len != b.slice.len) return false;
                return true;
            },
        }
    }

    pub fn createFunction(allocator: Allocator, params: []const Hir.Index, body: Hir.Index) !Value {
        const params_owned = try allocator.alloc(Hir.Index, params.len);
        std.mem.copy(Hir.Index, params_owned, params);
        const function = try allocator.create(Function);
        function.* = .{ .params = params_owned, .body = body };
        return .{ .function = function };
    }

    pub fn createArray(allocator: Allocator, elements: []const ValueHandle) !Value {
        const elements_owned = try allocator.alloc(ValueHandle, elements.len);
        std.mem.copy(ValueHandle, elements_owned, elements);
        const array = try allocator.create(Array);
        array.* = .{ .elements = elements_owned };
        return .{ .array = array };
    }

    pub fn createSlice(allocator: Allocator, ptr: ValueHandle, len: ValueHandle) !Value {
        const slice = try allocator.create(Slice);
        slice.* = .{ .ptr = ptr, .len = len };
        return .{ .slice = slice };
    }

    pub const Common = .{
        .zero = Value{ .integer = 0 },
        .one = Value{ .integer = 1 },
    };
};

// pub const Value = extern union {
//     basic: Basic,
//     extended: *Extended,
//
//     const basic_length = 4096;
//
//     pub const Kind = enum(u32) {
//         zero,
//         one,
//         none,
//         u32,
//
//         // everything below is a payload
//         u64,
//         f64,
//         function,
//         array,
//         string,
//     };
//
//     pub const Basic = packed struct {
//         // performance hack borrowed from zigc:
//         // first page of ram is unmapped, so 0..4095 are invalid pointer values
//         // thus, anything we can fit inside 12 bits can be assumed to be basic
//         // make sure kind isn't too large
//         kind: Kind,
//         // and we pad the other 32 bits
//         padding: u32 = 0,
//     };
//
//
//     pub const U32 = struct {
//         base: Extended = .{ .kind = .u32 },
//         int: u32,
//     };
//
//     pub const U64 = struct {
//         base: Extended = .{ .kind = .u64 },
//         int: u64,
//     };
//
//     pub const F64 = struct {
//         base: Extended = .{ .kind = .f64 },
//         float: f64,
//     };
//
//
//
//     pub const String = struct {
//         base: Extended = .{ .kind = .string },
//         literal: u32,
//     };
//
//     pub const Slice = struct {
//         base: Extended = .{ .kind = .slice },
//         ptr: Hir.Index,
//         len: u64,
//     };
//
//     pub inline fn isBasic(val: Value) bool {
//         const bits: u64 = @bitCast(val);
//         return bits < basic_length;
//     }
//
//     pub fn kind(val: Value) Kind {
//         if (val.isBasic()) return val.basic.kind;
//         return val.extended.kind;
//     }
//
//     pub fn toInt(val: Value) u64 {
//         switch (val.kind()) {
//             .zero => return 0,
//             .one => return 1,
//             .u32 => {
//                 const payload = val.extended.cast(Value.U32).?;
//                 return @intCast(payload.int);
//             },
//             .u64 => {
//                 const payload = val.extended.cast(Value.U64).?;
//                 return payload.int;
//             },
//             else => unreachable,
//         }
//     }
//
//     pub fn toFloat(val: Value) f64 {
//         const f = val.extended.cast(Value.F64).?;
//         return f.float;
//     }
//
// };
