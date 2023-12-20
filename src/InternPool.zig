const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;

const InternPool = @This();
pub const ValueHandle = u32;
const max_load_percentage = std.hash_map.default_max_load_percentage;
const StringIndexContext = std.hash_map.StringIndexContext;

const ValueContext = struct {
    pub const hash = std.hash_map.getAutoHashFn(Value, @This());
    pub const eql = struct {
        fn eql(ctx: ValueContext, a: Value, b: Value) bool {
            _ = ctx;
            return Value.eql(a, b);
        }
    }.eql;
};

// gpa: Allocator,
arena: Allocator,
// underlying value store - allows allocation to a contigious
// store and provides lookup by a u32 index
values: std.MultiArrayList(Value),
// TODO: encountering some strange issues with unions on multi arraylist
value_map: std.HashMapUnmanaged(Value, ValueHandle, ValueContext, max_load_percentage),
// string_bytes: std.ArrayListUnmanaged(u8),
// string_table: std.HashMapUnmanaged(u32, void, StringIndexContext, max_load_percentage),

const Index = enum(u32) {
    _,
};

const Item = struct {
    tag: Tag,
    data: u32,
};

const Tag = enum(u8) {
    type_void,
    type_comptime_uint,
    type_comptime_sint,
    type_comptime_float,
    type_uint,
    type_sint,
    type_float,
    type_pointer,
    type_many_pointer,
    type_array,
    type_slice,
    ident,
};

pub const Ptr = struct {
    values: std.MultiArrayList(Value).Slice,

    pub fn getValue(self: *const Ptr, handle: ValueHandle) Value {
        return self.values.get(handle);
    }
};

pub fn init(arena: Allocator) InternPool {
    return .{
        .arena = arena,
        .values = .{},
        .value_map = .{},
    };
}

// pub fn initArrayValue(self: *InternPool)
pub fn internValue(self: *InternPool, value: Value) !ValueHandle {
    if (self.value_map.get(value)) |handle| {
        return handle;
    } else {
        const handle: ValueHandle = @intCast(self.values.len);
        try self.values.append(self.arena, value);
        try self.value_map.put(self.arena, value, handle);
        return handle;
    }
}

pub fn getValue(self: *const InternPool, handle: ValueHandle) Value {
    return self.values.get(handle);
}

pub fn ptr(self: *InternPool) !Ptr {
    return .{
        .values = self.values.toOwnedSlice(),
    };
}
