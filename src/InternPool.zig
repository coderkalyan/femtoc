const std = @import("std");
const Value = @import("value.zig").Value;
const Allocator = std.mem.Allocator;

const InternPool = @This();
pub const ValueHandle = u32;
const max_load_percentage = std.hash_map.default_max_load_percentage;
const StringIndexContext = std.hash_map.StringIndexContext;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;

const ValueContext = struct {
    pub const hash = std.hash_map.getAutoHashFn(Value, @This());
    pub const eql = struct {
        fn eql(ctx: ValueContext, a: Value, b: Value) bool {
            _ = ctx;
            return Value.eql(a, b);
        }
    }.eql;
};

gpa: Allocator,
// arena: Allocator,
// underlying value store - allows allocation to a contigious
// store and provides lookup by a u32 index
// values: std.MultiArrayList(Value),
// TODO: encountering some strange issues with unions on multi arraylist
// value_map: std.HashMapUnmanaged(Value, ValueHandle, ValueContext, max_load_percentage),

// string interning for identifiers, string literals, etc
// underlying data is stored in a contigious array of bytes using null termination
string_bytes: std.ArrayListUnmanaged(u8),
// the string handle is just the offset in string_bytes where the string starts
// given the handle, we can simply use pointer arithmetic and casts to get the string
// this hashset stores all available handles, and uses an adapter + context to
// probe the string_bytes array to find the string to return
string_table: std.HashMapUnmanaged(u32, void, StringIndexContext, max_load_percentage),

pub const StringIndex = enum(u32) { _ };

pub fn getOrPutString(pool: *InternPool, string: []const u8) !StringIndex {
    const gop = try pool.string_table.getOrPutContextAdapted(pool.gpa, string, StringIndexAdapter{
        .bytes = &pool.string_bytes,
    }, StringIndexContext{
        .bytes = &pool.string_bytes,
    });
    if (gop.found_existing) return @enumFromInt(gop.key_ptr.*);

    try pool.string_bytes.ensureUnusedCapacity(pool.gpa, string.len + 1);
    const new_off: u32 = @intCast(pool.string_bytes.items.len);

    pool.string_bytes.appendSliceAssumeCapacity(string);
    pool.string_bytes.appendAssumeCapacity(0);

    gop.key_ptr.* = new_off;
    return @enumFromInt(new_off);
}

pub fn getString(pool: *InternPool, index: StringIndex) ?[:0]const u8 {
    const off: u32 = @intFromEnum(index);
    if (off >= pool.string_bytes.items.len) return null;
    return std.mem.sliceTo(@as([*:0]const u8, @ptrCast(pool.string_bytes.items.ptr + off)), 0);
}

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

// pub const Ptr = struct {
//     values: std.MultiArrayList(Value).Slice,
//
//     pub fn getValue(self: *const Ptr, handle: ValueHandle) Value {
//         return self.values.get(handle);
//     }
// };
//
pub fn init(gpa: Allocator) InternPool {
    return .{
        .gpa = gpa,
        // .values = .{},
        // .value_map = .{},
        .string_bytes = .{},
        .string_table = .{},
    };
}

pub fn deinit(pool: *InternPool) void {
    pool.string_bytes.deinit(pool.gpa);
    pool.string_table.deinit(pool.gpa);
}
//
// // pub fn initArrayValue(self: *InternPool)
// pub fn internValue(self: *InternPool, value: Value) !ValueHandle {
//     if (self.value_map.get(value)) |handle| {
//         return handle;
//     } else {
//         const handle: ValueHandle = @intCast(self.values.len);
//         try self.values.append(self.arena, value);
//         try self.value_map.put(self.arena, value, handle);
//         return handle;
//     }
// }
//
// pub fn getValue(self: *const InternPool, handle: ValueHandle) Value {
//     return self.values.get(handle);
// }
//
// pub fn ptr(self: *InternPool) !Ptr {
//     return .{
//         .values = self.values.toOwnedSlice(),
//     };
// }

test "interner" {
    var interner = InternPool.init(std.testing.allocator);
    defer interner.deinit();

    try std.testing.expectEqual(interner.string_bytes.items.len, 0);
    try std.testing.expectEqual(interner.string_table.count(), 0);

    // add three distinct strings
    const apple = try interner.getOrPutString("apple");
    const banana = try interner.getOrPutString("banana");
    const cherry = try interner.getOrPutString("cherry");
    try std.testing.expectEqual(interner.string_bytes.items.len, 20);
    try std.testing.expectEqual(interner.string_table.count(), 3);
    try std.testing.expect(std.mem.eql(u8, "apple", interner.getString(apple).?));
    try std.testing.expect(std.mem.eql(u8, "banana", interner.getString(banana).?));
    try std.testing.expect(std.mem.eql(u8, "cherry", interner.getString(cherry).?));

    // adding the same string again should return the original id
    try std.testing.expectEqual(apple, try interner.getOrPutString("apple"));
    try std.testing.expectEqual(interner.string_bytes.items.len, 20);
    try std.testing.expectEqual(interner.string_table.count(), 3);

    // partly overlapping string is a unique string
    const apfel = try interner.getOrPutString("apfel");
    try std.testing.expectEqual(interner.string_bytes.items.len, 26);
    try std.testing.expectEqual(interner.string_table.count(), 4);
    try std.testing.expect(std.mem.eql(u8, "apfel", interner.getString(apfel).?));
    try std.testing.expect(apple != apfel);
    try std.testing.expect(!std.mem.eql(u8, "apple", interner.getString(apfel).?));

    // existing strings should not be modified
    try std.testing.expect(std.mem.eql(u8, "apple", interner.getString(apple).?));
    try std.testing.expect(std.mem.eql(u8, "banana", interner.getString(banana).?));
    try std.testing.expect(std.mem.eql(u8, "cherry", interner.getString(cherry).?));
}
