const std = @import("std");
const Air = @import("air/Air.zig");
const Type = @import("air/type.zig").Type;
const TypedValue = @import("air/TypedValue.zig");
const Decl = Air.Decl;
const Allocator = std.mem.Allocator;

const InternPool = @This();
pub const ValueHandle = u32;
const max_load_percentage = std.hash_map.default_max_load_percentage;
const StringIndexContext = std.hash_map.StringIndexContext;
const StringIndexAdapter = std.hash_map.StringIndexAdapter;

gpa: Allocator,
map: std.AutoArrayHashMapUnmanaged(void, void),
items: std.MultiArrayList(Item),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(u64),

// string interning for identifiers, string literals, etc
// underlying data is stored in a contigious array of bytes using null termination
string_bytes: std.ArrayListUnmanaged(u8),
// the string handle is just the offset in string_bytes where the string starts
// given the handle, we can simply use pointer arithmetic and casts to get the string
// this hashset stores all available handles, and uses an adapter + context to
// probe the string_bytes array to find the string to return
string_table: std.HashMapUnmanaged(u32, void, StringIndexContext, max_load_percentage),
// all toplevel declarations in the final object
// use of segmented list allows us to access either by pointer (segmented lists have
// pointer stability across resizes) or by index (storing a smaller 32 bit value in items)
decls: std.SegmentedList(Decl, 0),
// function bodies in the final object
// similar to decls, but stores a list of Air instruction bodies (associated with decls)
bodies: std.SegmentedList(Air, 0),

const Item = struct {
    tag: Tag,
    data: u32,
};

const Tag = enum(u8) {
    // types
    void_type,
    comptime_uint_type,
    comptime_sint_type,
    comptime_float_type,
    uint_type,
    sint_type,
    float_type,
    pointer_type,
    many_pointer_type,
    slice_type,
    array_type,
    comptime_array_type,
    function_type,

    // typed values
    none_tv,
    integer_tv,
    float_tv,
    array_tv,
    body_tv,

    decl,
};

pub const ValueIndex = enum(u32) { _ };
pub const DeclIndex = enum(u32) { _ };
pub const AirIndex = enum(u32) { _ };
pub const StringIndex = enum(u32) { _ };
pub const Index = enum(u32) {
    // common types, mirrors those in Type
    void_type,
    comptime_uint_type,
    comptime_sint_type,
    comptime_float_type,
    u1_type,
    u8_type,
    u16_type,
    u32_type,
    u64_type,
    i8_type,
    i16_type,
    i32_type,
    i64_type,
    f32_type,
    f64_type,
    // common typed values, mirrors those in TypedValue
    none,
    u1_false,
    u1_true,
    u8_zero,
    u8_one,
    u32_zero,
    u32_one,
    u64_zero,
    u64_one,

    _,

    // must match order above
    pub const Common = &.{
        .{ .ty = Type.void_type },
        .{ .ty = Type.comptime_uint_type },
        .{ .ty = Type.comptime_sint_type },
        .{ .ty = Type.comptime_float_type },
        .{ .ty = Type.u1_type },
        .{ .ty = Type.u8_type },
        .{ .ty = Type.u16_type },
        .{ .ty = Type.u32_type },
        .{ .ty = Type.u64_type },
        .{ .ty = Type.i8_type },
        .{ .ty = Type.i16_type },
        .{ .ty = Type.i32_type },
        .{ .ty = Type.i64_type },
        .{ .ty = Type.f32_type },
        .{ .ty = Type.f64_type },
        .{ .tv = TypedValue.none },
        .{ .tv = TypedValue.u1_false },
        .{ .tv = TypedValue.u1_true },
        .{ .tv = TypedValue.u8_zero },
        .{ .tv = TypedValue.u8_one },
        .{ .tv = TypedValue.u32_zero },
        .{ .tv = TypedValue.u32_one },
        .{ .tv = TypedValue.u64_zero },
        .{ .tv = TypedValue.u64_one },
    };
};

pub const ExtraSlice = struct {
    start: u32,
    end: u32,
};

pub const ArrayType = struct {
    element: Index,
    count: u32,
};

pub const FunctionType = struct {
    params_start: u32,
    params_end: u32,
    @"return": Index,
};

pub const IntegerTypedValue = struct {
    ty: Index,
    value: ValueIndex,
};

pub const FloatTypedValue = struct {
    ty: Index,
    value: ValueIndex,
};

pub const ArrayTypedValue = struct {
    ty: Index,
    elements_start: u32,
    elements_end: u32,
};

pub const BodyTypedValue = struct {
    ty: Index,
    body: AirIndex,
};

pub fn init(gpa: Allocator) !InternPool {
    var ip: InternPool = .{
        .gpa = gpa,
        .map = .{},
        .items = .{},
        .extra = .{},
        .values = .{},
        .decls = .{},
        .bodies = .{},
        .string_bytes = .{},
        .string_table = .{},
    };

    // initialize the pool with default "common" values
    inline for (Index.Common, std.meta.fields(Index)) |entry, field| {
        const ip_index = try ip.getOrPut(entry);
        std.debug.assert(std.mem.eql(u8, @tagName(ip_index), field.name));
    }

    return ip;
}

pub fn deinit(pool: *InternPool) void {
    pool.map.deinit(pool.gpa);
    pool.items.deinit(pool.gpa);
    pool.extra.deinit(pool.gpa);
    pool.values.deinit(pool.gpa);
    pool.decls.deinit(pool.gpa);
    pool.string_bytes.deinit(pool.gpa);
    pool.string_table.deinit(pool.gpa);
}

pub const Key = union(enum) {
    ty: Type,
    tv: TypedValue,
    decl: DeclIndex,

    const Adapter = struct {
        pool: *InternPool,

        pub fn eql(adapter: Adapter, a: Key, b_void: void, b_map_index: usize) bool {
            _ = b_void;
            const b = adapter.pool.indexToKey(@enumFromInt(b_map_index));
            return b.eql(a, adapter.pool);
        }

        pub fn hash(adapter: Adapter, key: Key) u32 {
            _ = adapter;
            return key.hash32();
        }
    };

    fn hash32(key: Key) u32 {
        return @truncate(key.hash64());
    }

    fn hash64(key: Key) u64 {
        const Hash = std.hash.Wyhash;
        const asBytes = std.mem.asBytes;
        const seed = @intFromEnum(key);
        return switch (key) {
            inline .ty,
            .tv,
            .decl,
            => |data| Hash.hash(seed, asBytes(&data)),
        };
    }

    fn eql(a: Key, b: Key, pool: *const InternPool) bool {
        _ = pool;
        const KeyTag = std.meta.Tag(Key);
        const a_tag = @as(KeyTag, a);
        const b_tag = @as(KeyTag, b);
        if (a_tag != b_tag) return false;
        switch (a_tag) {
            inline else => |tag| {
                const a_data = @field(a, @tagName(tag));
                const b_data = @field(b, @tagName(tag));
                return std.meta.eql(a_data, b_data);
            },
        }
    }
};

pub fn indexToKey(pool: *InternPool, index: Index) Key {
    const item = pool.items.get(@intFromEnum(index));
    const data = item.data;
    return switch (item.tag) {
        .void_type,
        .comptime_uint_type,
        .comptime_sint_type,
        .uint_type,
        .sint_type,
        .comptime_float_type,
        .float_type,
        .pointer_type,
        .many_pointer_type,
        .slice_type,
        .array_type,
        .comptime_array_type,
        .function_type,
        => .{ .ty = Type.fromInterned(pool, index) },
        .none_tv,
        .integer_tv,
        .float_tv,
        .array_tv,
        .body_tv,
        => .{ .tv = TypedValue.fromInterned(pool, index) },
        .decl => .{ .decl = @enumFromInt(data) },
    };
}

pub fn indexToType(pool: *InternPool, index: Index) Type {
    return pool.indexToKey(index).ty;
}

pub fn addExtra(pool: *InternPool, extra: anytype) !u32 {
    const len: u32 = @intCast(pool.extra.items.len);
    const fields = std.meta.fields(@TypeOf(extra));
    try pool.extra.ensureUnusedCapacity(pool.gpa, fields.len);
    inline for (fields) |field| {
        switch (field.type) {
            u32 => pool.extra.appendAssumeCapacity(@field(extra, field.name)),
            inline else => {
                const num: u32 = @intFromEnum(@field(extra, field.name));
                pool.extra.appendAssumeCapacity(num);
            },
        }
    }
    return len;
}

pub fn extraData(pool: *InternPool, comptime T: type, index: u32) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            u32 => @field(result, field.name) = pool.extra.items[index + i],
            inline else => @field(result, field.name) = @enumFromInt(pool.extra.items[index + i]),
        }
    }
    return result;
}

pub fn get(pool: *InternPool, key: Key) ?Index {
    const adapter: Key.Adapter = .{ .pool = pool };
    return pool.map.getAdapter(pool.gpa, key, adapter);
}

pub fn getOrPut(pool: *InternPool, key: Key) Allocator.Error!Index {
    const adapter: Key.Adapter = .{ .pool = pool };
    const gop = try pool.map.getOrPutAdapted(pool.gpa, key, adapter);
    if (gop.found_existing) return @enumFromInt(gop.index);
    try pool.items.ensureUnusedCapacity(pool.gpa, 1);
    switch (key) {
        .ty => try pool.putType(key),
        .tv => try pool.putTypedValue(key),
        .decl => |decl| pool.items.appendAssumeCapacity(.{
            .tag = .decl,
            .data = @intFromEnum(decl),
        }),
    }
    return @enumFromInt(pool.items.len - 1);
}

pub fn getOrPutType(pool: *InternPool, ty: Type) Allocator.Error!Index {
    return pool.getOrPut(.{ .ty = ty });
}

fn putType(pool: *InternPool, key: Key) !void {
    const ty = key.ty;
    try pool.items.ensureUnusedCapacity(pool.gpa, 1);
    switch (ty) {
        .void => pool.items.appendAssumeCapacity(.{ .tag = .void_type, .data = 0 }),
        .comptime_int => |int| switch (int.sign) {
            .unsigned => pool.items.appendAssumeCapacity(.{ .tag = .comptime_uint_type, .data = 64 }),
            .signed => pool.items.appendAssumeCapacity(.{ .tag = .comptime_sint_type, .data = 64 }),
        },
        .int => |int| switch (int.sign) {
            .unsigned => pool.items.appendAssumeCapacity(.{ .tag = .uint_type, .data = int.width }),
            .signed => pool.items.appendAssumeCapacity(.{ .tag = .sint_type, .data = int.width }),
        },
        .comptime_float => pool.items.appendAssumeCapacity(.{ .tag = .comptime_float_type, .data = 64 }),
        .float => |float| pool.items.appendAssumeCapacity(.{ .tag = .float_type, .data = float.width }),
        .pointer => |pointer| pool.items.appendAssumeCapacity(.{ .tag = .pointer_type, .data = @intFromEnum(pointer.pointee) }),
        .many_pointer => |pointer| pool.items.appendAssumeCapacity(.{
            .tag = .many_pointer_type,
            .data = @intFromEnum(pointer.pointee),
        }),
        .slice => |slice| pool.items.appendAssumeCapacity(.{ .tag = .slice_type, .data = @intFromEnum(slice.element) }),
        .array => |array| {
            const pl = try pool.addExtra(ArrayType{
                .element = array.element,
                .count = array.count,
            });
            pool.items.appendAssumeCapacity(.{ .tag = .array_type, .data = pl });
        },
        .comptime_array => |array| pool.items.appendAssumeCapacity(.{ .tag = .comptime_array_type, .data = @intCast(array.count) }),
        .function => |function| {
            const pl = try pool.addExtra(FunctionType{
                .params_start = function.params.start,
                .params_end = function.params.end,
                .@"return" = function.@"return",
            });
            pool.items.appendAssumeCapacity(.{ .tag = .function_type, .data = pl });
        },
    }
}

fn putTypedValue(pool: *InternPool, key: Key) !void {
    const tv = key.tv;
    try pool.items.ensureUnusedCapacity(pool.gpa, 1);
    switch (tv.val) {
        .none => pool.items.appendAssumeCapacity(.{ .tag = .none_tv, .data = @intFromEnum(tv.ty) }),
        .integer => |int| {
            try pool.values.append(pool.gpa, int);
            const value_index: u32 = @intCast(pool.values.items.len - 1);
            const pl = try pool.addExtra(IntegerTypedValue{
                .ty = tv.ty,
                .value = @enumFromInt(value_index),
            });
            pool.items.appendAssumeCapacity(.{ .tag = .integer_tv, .data = pl });
        },
        .float => |float| {
            try pool.values.append(pool.gpa, @bitCast(float));
            const value_index: u32 = @intCast(pool.values.items.len - 1);
            const pl = try pool.addExtra(FloatTypedValue{
                .ty = tv.ty,
                .value = @enumFromInt(value_index),
            });
            pool.items.appendAssumeCapacity(.{ .tag = .float_tv, .data = pl });
        },
        .array => |array| {
            const pl = try pool.addExtra(ArrayTypedValue{
                .ty = tv.ty,
                .elements_start = array.start,
                .elements_end = array.end,
            });
            pool.items.appendAssumeCapacity(.{ .tag = .array_tv, .data = pl });
        },
        .body => |body| {
            const pl = try pool.addExtra(BodyTypedValue{
                .ty = tv.ty,
                .body = body,
            });
            pool.items.appendAssumeCapacity(.{ .tag = .body_tv, .data = pl });
        },
    }
}

pub fn putDecl(pool: *InternPool, decl: Decl) !Index {
    try pool.decls.append(pool.gpa, decl);
    const index: u32 = @intCast(pool.decls.len - 1);
    return pool.getOrPut(.{ .decl = @enumFromInt(index) });
}

pub fn addOneDecl(pool: *InternPool) !DeclIndex {
    _ = try pool.decls.addOne(pool.gpa);
    const index: u32 = @intCast(pool.decls.len - 1);
    return @enumFromInt(index);
}

pub fn putBody(pool: *InternPool, ty: InternPool.Index, body: Air) !Index {
    try pool.bodies.append(pool.gpa, body);
    const index: u32 = @intCast(pool.bodies.len - 1);
    return pool.getOrPut(.{ .tv = .{
        .ty = ty,
        .val = .{ .body = @enumFromInt(index) },
    } });
}

pub fn addOneBody(pool: *InternPool, ty: InternPool.Index) !AirIndex {
    _ = ty;
    _ = try pool.bodies.addOne(pool.gpa);
    const index: u32 = @intCast(pool.bodies.len - 1);
    return @enumFromInt(index);
}

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

test "string intern" {
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

test "basic type intern" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    const u8_key = Key.initIntType(.unsigned, 8);
    try std.testing.expectEqual(u8_key, Key{
        .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 8 },
    });
    const u8_index = try pool.getOrPut(u8_key);
    const u8_key2 = pool.indexToKey(u8_index);
    try std.testing.expectEqual(u8_key, u8_key2);
    const u8_index2 = try pool.getOrPut(u8_key2);
    try std.testing.expectEqual(u8_index, u8_index2);
}

test "pointer type intern" {
    var pool = InternPool.init(std.testing.allocator);
    defer pool.deinit();

    const u8_key = Key.initIntType(.unsigned, 8);
    const u32_key = Key.initIntType(.unsigned, 32);
    const u8_index = try pool.getOrPut(u8_key);
    const u32_index = try pool.getOrPut(u32_key);
    try std.testing.expect(u8_index != u32_index);

    const u8_ptr_key: Key = .{ .pointer_type = .{ .pointee = u8_index } };
    const u32_ptr_key: Key = .{ .pointer_type = .{ .pointee = u32_index } };
    const u8_ptr_index = try pool.getOrPut(u8_ptr_key);
    const u32_ptr_index = try pool.getOrPut(u32_ptr_key);
    try std.testing.expect(u8_ptr_index != u32_ptr_index);
    try std.testing.expectEqual(u8_ptr_index, try pool.getOrPut(u8_ptr_key));
    try std.testing.expectEqual(u32_ptr_index, try pool.getOrPut(u32_ptr_key));
}
