const std = @import("std");
const Decl = @import("air/Decl.zig");
const Type = @import("air/type.zig").Type;
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

const Item = struct {
    tag: Tag,
    data: u32,
};

const Tag = enum(u8) {
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
    function_type,

    int,
    float,

    decl,
};

const ValueIndex = enum(u32) { _ };
const DeclIndex = enum(u32) { _ };

pub const Array = struct {
    element: Index,
    count: u32,
};

pub const FunctionType = struct {
    params_start: u32,
    params_end: u32,
    @"return": Index,
};

const Int = struct {
    ty: Index,
    value: ValueIndex,
};

const Float = struct {
    ty: Index,
    value: ValueIndex,
};

pub const Key = union(enum) {
    ty: Type,
    // void_type: VoidType,
    // int_type: IntType,
    // float_type: FloatType,
    // pointer_type: PointerType,
    // many_pointer_type: ManyPointerType,
    // slice_type: SliceType,
    // array_type: ArrayType,
    // function_type: Key.FunctionType,
    int: Key.Int,
    float: Key.Float,
    decl: DeclIndex,

    pub const VoidType = struct {
        magic: u32 = 0xcafeb0ba,
    };

    pub const IntType = struct {
        is_comptime: bool,
        sign: Sign,
        width: u8,

        pub const Sign = enum {
            signed,
            unsigned,
        };
    };

    pub const FloatType = struct {
        is_comptime: bool,
        width: u8,
    };

    pub const PointerType = struct {
        pointee: Index,
    };

    pub const ManyPointerType = struct {
        pointee: Index,
    };

    pub const SliceType = struct {
        element: Index,
    };

    pub const ArrayType = struct {
        element: Index,
        count: u32,
    };

    pub const FunctionType = struct {
        params: Slice,
        @"return": Index,

        const Slice = struct {
            start: u32,
            end: u32,
        };
    };

    pub const Int = struct {
        value: u64,
        ty: Index,
    };

    pub const Float = struct {
        value: f64,
        ty: Index,
    };

    pub fn initVoidType() Key {
        return .{ .void_type = .{} };
    }

    pub fn initIntType(sign: IntType.Sign, width: u8) Key {
        return .{ .int_type = .{
            .is_comptime = false,
            .sign = sign,
            .width = width,
        } };
    }

    pub fn initComptimeIntType(sign: IntType.Sign) Key {
        return .{ .int_type = .{
            .is_comptime = true,
            .sign = sign,
            .width = 64,
        } };
    }

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
            // inline .void_type,
            // .int_type,
            // .float_type,
            // .pointer_type,
            // .many_pointer_type,
            // .slice_type,
            // .array_type,
            // .function_type,
            inline .ty,
            .int,
            .decl,
            => |data| Hash.hash(seed, asBytes(&data)),
            .float => |float| {
                var hasher = Hash.init(seed);
                std.hash.autoHash(&hasher, float.ty);
                std.hash.autoHash(&hasher, @as(u64, @bitCast(float.value)));
                return hasher.final();
            },
        };
    }

    fn eql(a: Key, b: Key, pool: *const InternPool) bool {
        _ = pool;
        const KeyTag = std.meta.Tag(Key);
        const a_tag = @as(KeyTag, a);
        const b_tag = @as(KeyTag, b);
        if (a_tag != b_tag) return false;
        switch (a_tag) {
            // .void_type => return true,
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
        .function_type,
        => .{ .ty = Type.fromInterned(pool, index) },
        .int => {
            const int = pool.extraData(Int, data);
            return .{ .int = .{
                .ty = int.ty,
                .value = pool.values.items[@intFromEnum(int.value)],
            } };
        },
        .float => {
            const float = pool.extraData(Float, data);
            return .{ .float = .{
                .ty = float.ty,
                .value = @bitCast(pool.values.items[@intFromEnum(float.value)]),
            } };
        },
        .decl => .{ .decl = @enumFromInt(data) },
    };
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
        // .void_type => |void_type| pool.items.appendAssumeCapacity(.{
        //     .tag = .void_type,
        //     .data = void_type.magic,
        // }),
        // .int_type => |int_type| switch (int_type.is_comptime) {
        //     true => switch (int_type.sign) {
        //         .unsigned => pool.items.appendAssumeCapacity(.{
        //             .tag = .comptime_uint_type,
        //             .data = @intCast(int_type.width),
        //         }),
        //         .signed => pool.items.appendAssumeCapacity(.{
        //             .tag = .comptime_sint_type,
        //             .data = @intCast(int_type.width),
        //         }),
        //     },
        //     false => switch (int_type.sign) {
        //         .unsigned => pool.items.appendAssumeCapacity(.{
        //             .tag = .uint_type,
        //             .data = @intCast(int_type.width),
        //         }),
        //         .signed => pool.items.appendAssumeCapacity(.{
        //             .tag = .sint_type,
        //             .data = @intCast(int_type.width),
        //         }),
        //     },
        // },
        // .float_type => |float_type| switch (float_type.is_comptime) {
        //     true => pool.items.appendAssumeCapacity(.{
        //         .tag = .comptime_float_type,
        //         .data = @intCast(float_type.width),
        //     }),
        //     false => pool.items.appendAssumeCapacity(.{
        //         .tag = .float_type,
        //         .data = @intCast(float_type.width),
        //     }),
        // },
        // .pointer_type => |pointer_type| pool.items.appendAssumeCapacity(.{
        //     .tag = .pointer_type,
        //     .data = @intFromEnum(pointer_type.pointee),
        // }),
        // .many_pointer_type => |many_pointer_type| pool.items.appendAssumeCapacity(.{
        //     .tag = .many_pointer_type,
        //     .data = @intFromEnum(many_pointer_type.pointee),
        // }),
        // .slice_type => |slice_type| pool.items.appendAssumeCapacity(.{
        //     .tag = .slice_type,
        //     .data = @intFromEnum(slice_type.element),
        // }),
        // .array_type => |array_type| {
        //     const pl = try pool.addExtra(Array{
        //         .element = array_type.element,
        //         .count = array_type.count,
        //     });
        //     pool.items.appendAssumeCapacity(.{
        //         .tag = .array_type,
        //         .data = pl,
        //     });
        // },
        // .function_type => |function_type| {
        //     const pl = try pool.addExtra(FunctionType{
        //         .params_start = function_type.params.start,
        //         .params_end = function_type.params.end,
        //         .@"return" = function_type.@"return",
        //     });
        //     pool.items.appendAssumeCapacity(.{
        //         .tag = .function_type,
        //         .data = pl,
        //     });
        // },
        .int => |int| {
            try pool.values.append(pool.gpa, int.value);
            const value_index: u32 = @intCast(pool.values.items.len - 1);
            const pl = try pool.addExtra(Int{
                .ty = int.ty,
                .value = @enumFromInt(value_index),
            });
            pool.items.appendAssumeCapacity(.{
                .tag = .int,
                .data = pl,
            });
        },
        .float => |float| {
            try pool.values.append(pool.gpa, @bitCast(float.value));
            const value_index: u32 = @intCast(pool.values.items.len - 1);
            const pl = try pool.addExtra(Float{
                .ty = float.ty,
                .value = @enumFromInt(value_index),
            });
            pool.items.appendAssumeCapacity(.{
                .tag = .int,
                .data = pl,
            });
        },
        .decl => |decl| pool.items.appendAssumeCapacity(.{
            .tag = .decl,
            .data = @intFromEnum(decl),
        }),
    }
    return @enumFromInt(pool.items.len - 1);
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
            const pl = try pool.addExtra(Array{
                .element = array.element,
                .count = array.count,
            });
            pool.items.appendAssumeCapacity(.{ .tag = .array_type, .data = pl });
        },
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

pub const Index = enum(u32) {
    _,
};

pub const ExtraSlice = struct {
    start: u32,
    end: u32,
};

pub fn init(gpa: Allocator) InternPool {
    return .{
        .gpa = gpa,
        .map = .{},
        .items = .{},
        .extra = .{},
        .values = .{},
        .decls = .{},
        .string_bytes = .{},
        .string_table = .{},
    };
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
