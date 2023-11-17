const std = @import("std");

pub const Error = error{ InternTableFull, InvalidId };

// "Interns" strings by storing each unique string exactly once
// and returning a unique handle to the string the next time it
// is interned. Can also return the string given a handle
pub const Interner = struct {
    gpa: std.mem.Allocator,
    // maps string buffers to unique u32 handles
    // entries can be accessed not only by key (string)
    // but also index (ArrayHashMap) which allows us to
    // store a handle->index mapping to get back the string,
    // rather than a more expensive bimap
    map: std.StringArrayHashMap(u32),
    // holds handle->index mappings. handles are incremented
    // rather than randomized, so all elements in this arraylist
    // map exactly one handle to a real entry index
    list: std.ArrayList(u32),

    pub fn init(gpa: std.mem.Allocator) Interner {
        return Interner{
            .gpa = gpa,
            .map = std.StringArrayHashMap(u32).init(gpa),
            .list = std.ArrayList(u32).init(gpa),
        };
    }

    pub fn intern(self: *Interner, string: []const u8) !u32 {
        // if we've seen this string before, return its handle
        if (self.map.get(string)) |id| {
            return id;
        }

        if (self.list.items.len >= std.math.maxInt(u32)) return Error.InternTableFull;
        // handle is the "next" element in the list
        const id: u32 = @intCast(self.list.items.len);
        try self.map.put(string, id);
        const index = self.map.getIndex(string).?;
        try self.list.append(@intCast(index));

        return id;
    }

    pub fn get(self: *const Interner, id: u32) ![]const u8 {
        if (id >= self.list.items.len) return Error.InvalidId;
        const index = self.list.items[id];
        return self.map.keys()[index];
    }
};

test "interner" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();
    var interner = Interner.init(arena.allocator());

    try std.testing.expectEqual(interner.list.items.len, 0);
    try std.testing.expectEqual(interner.map.count(), 0);

    // add three distinct strings
    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");
    try std.testing.expectEqual(interner.list.items.len, 3);
    try std.testing.expectEqual(interner.map.count(), 3);
    try std.testing.expect(std.mem.eql(u8, "apple", try interner.get(apple)));
    try std.testing.expect(std.mem.eql(u8, "banana", try interner.get(banana)));
    try std.testing.expect(std.mem.eql(u8, "cherry", try interner.get(cherry)));

    // adding the same string again should return the original id
    try std.testing.expectEqual(apple, try interner.intern("apple"));
    try std.testing.expectEqual(interner.list.items.len, 3);
    try std.testing.expectEqual(interner.map.count(), 3);

    // partly overlapping string is a unique string
    const apfel = try interner.intern("apfel");
    try std.testing.expectEqual(interner.list.items.len, 4);
    try std.testing.expectEqual(interner.map.count(), 4);
    try std.testing.expect(std.mem.eql(u8, "apfel", try interner.get(apfel)));
    try std.testing.expect(apple != apfel);
    try std.testing.expect(!std.mem.eql(u8, "apple", try interner.get(apfel)));

    // existing strings should not be modified
    try std.testing.expect(std.mem.eql(u8, "apple", try interner.get(apple)));
    try std.testing.expect(std.mem.eql(u8, "banana", try interner.get(banana)));
    try std.testing.expect(std.mem.eql(u8, "cherry", try interner.get(cherry)));

    try std.testing.expectError(Error.InvalidId, interner.get(4));
}
