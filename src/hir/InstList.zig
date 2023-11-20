const std = @import("std");
const Allocator = std.mem.Allocator;

const InstList = @This();

gpa: Allocator,
tape: *std.ArrayListUnmanaged(LinkedNode),
head: u32,
len: u32,
cursor: Loc,

const Loc = struct {
    node: u32,
    pos: u8,
};

pub const LinkedNode = struct {
    link: u32,
    link_valid: bool,
    len: u8,
    insts: [14]u32,
};

pub fn init(gpa: Allocator, tape: *std.ArrayListUnmanaged(LinkedNode)) !InstList {
    const tape_len: u32 = @intCast(tape.items.len);
    try tape.append(gpa, .{
        .link = undefined,
        .link_valid = false,
        .len = 0,
        .insts = undefined,
    });

    return .{
        .gpa = gpa,
        .tape = tape,
        .head = tape_len,
        .len = 0,
        .cursor = .{ .node = tape_len, .pos = 0 },
    };
}

// inserts the instruction at the cursor (node and position in node)
// creating new nodes or moving around if necessary if necessary
pub fn linkInst(self: *InstList, inst: u32) !void {
    const node = &self.tape.items[self.cursor.node];
    if (node.len < 14) {
        // we have space in this node to insert the instruction,
        // so no linked list operations necessary
        if (self.cursor.pos < node.len) {
            // need to move around
            var i: u32 = node.len;
            while (i > self.cursor.pos) : (i -= 1) {
                node.insts[i] = node.insts[i - 1];
            }
        }

        node.insts[self.cursor.pos] = inst;
        node.len += 1;
        self.cursor.pos += 1;
    } else {
        // out of space in the node, so create a new node
        if (!node.link_valid) {
            // at end of list, so we can just append a new node easily
            const tape_len: u32 = @intCast(self.tape.items.len);
            try self.tape.append(self.gpa, .{
                .link = undefined,
                .link_valid = false,
                .len = 0,
                .insts = undefined,
            });

            node.link = tape_len;
            node.link_valid = true;

            const new_node = &self.tape.items[tape_len];
            new_node.len = 1;
            new_node.insts[0] = inst;
            self.cursor.node = tape_len;
            self.cursor.pos = 1;
        } else {
            // append a new node and swap it with the current one (in memory),
            // but keep the links the same/correct
            const tape_len: u32 = @intCast(self.tape.items.len);
            try self.tape.append(self.gpa, .{
                .link = node.link,
                .link_valid = true,
                .len = node.len,
                .insts = undefined,
            });

            // copy everything after the cursor
            const new_node = &self.tape.items[tape_len];
            for (node.insts[self.cursor.pos..], 0..) |cpy, i| {
                new_node.insts[i] = cpy;
            }

            node.link = tape_len;
            node.insts[self.cursor.pos] = inst;
            self.cursor.pos += 1;
            node.len = self.cursor.pos;
        }
    }

    self.len += 1;
}

pub fn cursorSeekToHead(self: *InstList) void {
    self.cursor.node = self.head;
    self.cursor.pos = 0;
}

pub fn cursorSeekForward(self: *InstList, step: u32) void {
    var i: u32 = 0;
    while (i < step) : (i += 1) {
        const node = &self.tape.items[self.cursor.node];
        if (self.cursor.pos >= node.len) {
            std.debug.assert(node.link_valid);
            self.cursor.node = node.link;
            self.cursor.pos = 0;
        } else {
            self.cursor.pos += 1;
        }
    }
}

pub fn cursorSeekToTail(self: *InstList) void {
    self.cursor.node = self.head;
    var node = &self.tape.items[self.cursor.node];
    while (node.link_valid) {
        self.cursor.node = node.link;
        node = &self.tape.items[self.cursor.node];
    }

    self.cursor.pos = node.len;
}

pub fn iterate(self: *InstList) InstIterator {
    return .{
        .tape = self.tape.items,
        .counter = 0,
        .len = self.len,
        .cursor = .{ .node = self.head, .pos = 0 },
    };
}

pub const InstIterator = struct {
    tape: []LinkedNode,
    counter: u32,
    len: u32,
    cursor: struct {
        node: u32,
        pos: u8,
    },

    pub fn next(self: *InstIterator) ?u32 {
        const node = &self.tape[self.cursor.node];
        if (self.cursor.pos >= node.len) {
            // no more items in this node, move on
            if (node.link_valid) {
                self.cursor.node = node.link;
                self.cursor.pos = 0;
            } else {
                return null;
            }
        }

        const new_node = &self.tape[self.cursor.node];
        const inst = new_node.insts[self.cursor.pos];
        self.cursor.pos += 1;
        return inst;
    }
};

fn testLink(b: *InstList, expected_links: []const u32) !void {
    var it = b.iterate();

    for (expected_links) |expected| {
        const inst = it.next().?;
        try std.testing.expectEqual(expected, inst);
    }

    const last = it.next();
    try std.testing.expectEqual(last, null);
}

test "instruction link" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var tape = std.ArrayListUnmanaged(LinkedNode){};
    var block1 = try InstList.init(allocator, &tape);

    // std.debug.print("tape: {any}\n", .{block1.tape.items});

    try block1.linkInst(1000);
    try block1.linkInst(1001);
    try block1.linkInst(1004);
    try testLink(&block1, &.{ 1000, 1001, 1004 });

    block1.cursorSeekToHead();
    block1.cursorSeekForward(2);
    try block1.linkInst(1002);
    try block1.linkInst(1003);
    try testLink(&block1, &.{ 1000, 1001, 1002, 1003, 1004 });

    block1.cursorSeekToHead();
    for (1100..1109) |inst| {
        try block1.linkInst(@intCast(inst));
    }

    block1.cursorSeekToTail();
    try block1.linkInst(1200);
    try block1.linkInst(1201);
    try block1.linkInst(1202);
    try testLink(&block1, &.{ 1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1000, 1001, 1002, 1003, 1004, 1200, 1201, 1202 });

    block1.cursorSeekToHead();
    try block1.linkInst(1300);
    try block1.linkInst(1301);
    try block1.linkInst(1302);
    try testLink(&block1, &.{ 1300, 1301, 1302, 1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1000, 1001, 1002, 1003, 1004, 1200, 1201, 1202 });

    block1.cursorSeekForward(1);
    try block1.linkInst(1400);
    try testLink(&block1, &.{ 1300, 1301, 1302, 1400, 1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1000, 1001, 1002, 1003, 1004, 1200, 1201, 1202 });

    var block2 = try InstList.init(allocator, &tape);
    for (2000..2028) |inst| {
        try block2.linkInst(@intCast(inst));
    }
    block2.cursorSeekToHead();
    try block2.linkInst(2100);

    // std.debug.print("tape: {any}\n", .{block1.tape.items});
    try testLink(&block2, &.{ 2100, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027 });
    try testLink(&block1, &.{ 1300, 1301, 1302, 1400, 1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1000, 1001, 1002, 1003, 1004, 1200, 1201, 1202 });
}
