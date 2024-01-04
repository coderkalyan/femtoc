const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Context = @import("Context.zig");
const c = Context.c;

const Allocator = std.mem.Allocator;
const Scope = @This();

tag: Tag,

pub fn cast(base: *Scope, comptime T: type) ?*T {
    return @fieldParentPtr(T, "base", base);
}

pub fn parent(base: *Scope) ?*Scope {
    return switch (base.tag) {
        .function => null,
        .loop => base.cast(Loop).?.parent,
        .block => base.cast(Block).?.parent,
    };
}

const Tag = enum {
    function,
    loop,
    block,
};

pub const Function = struct {
    const base_tag: Tag = .function;
    base: Scope = .{ .tag = base_tag },

    entry: c.LLVMBasicBlockRef,
    prev_alloca: c.LLVMValueRef,

    pub fn init(entry: c.LLVMBasicBlockRef) @This() {
        return .{
            .entry = entry,
            .prev_alloca = null,
        };
    }
};

pub const Loop = struct {
    const base_tag: Tag = .loop;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    entry: c.LLVMBasicBlockRef,
    exit: c.LLVMBasicBlockRef,

    pub fn init(s: *Scope, entry: c.LLVMBasicBlockRef, exit: c.LLVMBasicBlockRef) @This() {
        return .{
            .parent = s,
            .entry = entry,
            .exit = exit,
        };
    }
};

pub const Block = struct {
    const base_tag: Tag = .block;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    arena: Allocator,
    lifetimes: std.ArrayListUnmanaged(c.LLVMValueRef),
    invariants: std.ArrayListUnmanaged(c.LLVMValueRef),

    pub fn init(s: *Scope, arena: Allocator) Block {
        return .{
            .parent = s,
            .arena = arena,
            .lifetimes = .{},
            .invariants = .{},
        };
    }

    pub fn deinit(b: *Block) void {
        b.lifetimes.deinit(b.arena);
        b.invariants.deinit(b.arena);
    }
};

pub fn resolve(inner: *Scope, tag: Tag) *Scope {
    var s = inner;
    while (s.tag != tag) {
        s = s.parent().?;
    }

    return s;
}
