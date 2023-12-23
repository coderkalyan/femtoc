const std = @import("std");
const Ast = @import("../Ast.zig");
const Fir = @import("Fir.zig");
const FirGen = @import("FirGen.zig");
const InternPool = @import("../InternPool.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const Inst = Fir.Inst;
const Scope = @This();

tag: Tag,

pub fn cast(base: *Scope, comptime T: type) ?*T {
    return @fieldParentPtr(T, "base", base);
}

pub fn parent(base: *Scope) ?*Scope {
    return switch (base.tag) {
        .module => null,
        .namespace => base.cast(Namespace).?.parent,
        .block => base.cast(Block).?.parent,
        .local_val => base.cast(LocalVal).?.parent,
        .local_ptr => base.cast(LocalPtr).?.parent,
        .local_type => base.cast(LocalType).?.parent,
    };
}

const Tag = enum {
    module,
    namespace,
    block,
    local_val,
    local_ptr,
    local_type,
};

pub const Module = struct {
    const base_tag: Tag = .module;
    base: Scope = .{ .tag = base_tag },
};

pub const Namespace = struct {
    const base_tag: Tag = .namespace;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    decls: std.AutoHashMapUnmanaged(InternPool.StringIndex, void),
    types: std.AutoHashMapUnmanaged(InternPool.StringIndex, void),

    pub fn init(s: *Scope) @This() {
        return .{
            .parent = s,
            .decls = .{},
            .types = .{},
        };
    }
};

pub const Block = struct {
    const base_tag: Tag = .block;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    fg: *FirGen,
    tree: *const Ast,
    // list of instruction indices (in order) that represent a block body
    insts: std.ArrayListUnmanaged(Fir.Index),

    pub fn init(fg: *FirGen, s: *Scope) Block {
        return .{
            .parent = s,
            .fg = fg,
            .tree = fg.tree,
            .insts = .{},
        };
    }

    pub fn deinit(b: *Block) void {
        b.insts.deinit(b.fg.arena);
    }

    pub fn addUnlinked(b: *Block, inst: Inst) !Fir.Index {
        return b.fg.add(inst);
    }

    pub fn add(b: *Block, inst: Inst) !Fir.Index {
        try b.insts.ensureUnusedCapacity(b.fg.arena, 1);
        const index = try b.addUnlinked(inst);
        b.insts.appendAssumeCapacity(index);
        return index;
    }

    pub fn addBlockUnlinked(b: *Block, inner: *Block, node: Node.Index) !Fir.Index {
        const fg = b.fg;
        const scratch_top = fg.scratch.items.len;
        defer fg.scratch.shrinkRetainingCapacity(scratch_top);
        try fg.scratch.ensureUnusedCapacity(fg.arena, inner.insts.items.len);
        for (inner.insts.items) |inst| {
            fg.scratch.appendAssumeCapacity(@intFromEnum(inst));
        }
        const insts = fg.scratch.items[scratch_top..];
        const pl = try fg.addSlice(insts);
        return b.addUnlinked(.{
            .data = .{ .block = .{ .insts = pl } },
            .loc = .{ .node = node },
        });
    }

    pub fn addBlock(b: *Block, inner: *Block, node: Node.Index) !Fir.Index {
        const index = try b.addBlockUnlinked(inner, node);
        try b.linkInst(index);
        return index;
    }

    pub fn addBlockInlineUnlinked(fg: *FirGen, inner: *Block, node: Node.Index) !Fir.Index {
        const scratch_top = fg.scratch.items.len;
        defer fg.scratch.shrinkRetainingCapacity(scratch_top);
        try fg.scratch.ensureUnusedCapacity(fg.arena, inner.insts.items.len);
        for (inner.insts.items) |inst| {
            fg.scratch.appendAssumeCapacity(@intFromEnum(inst));
        }
        const insts = fg.scratch.items[scratch_top..];
        const pl = try fg.addSlice(insts);
        return fg.add(.{
            .data = .{ .block_inline = .{ .insts = pl } },
            .loc = .{ .node = node },
        });
    }

    pub fn linkInst(b: *Block, inst: Fir.Index) !void {
        return b.insts.append(b.fg.arena, inst);
    }
};

pub const LocalVal = struct {
    const base_tag: Tag = .local_val;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    ident: InternPool.StringIndex,
    inst: Fir.Index,

    pub fn init(s: *Scope, ident: InternPool.StringIndex, inst: Fir.Index) LocalVal {
        return .{
            .parent = s,
            .ident = ident,
            .inst = inst,
        };
    }
};

pub const LocalPtr = struct {
    const base_tag: Tag = .local_ptr;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    ident: InternPool.StringIndex,
    ptr: Fir.Index,

    pub fn init(s: *Scope, ident: InternPool.StringIndex, ptr: Fir.Index) LocalPtr {
        return .{
            .parent = s,
            .ident = ident,
            .ptr = ptr,
        };
    }
};

pub const LocalType = struct {
    const base_tag: Tag = .local_type;
    base: Scope = .{ .tag = base_tag },

    parent: *Scope,
    ident: InternPool.StringIndex,
    inst: Fir.Index,

    pub fn init(s: *Scope, ident: InternPool.StringIndex, inst: Fir.Index) @This() {
        return .{
            .parent = s,
            .ident = ident,
            .inst = inst,
        };
    }
};

pub fn resolveIdent(inner: *Scope, ident: InternPool.StringIndex) !?*Scope {
    var found: ?*Scope = null;
    var shadows_scope: bool = false;
    var s: *Scope = inner;

    while (true) {
        switch (s.tag) {
            .module => break,
            .namespace => {
                const namespace = s.cast(Namespace).?;
                if (namespace.decls.get(ident)) |_| {
                    if (found) |_| {
                        return error.IdentifierShadowed;
                    } else {
                        found = s;
                    }
                }

                s = namespace.parent;
            },
            .block => {
                const block = s.cast(Block).?;
                shadows_scope = true;

                s = block.parent;
            },
            .local_val => {
                const local_val = s.cast(LocalVal).?;
                if (local_val.ident == ident) {
                    if (found != null) {
                        if (shadows_scope) return error.IdentifierShadowed;
                    } else {
                        found = s;
                    }
                }

                s = local_val.parent;
            },
            .local_ptr => {
                const local_ptr = s.cast(LocalPtr).?;
                if (local_ptr.ident == ident) {
                    if (found != null) {
                        if (shadows_scope) return error.IdentifierShadowed;
                    } else {
                        found = s;
                    }
                }

                s = local_ptr.parent;
            },
            .local_type => {
                const local_type = s.cast(LocalPtr).?;
                if (local_type.ident == ident) {
                    if (found != null) {
                        if (shadows_scope) return error.IdentifierShadowed;
                    } else {
                        found = s;
                    }
                }

                s = local_type.parent;
            },
        }
    }

    return found;
}

// TODO: update these tests
// test "namespace member" {
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena.deinit();
//     var allocator = arena.allocator();
//
//     var interner = Interner.init(allocator);
//     var module = Scope.Module{};
//     var namespace = Scope.Namespace.init(&module.base);
//
//     const apple = try interner.intern("apple");
//     const banana = try interner.intern("banana");
//     const cherry = try interner.intern("cherry");
//
//     try std.testing.expectEqual(try namespace.base.resolveVar(apple), null);
//     try std.testing.expectEqual(try namespace.base.resolveVar(banana), null);
//     try std.testing.expectEqual(try namespace.base.resolveVar(cherry), null);
//
//     try namespace.decls.putNoClobber(allocator, apple, 0);
//     try namespace.decls.putNoClobber(allocator, banana, 1);
//     try namespace.decls.putNoClobber(allocator, cherry, 2);
//
//     try std.testing.expectEqual((try namespace.base.resolveVar(apple)).?, &namespace.base);
//     try std.testing.expectEqual((try namespace.base.resolveVar(banana)).?, &namespace.base);
//     try std.testing.expectEqual((try namespace.base.resolveVar(cherry)).?, &namespace.base);
// }
//
// test "local var" {
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena.deinit();
//     var allocator = arena.allocator();
//
//     var interner = Interner.init(allocator);
//     var module = Scope.Module{};
//     var namespace = Scope.Namespace.init(&module.base);
//
//     const apple = try interner.intern("apple");
//     const banana = try interner.intern("banana");
//     const cherry = try interner.intern("cherry");
//
//     const b = indexToRef(1);
//     const c = indexToRef(2);
//
//     try namespace.decls.putNoClobber(allocator, apple, 0);
//     var banana_var = Scope.LocalVal.init(&namespace.base, banana, b);
//     var cherry_var = Scope.LocalVal.init(&banana_var.base, cherry, c);
//
//     try std.testing.expectEqual((try namespace.base.resolveVar(apple)).?, &namespace.base);
//     try std.testing.expectEqual((try namespace.base.resolveVar(banana)), null);
//     try std.testing.expectEqual((try namespace.base.resolveVar(cherry)), null);
//
//     try std.testing.expectEqual((try banana_var.base.resolveVar(apple)).?, &namespace.base);
//     try std.testing.expectEqual((try banana_var.base.resolveVar(banana)).?, &banana_var.base);
//     try std.testing.expectEqual((try banana_var.base.resolveVar(cherry)), null);
//
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &namespace.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &banana_var.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);
// }
//
// test "namespace shadowing" {
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena.deinit();
//     var allocator = arena.allocator();
//
//     var interner = Interner.init(allocator);
//     var module = Scope.Module{};
//     var namespace = Scope.Namespace.init(&module.base);
//
//     const apple = try interner.intern("apple");
//     const banana = try interner.intern("banana");
//     const cherry = try interner.intern("cherry");
//
//     const c = indexToRef(2);
//     const d = indexToRef(3);
//
//     try namespace.decls.putNoClobber(allocator, apple, 0);
//     try namespace.decls.putNoClobber(allocator, banana, 1);
//     var cherry_var = Scope.LocalVal.init(&namespace.base, cherry, c);
//     var illegal_var = Scope.LocalVal.init(&cherry_var.base, apple, d);
//
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &namespace.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &namespace.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);
//
//     try std.testing.expectError(error.IdentifierShadowed, illegal_var.base.resolveVar(apple));
// }
//
// test "block shadowing" {
//     var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
//     defer arena.deinit();
//     var allocator = arena.allocator();
//
//     var interner = Interner.init(allocator);
//     var module = Scope.Module{};
//     var outer = Scope.Block{
//         .parent = &module.base,
//         .instructions = .{},
//         .hg = undefined,
//         .scratch = .{},
//         .force_comptime = true,
//         .return_ty = @enumFromInt(0),
//     };
//
//     const apple = try interner.intern("apple");
//     const banana = try interner.intern("banana");
//     const cherry = try interner.intern("cherry");
//
//     const a = indexToRef(0);
//     const b = indexToRef(1);
//     const c = indexToRef(2);
//     const d = indexToRef(3);
//     const e = indexToRef(4);
//
//     var apple_var = Scope.LocalVal.init(&outer.base, apple, a);
//     var banana_var = Scope.LocalVal.init(&apple_var.base, banana, b);
//     var inner = Scope.Block{
//         .parent = &banana_var.base,
//         .hg = undefined,
//         .instructions = .{},
//         .scratch = .{},
//         .force_comptime = false,
//         .return_ty = @enumFromInt(0),
//     };
//     var cherry_var = Scope.LocalVal.init(&inner.base, cherry, c);
//
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &apple_var.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &banana_var.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);
//
//     var cherry_new_var = Scope.LocalVal.init(&cherry_var.base, cherry, d);
//     try std.testing.expectEqual((try cherry_new_var.base.resolveVar(cherry)).?, &cherry_new_var.base);
//     try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);
//
//     var apple_new_var = Scope.LocalVal.init(&cherry_new_var.base, apple, e);
//     try std.testing.expectError(error.IdentifierShadowed, apple_new_var.base.resolveVar(apple));
// }

// test "block linked list" {
//     var module = Scope.Module{};
//     var outer = Scope.Block{
//         .parent = &module.base,
//         .instructions = .{},
//         .head = 0,
//         .cursor = 0,
//         .hg = undefined,
//         .scratch = .{},
//         .force_comptime = true,
//         .return_ty = @enumFromInt(0),
//     };
//     outer.addInst();
// }
