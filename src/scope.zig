const std = @import("std");
const Hir = @import("Hir.zig");
const HirGen = @import("HirGen.zig");
const Ast = @import("Ast.zig");
const Interner = @import("interner.zig").Interner;

const Node = Ast.Node;
const Inst = Hir.Inst;
const Allocator = std.mem.Allocator;
const indexToRef = Inst.indexToRef;
// pub const IdentifierError = error { Invalid, Shadowed };

pub const Scope = struct {
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
        decls: std.AutoHashMapUnmanaged(u32, Node.Index),
        types: std.AutoHashMapUnmanaged(u32, Node.Index),

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
        instructions: std.ArrayListUnmanaged(u32),
        scratch: std.ArrayListUnmanaged(u32),
        hg: *HirGen,

        force_comptime: bool,

        pub fn init(b: *Block, s: *Scope) Block {
            return .{
                .parent = s,
                .instructions = .{},
                .scratch = .{},
                .hg = b.hg,
                .force_comptime = b.force_comptime,
            };
        }

        pub fn deinit(b: *Block) void {
            b.instructions.deinit(b.hg.arena);
        }

        pub fn addInst(b: *Block, inst: Inst) !Hir.Index {
            const gpa = b.hg.gpa;
            const arena = b.hg.arena;
            const index = @intCast(Hir.Index, b.hg.instructions.len);
            try b.hg.instructions.ensureUnusedCapacity(gpa, 1);
            try b.instructions.ensureUnusedCapacity(arena, 1);

            b.hg.instructions.appendAssumeCapacity(inst);
            b.instructions.appendAssumeCapacity(index);
            return index;
        }
    };

    pub const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        ident: u32,
        ref: Hir.Ref,

        pub fn init(s: *Scope, ident: u32, ref: Hir.Ref) LocalVal {
            return .{
                .parent = s,
                .ident = ident,
                .ref = ref,
            };
        }
    };

    pub const LocalPtr = struct {
        const base_tag: Tag = .local_ptr;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        ident: u32,
        ptr: Hir.Ref,

        pub fn init(s: *Scope, ident: u32, ptr: Hir.Ref) LocalPtr {
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
        ident: u32,
        ref: Hir.Ref,

        pub fn init(s: *Scope, ident: u32, ref: Hir.Ref) @This() {
            return .{
                .parent = s,
                .ident = ident,
                .ref = ref,
            };
        }
    };

    pub fn resolveVar(inner: *Scope, ident: u32) !?*Scope {
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
                .local_type => s = s.cast(LocalType).?.parent,
            }
        }

        return found;
    }

    pub fn resolveType(inner: *Scope, ident: u32) !?*Scope {
        var found: ?*Scope = null;
        var shadows_scope: bool = false;
        var s: *Scope = inner;

        while (true) {
            switch (s.tag) {
                .module => break,
                .namespace => {
                    const namespace = s.cast(Namespace).?;
                    if (namespace.types.get(ident)) |_| {
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
                .local_val => s = s.cast(LocalVal).?.parent,
                .local_ptr => s = s.cast(LocalPtr).?.parent,
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

    pub fn resolveLoop(inner: *Scope) ?*Scope {
        var s: *Scope = inner;

        while (true) {
            switch (s.tag) {
                .module => break,
                .namespace => s = s.cast(Namespace).?.parent,
                .block => {
                    const block = s.cast(Block).?;
                    if (block.k == .loop) return s;
                    s = block.parent;
                },
                .local_val => s = s.cast(LocalVal).?.parent,
                .local_ptr => s = s.cast(LocalPtr).?.parent,
                .local_type => s = s.cast(LocalPtr).?.parent,
            }
        }

        return null;
    }
};

test "namespace member" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var interner = Interner.init(allocator);
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(&module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try std.testing.expectEqual(try namespace.base.resolveVar(apple), null);
    try std.testing.expectEqual(try namespace.base.resolveVar(banana), null);
    try std.testing.expectEqual(try namespace.base.resolveVar(cherry), null);

    try namespace.decls.putNoClobber(allocator, apple, 0);
    try namespace.decls.putNoClobber(allocator, banana, 1);
    try namespace.decls.putNoClobber(allocator, cherry, 2);

    try std.testing.expectEqual((try namespace.base.resolveVar(apple)).?, &namespace.base);
    try std.testing.expectEqual((try namespace.base.resolveVar(banana)).?, &namespace.base);
    try std.testing.expectEqual((try namespace.base.resolveVar(cherry)).?, &namespace.base);
}

test "local var" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var interner = Interner.init(allocator);
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(&module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    const b = indexToRef(1);
    const c = indexToRef(2);

    try namespace.decls.putNoClobber(allocator, apple, 0);
    var banana_var = Scope.LocalVal.init(&namespace.base, banana, b);
    var cherry_var = Scope.LocalVal.init(&banana_var.base, cherry, c);

    try std.testing.expectEqual((try namespace.base.resolveVar(apple)).?, &namespace.base);
    try std.testing.expectEqual((try namespace.base.resolveVar(banana)), null);
    try std.testing.expectEqual((try namespace.base.resolveVar(cherry)), null);

    try std.testing.expectEqual((try banana_var.base.resolveVar(apple)).?, &namespace.base);
    try std.testing.expectEqual((try banana_var.base.resolveVar(banana)).?, &banana_var.base);
    try std.testing.expectEqual((try banana_var.base.resolveVar(cherry)), null);

    try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &namespace.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &banana_var.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);
}

test "namespace shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var interner = Interner.init(allocator);
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(&module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    const c = indexToRef(2);
    const d = indexToRef(3);

    try namespace.decls.putNoClobber(allocator, apple, 0);
    try namespace.decls.putNoClobber(allocator, banana, 1);
    var cherry_var = Scope.LocalVal.init(&namespace.base, cherry, c);
    var illegal_var = Scope.LocalVal.init(&cherry_var.base, apple, d);

    try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &namespace.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &namespace.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);

    try std.testing.expectError(error.IdentifierShadowed, illegal_var.base.resolveVar(apple));
}

test "block shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var allocator = arena.allocator();

    var interner = Interner.init(allocator);
    var module = Scope.Module {};
    var outer = Scope.Block {
        .parent = &module.base,
        .instructions = .{},
        .hg = undefined,
        .scratch = .{},
        .force_comptime = true,
    };

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    const a = indexToRef(0);
    const b = indexToRef(1);
    const c = indexToRef(2);
    const d = indexToRef(3);
    const e = indexToRef(4);

    var apple_var = Scope.LocalVal.init(&outer.base, apple, a);
    var banana_var = Scope.LocalVal.init(&apple_var.base, banana, b);
    var inner = Scope.Block {
        .parent = &banana_var.base,
        .hg = undefined,
        .instructions = .{},
        .scratch = .{},
        .force_comptime = false,
    };
    var cherry_var = Scope.LocalVal.init(&inner.base, cherry, c);

    try std.testing.expectEqual((try cherry_var.base.resolveVar(apple)).?, &apple_var.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(banana)).?, &banana_var.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);

    var cherry_new_var = Scope.LocalVal.init(&cherry_var.base, cherry, d);
    try std.testing.expectEqual((try cherry_new_var.base.resolveVar(cherry)).?, &cherry_new_var.base);
    try std.testing.expectEqual((try cherry_var.base.resolveVar(cherry)).?, &cherry_var.base);

    var apple_new_var = Scope.LocalVal.init(&cherry_new_var.base, apple, e);
    try std.testing.expectError(error.IdentifierShadowed, apple_new_var.base.resolveVar(apple));
}
