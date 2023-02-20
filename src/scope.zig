const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const Interner = @import("interner.zig").Interner;

const Node = Ast.Node;
const Inst = Hir.Inst;
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
        gen_hir,
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
        decls: std.AutoHashMap(u32, Node.Index),

        pub fn init(a: std.mem.Allocator, s: *Scope) @This() {
            return @This() {
                .parent = s,
                .decls = std.AutoHashMap(u32, Node.Index).init(a),
            };
        }
    };

    pub const Block = struct {
        const base_tag: Tag = .block;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        k: Kind,

        pub const Self = @This();
        pub const Kind = enum {
            toplevel,
            function,
        };

        pub fn init(s: *Scope, k: Kind) Self {
            return Self {
                .parent = s,
                .k = k,
            };
        }
    };

    pub const GenHir = struct {
        const base_tag: Tag = .gen_hir;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        arena: std.mem.Allocator,
        scratch: std.ArrayList(Hir.Index),

        pub const Self = @This();

        pub fn init(arena: std.mem.Allocator, s: *Scope) Self {
            return Self {
                .parent = s,
                .arena = arena,
                .scratch = std.ArrayList(Hir.Index).init(arena),
            };
        }

        pub fn addInst(gh: *Self, index: Hir.Index) !void {
            try gh.scratch.append(index);
        }
    };

    pub const LocalVal = struct {
        const base_tag: Tag = .local_val;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        ident: u32,
        ref: Hir.Ref,

        pub fn init(s: *Scope, ident: u32, ref: Hir.Ref) @This() {
            return @This() {
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

        pub fn init(s: *Scope, ident: u32, ptr: Hir.Ref) @This() {
            return @This() {
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
        node: Node.Index,

        pub fn init(s: *Scope, ident: u32, node: Node.Index) @This() {
            return @This() {
                .parent = s,
                .ident = ident,
                .node = node,
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
                .gen_hir => {
                    const genhir = s.cast(GenHir).?;
                    s = genhir.parent;
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

    pub fn resolveType(inner: *Scope, ident: u32) ?*Scope {
        var found: ?*Scope = null;
        var shadows_scope: bool = false;
        var s: *Scope = inner;

        while (true) {
            switch (s.tag) {
                .module => break,
                .namespace => {
                    const namespace = s.cast(Namespace).?;
                    if (namespace.decls.get(ident)) |node| {
                        if (found) |_| {
                            return error.IdentifierShadowed;
                        } else {
                            found = node;
                        }
                    }

                    s = namespace.parent;
                },
                .gen_hir => {
                    const genhir = s.cast(GenHir).?;
                    s = genhir.parent;
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
                            if (shadows_scope) error.IdentifierShadowed;
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
};

test "namespace member" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(arena.allocator(), &module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try std.testing.expectEqual(try namespace.base.resolveVar(apple), null);
    try std.testing.expectEqual(try namespace.base.resolveVar(banana), null);
    try std.testing.expectEqual(try namespace.base.resolveVar(cherry), null);

    try namespace.decls.putNoClobber(apple, 0);
    try namespace.decls.putNoClobber(banana, 1);
    try namespace.decls.putNoClobber(cherry, 2);

    try std.testing.expectEqual((try namespace.base.resolveVar(apple)).?, 0);
    try std.testing.expectEqual((try namespace.base.resolveVar(banana)).?, 1);
    try std.testing.expectEqual((try namespace.base.resolveVar(cherry)).?, 2);
}

test "local var" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(arena.allocator(), &module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, 0);
    var banana_var = Scope.LocalVar.init(&namespace.base, banana, 1);
    var cherry_var = Scope.LocalVar.init(&banana_var.base, cherry, 2);

    try std.testing.expectEqual(namespace.base.resolveVar(apple).?, 0);
    try std.testing.expectEqual(namespace.base.resolveVar(banana), null);
    try std.testing.expectEqual(namespace.base.resolveVar(cherry), null);

    try std.testing.expectEqual(banana_var.base.resolveVar(apple).?, 0);
    try std.testing.expectEqual(banana_var.base.resolveVar(banana).?, 1);
    try std.testing.expectEqual(banana_var.base.resolveVar(cherry), null);

    try std.testing.expectEqual(cherry_var.base.resolveVar(apple).?, 0);
    try std.testing.expectEqual(cherry_var.base.resolveVar(banana).?, 1);
    try std.testing.expectEqual(cherry_var.base.resolveVar(cherry).?, 2);
}

test "namespace shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var module = Scope.Module {};
    var namespace = Scope.Namespace.init(arena.allocator(), &module.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, 0);
    try namespace.decls.putNoClobber(banana, 1);
    var cherry_var = Scope.LocalVar.init(&namespace.base, cherry, 2);
    var illegal_var = Scope.LocalVar.init(&cherry_var.base, apple, 3);

    try std.testing.expectEqual(cherry_var.base.resolveVar(apple).?, 0);
    try std.testing.expectEqual(cherry_var.base.resolveVar(banana).?, 1);
    try std.testing.expectEqual(cherry_var.base.resolveVar(cherry).?, 2);

    try std.testing.expectError(error.IdentifierShadowed, illegal_var.base.resolveVar(apple));
}

test "block shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var module = Scope.Module {};
    var outer = Scope.Block.init(arena.allocator(), &module.base);
    defer outer.deinit();

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    var apple_var = Scope.LocalVar.init(&outer.base, apple, 0);
    var banana_var = Scope.LocalVar.init(&apple_var.base, banana, 1);
    var inner_arena = std.heap.ArenaAllocator.init(arena.allocator());
    var inner = Scope.Block.init(inner_arena.allocator(), &banana_var.base);
    defer inner.deinit();
    var cherry_var = Scope.LocalVar.init(&inner.base, cherry, 2);

    try std.testing.expectEqual(try cherry_var.base.resolveVar(apple), 0);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(banana), 1);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(cherry), 2);

    var cherry_new_var = Scope.LocalVar.init(&cherry_var.base, cherry, 3);
    try std.testing.expectEqual(try cherry_new_var.base.resolveVar(cherry), 3);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(cherry), 2);

    var apple_new_var = Scope.LocalVar.init(&cherry_new_var.base, apple, 4);
    try std.testing.expectError(error.IdentifierShadowed, apple_new_var.base.resolveVar(apple));
}
