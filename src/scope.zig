const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const Interner = @import("interner.zig").Interner;

const Node = Ast.Node;
const Inst = Hir.Inst;
pub const IdentifierError = error { Invalid, Shadowed };

pub const Scope = struct {
    tag: Tag,

    pub fn cast(base: *Scope, comptime T: type) ?*T {
        return @fieldParentPtr(T, "base", base);
    }

    pub fn parent(base: *Scope) ?*Scope {
        return switch (base.tag) {
            .toplevel => null,
            .namespace => base.cast(Namespace).?.parent,
            .block => base.cast(Block).?.parent,
            .local_var => base.cast(LocalVar).?.parent,
        };
    }

    const Tag = enum {
        toplevel,
        gen_hir,
        namespace,
        block,
        local_var,
    };

    pub const Toplevel = struct {
        const base_tag: Tag = .toplevel;
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

    pub const LocalVar = struct {
        const base_tag: Tag = .local_var;
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

    pub fn resolveVar(scope: *Scope, ident: u32) !Node.Index {
        var found: ?Node.Index = null;
        var shadows_scope: bool = false;
        var s: *Scope = scope;

        while (true) {
            switch (s.tag) {
                .toplevel => break,
                .namespace => {
                    const namespace = s.cast(Namespace).?;
                    if (namespace.decls.get(ident)) |node| {
                        if (found) |_| {
                            return IdentifierError.Shadowed;
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
                .local_var => {
                    const local_var = s.cast(LocalVar).?;
                    if (local_var.ident == ident) {
                        if (found != null) {
                            if (shadows_scope) return IdentifierError.Shadowed;
                        } else {
                            found = local_var.node;
                        }
                    }

                    s = local_var.parent;
                },
            }
        }

        return found orelse IdentifierError.Invalid;
    }
};

test "namespace member" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(apple));
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(banana));
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(cherry));

    try namespace.decls.putNoClobber(apple, 0);
    try namespace.decls.putNoClobber(banana, 1);
    try namespace.decls.putNoClobber(cherry, 2);

    try std.testing.expectEqual(try namespace.base.resolveVar(apple), 0);
    try std.testing.expectEqual(try namespace.base.resolveVar(banana), 1);
    try std.testing.expectEqual(try namespace.base.resolveVar(cherry), 2);
}

test "local var" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, 0);
    var banana_var = Scope.LocalVar.init(&namespace.base, banana, 1);
    var cherry_var = Scope.LocalVar.init(&banana_var.base, cherry, 2);

    try std.testing.expectEqual(try namespace.base.resolveVar(apple), 0);
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(banana));
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(cherry));

    try std.testing.expectEqual(try banana_var.base.resolveVar(apple), 0);
    try std.testing.expectEqual(try banana_var.base.resolveVar(banana), 1);
    try std.testing.expectError(IdentifierError.Invalid, banana_var.base.resolveVar(cherry));

    try std.testing.expectEqual(try cherry_var.base.resolveVar(apple), 0);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(banana), 1);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(cherry), 2);
}

test "namespace shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, 0);
    try namespace.decls.putNoClobber(banana, 1);
    var cherry_var = Scope.LocalVar.init(&namespace.base, cherry, 2);
    var illegal_var = Scope.LocalVar.init(&cherry_var.base, apple, 3);

    try std.testing.expectEqual(try cherry_var.base.resolveVar(apple), 0);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(banana), 1);
    try std.testing.expectEqual(try cherry_var.base.resolveVar(cherry), 2);

    try std.testing.expectError(IdentifierError.Shadowed, illegal_var.base.resolveVar(apple));
}

test "block shadowing" {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var outer = Scope.Block.init(arena.allocator(), &toplevel.base);
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
    try std.testing.expectError(IdentifierError.Shadowed, apple_new_var.base.resolveVar(apple));
}
