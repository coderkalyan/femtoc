const std = @import("std");
const hir = @import("hir.zig");
const Interner = @import("interner.zig").Interner;

const Inst = hir.Inst;
pub const IdentifierError = error { Invalid, Shadowed };

pub const Scope = struct {
    tag: Tag,

    fn cast(base: *Scope, comptime T: type) ?*T {
        return @fieldParentPtr(T, "base", base);
    }

    fn parent(base: *Scope) ?*Scope {
        return switch (base.tag) {
            .toplevel => null,
            .namespace => base.cast(Namespace).?.parent,
            .block => base.cast(Block).?.parent,
            .local_var => base.cast(LocalVar).?.parent,
        };
    }

    const Tag = enum {
        toplevel,
        namespace,
        block,
        local_var,
    };

    const Toplevel = struct {
        const base_tag: Tag = .toplevel;
        base: Scope = .{ .tag = base_tag },
    };

    const Namespace = struct {
        const base_tag: Tag = .namespace;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        decls: std.AutoHashMap(u32, Inst.Ref),

        pub fn init(a: std.mem.Allocator, s: *Scope) @This() {
            return @This() {
                .parent = s,
                .decls = std.AutoHashMap(u32, Inst.Ref).init(a),
            };
        }
    };

    const Block = struct {
        const base_tag: Tag = .block;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,

        pub fn init(s: *Scope) @This() {
            return @This() {
                .parent = s,
            };
        }
    };

    const LocalVar = struct {
        const base_tag: Tag = .local_var;
        base: Scope = .{ .tag = base_tag },

        parent: *Scope,
        ident: u32,
        inst: Inst.Ref,

        pub fn init(s: *Scope, ident: u32, inst: Inst.Ref) @This() {
            return @This() {
                .parent = s,
                .ident = ident,
                .inst = inst,
            };
        }
    };

    fn resolveVar(scope: *Scope, ident: u32) !Inst.Ref {
        var found: ?Inst.Ref = null;
        var shadows_scope: bool = false;
        var s: *Scope = scope;

        while (true) {
            switch (s.tag) {
                .toplevel => break,
                .namespace => {
                    const namespace = s.cast(Namespace).?;
                    if (namespace.decls.get(ident)) |ref| {
                        if (found) |_| {
                            return IdentifierError.Shadowed;
                        } else {
                            found = ref;
                        }
                    }

                    s = namespace.parent;
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
                            found = local_var.inst;
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
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
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

    try namespace.decls.putNoClobber(apple, Inst.indexToRef(0));
    try namespace.decls.putNoClobber(banana, Inst.indexToRef(1));
    try namespace.decls.putNoClobber(cherry, Inst.indexToRef(2));

    try std.testing.expectEqual(Inst.indexToRef(0), try namespace.base.resolveVar(apple));
    try std.testing.expectEqual(Inst.indexToRef(1), try namespace.base.resolveVar(banana));
    try std.testing.expectEqual(Inst.indexToRef(2), try namespace.base.resolveVar(cherry));
}

test "local var" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, Inst.indexToRef(0));
    var banana_var = Scope.LocalVar.init(&namespace.base, banana, Inst.indexToRef(1));
    var cherry_var = Scope.LocalVar.init(&banana_var.base, cherry, Inst.indexToRef(2));

    try std.testing.expectEqual(Inst.indexToRef(0), try namespace.base.resolveVar(apple));
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(banana));
    try std.testing.expectError(IdentifierError.Invalid, namespace.base.resolveVar(cherry));

    try std.testing.expectEqual(Inst.indexToRef(0), try banana_var.base.resolveVar(apple));
    try std.testing.expectEqual(Inst.indexToRef(1), try banana_var.base.resolveVar(banana));
    try std.testing.expectError(IdentifierError.Invalid, banana_var.base.resolveVar(cherry));

    try std.testing.expectEqual(Inst.indexToRef(0), try cherry_var.base.resolveVar(apple));
    try std.testing.expectEqual(Inst.indexToRef(1), try cherry_var.base.resolveVar(banana));
    try std.testing.expectEqual(Inst.indexToRef(2), try cherry_var.base.resolveVar(cherry));
}

test "namespace shadowing" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    try namespace.decls.putNoClobber(apple, Inst.indexToRef(0));
    try namespace.decls.putNoClobber(banana, Inst.indexToRef(1));
    var cherry_var = Scope.LocalVar.init(&namespace.base, cherry, Inst.indexToRef(2));
    var illegal_var = Scope.LocalVar.init(&cherry_var.base, apple, Inst.indexToRef(3));

    try std.testing.expectEqual(Inst.indexToRef(0), try cherry_var.base.resolveVar(apple));
    try std.testing.expectEqual(Inst.indexToRef(1), try cherry_var.base.resolveVar(banana));
    try std.testing.expectEqual(Inst.indexToRef(2), try cherry_var.base.resolveVar(cherry));

    try std.testing.expectError(IdentifierError.Shadowed, illegal_var.base.resolveVar(apple));
}

test "block shadowing" {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(gpa.allocator());
    defer arena.deinit();

    var interner = Interner.init(arena.allocator());
    var toplevel = Scope.Toplevel {};
    var outer = Scope.Block.init(&toplevel.base);

    const apple = try interner.intern("apple");
    const banana = try interner.intern("banana");
    const cherry = try interner.intern("cherry");

    var apple_var = Scope.LocalVar.init(&outer.base, apple, Inst.indexToRef(0));
    var banana_var = Scope.LocalVar.init(&apple_var.base, banana, Inst.indexToRef(1));
    var inner = Scope.Block.init(&banana_var.base);
    var cherry_var = Scope.LocalVar.init(&inner.base, cherry, Inst.indexToRef(2));

    try std.testing.expectEqual(Inst.indexToRef(0), try cherry_var.base.resolveVar(apple));
    try std.testing.expectEqual(Inst.indexToRef(1), try cherry_var.base.resolveVar(banana));
    try std.testing.expectEqual(Inst.indexToRef(2), try cherry_var.base.resolveVar(cherry));

    var cherry_new_var = Scope.LocalVar.init(&cherry_var.base, cherry, Inst.indexToRef(3));
    try std.testing.expectEqual(Inst.indexToRef(3), try cherry_new_var.base.resolveVar(cherry));
    try std.testing.expectEqual(Inst.indexToRef(2), try cherry_var.base.resolveVar(cherry));

    var apple_new_var = Scope.LocalVar.init(&cherry_new_var.base, apple, Inst.indexToRef(4));
    try std.testing.expectError(IdentifierError.Shadowed, apple_new_var.base.resolveVar(apple));
}
