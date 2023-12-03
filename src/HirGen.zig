const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseInt = @import("integerLiteral.zig").parseInt;
const parseFloat = @import("floatLiteral.zig").parseFloat;
const Scope = @import("scope.zig").Scope;
const interner = @import("interner.zig");
const Interner = interner.Interner;
const error_handler = @import("error_handler.zig");
const Value = @import("value.zig").Value;
const Type = @import("typing.zig").Type;
const BlockEditor = @import("hir/BlockEditor.zig");
const coercion = @import("hir/coercion.zig");

const implicit_return = @import("hir/implicit_return.zig");
const type_analysis = @import("hir/type_analysis.zig");
const dead_code_elimination = @import("hir/dead_code_elimination.zig");
const stack_analysis = @import("hir/stack_analysis.zig");

const Allocator = std.mem.Allocator;
const Inst = Hir.Inst;
const Ref = Hir.Ref;
const Node = Ast.Node;
const Token = lex.Token;
const Block = Scope.Block;
const indexToRef = Inst.indexToRef;
const refToIndex = Inst.refToIndex;
const HirGen = @This();

pub const GenError = error{
    NotImplemented,
    UnexpectedToken,
    InvalidIdentifier,
    InvalidRef,
    IdentifierShadowed,
    ConstAssign,
    AfterthoughtDecl,
};

const Error = GenError || interner.Error || Allocator.Error || @import("integerLiteral.zig").ParseError;

gpa: Allocator,
arena: Allocator,
tree: *const Ast,
insts: std.MultiArrayList(Inst),
extra: std.ArrayListUnmanaged(u32),
block_slices: std.ArrayListUnmanaged([]Hir.Index),
values: std.ArrayListUnmanaged(Value),
types: std.ArrayListUnmanaged(Type),
untyped_decls: std.AutoHashMapUnmanaged(u32, Hir.Index),
interner: Interner,
errors: std.ArrayListUnmanaged(error_handler.SourceError),
instmap: std.ArrayListUnmanaged(Ref),

pub fn generate(gpa: Allocator, tree: *const Ast) !Hir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var hirgen = HirGen{
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .insts = .{},
        .extra = .{},
        .block_slices = .{},
        .values = .{},
        .types = .{},
        .untyped_decls = .{},
        .interner = Interner.init(gpa),
        .errors = .{},
        .instmap = .{},
    };

    // post order format guarantees that the module node will be the last
    const module_node: u32 = @intCast(tree.nodes.len - 1);
    const module_index = try module(&hirgen, module_node);
    try implicit_return.executePass(&hirgen, module_index);
    try type_analysis.executePass(&hirgen, module_index);
    try stack_analysis.executePass(&hirgen, module_index);
    // try dead_code_elimination.executePass(&hirgen, module_index);

    return Hir{
        .insts = hirgen.insts.toOwnedSlice(),
        .module_index = module_index,
        .block_slices = try hirgen.block_slices.toOwnedSlice(gpa),
        .extra_data = try hirgen.extra.toOwnedSlice(gpa),
        .interner = hirgen.interner,
        .types = try hirgen.types.toOwnedSlice(gpa),
        .values = try hirgen.values.toOwnedSlice(gpa),
        .untyped_decls = try hirgen.untyped_decls.clone(gpa),
        .errors = try hirgen.errors.toOwnedSlice(gpa),
        .instmap = try hirgen.instmap.toOwnedSlice(gpa),
    };
}

const builtin_types = std.ComptimeStringMap(Hir.Ref, .{
    .{ "u8", .u8_ty },
    .{ "u16", .u16_ty },
    .{ "u32", .u32_ty },
    .{ "u64", .u64_ty },
    .{ "i8", .i8_ty },
    .{ "i16", .i16_ty },
    .{ "i32", .i32_ty },
    .{ "i64", .i64_ty },
    .{ "f32", .f32_ty },
    .{ "f64", .f64_ty },
    .{ "bool", .bool_ty },
    .{ "void", .void_ty },
});

pub fn addExtra(hg: *HirGen, extra: anytype) !Hir.ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try hg.extra.ensureUnusedCapacity(hg.gpa, fields.len);
    const len: u32 = @intCast(hg.extra.items.len);
    inline for (fields) |field| {
        switch (field.type) {
            u32 => hg.extra.appendAssumeCapacity(@field(extra, field.name)),
            Hir.Ref => hg.extra.appendAssumeCapacity(@intFromEnum(@field(extra, field.name))),
            else => unreachable,
        }
    }
    return len;
}

pub fn updateExtra(hg: *HirGen, index: Hir.ExtraIndex, extra: anytype) void {
    const fields = std.meta.fields(@TypeOf(extra));
    inline for (fields, 0..) |field, offset| {
        switch (field.type) {
            u32 => hg.extra.items[index + offset] = @field(extra, field.name),
            Hir.Ref => hg.extra.items[index + offset] = @intFromEnum(@field(extra, field.name)),
            else => unreachable,
        }
    }
}

pub fn extraData(hg: *HirGen, index: Hir.ExtraIndex, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            u32 => hg.extra.items[index + i],
            Ref => @enumFromInt(hg.extra.items[index + i]),
            else => unreachable,
        };
    }
    return result;
}

pub inline fn addInstUnlinked(hg: *HirGen, inst: Inst) !Hir.Index {
    const array_index: Hir.Index = @intCast(hg.insts.len);

    try hg.insts.ensureUnusedCapacity(hg.gpa, 1);
    try hg.instmap.ensureUnusedCapacity(hg.gpa, 1);

    hg.insts.appendAssumeCapacity(inst);
    hg.instmap.appendAssumeCapacity(indexToRef(array_index));

    return array_index;
}

pub inline fn updateRef(hg: *HirGen, index: Hir.Index, ref: Ref) void {
    hg.instmap.items[index] = ref;
}

pub inline fn resolveRef(hg: *HirGen, ref: Ref) Ref {
    return if (refToIndex(ref)) |index| hg.instmap[index] else ref;
}

fn addModule(hg: *HirGen, node: Node.Index) !Hir.Index {
    const decls = &hg.untyped_decls;
    const pl = try hg.addExtra(Inst.Module{
        .len = decls.size,
    });

    var it = decls.iterator();
    while (it.next()) |entry| {
        try hg.extra.appendSlice(hg.gpa, &.{ entry.key_ptr.*, entry.value_ptr.* });
    }

    return hg.addInstUnlinked(.{
        .tag = .module,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn integerLiteral(b: *BlockEditor, node: Node.Index) !Ref {
    const int_token = b.hg.tree.mainToken(node);
    const int_str = b.hg.tree.tokenString(int_token);
    const int = try parseInt(int_str);

    return switch (int) {
        0 => .zero_val,
        1 => .one_val,
        else => indexToRef(try b.addInt(int)),
    };
}

fn floatLiteral(b: *BlockEditor, node: Node.Index) !Ref {
    const float_token = b.hg.tree.mainToken(node);
    const float_str = b.hg.tree.tokenString(float_token);
    const float = try parseFloat(float_str);

    return indexToRef(try b.addFloat(float));
}

fn boolLiteral(b: *BlockEditor, node: Node.Index) Ref {
    const bool_token = b.hg.tree.mainToken(node);

    return switch (b.hg.tree.tokenTag(bool_token)) {
        .k_true => .btrue_val,
        .k_false => .bfalse_val,
        else => unreachable,
    };
}

fn variable(b: *BlockEditor, scope: *Scope, node: Node.Index) !Ref {
    const hg = b.hg;
    const ident_token = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_token);
    const id = try hg.interner.intern(ident_str);
    const var_scope = (try scope.resolveVar(id)) orelse {
        try b.hg.errors.append(b.hg.gpa, .{
            .tag = .invalid_identifer,
            .token = ident_token,
        });
        return error.InvalidIdentifier;
    };

    // for constants, the ref points to the instruction that returned its value
    // so we don't have to do anything else
    // but for mutable variables, the ref points to the address in memory so we have to
    // generate a load instruction to get the value at that address
    switch (var_scope.tag) {
        .local_val => {
            const local_val = var_scope.cast(Scope.LocalVal).?;
            return local_val.ref;
        },
        .local_ptr => {
            const local_ptr = var_scope.cast(Scope.LocalPtr).?;
            const addr = refToIndex(local_ptr.ptr).?;
            return indexToRef(try b.addLoad(addr, node));
        },
        .namespace => {
            return indexToRef(try b.addLoadGlobal(id, node));
        },
        else => unreachable,
    }
}

// TODO: this entire function is a mess and out of date
fn ty(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Ref {
    const hg = b.hg;
    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index);

    if (builtin_types.get(ident_str)) |ref| {
        return ref;
    } else {
        const id = try hg.interner.intern(ident_str);
        const ty_scope = (try scope.resolveType(ident_index)) orelse {
            try hg.errors.append(b.hg.gpa, .{
                .tag = .invalid_identifer,
                .token = ident_index,
            });
            return error.InvalidIdentifier;
        };
        switch (ty_scope.tag) {
            .local_type => {
                const local_type = ty_scope.cast(Scope.LocalType).?;
                return local_type.ref;
            },
            .namespace => {
                _ = id;
                unreachable;
                // const namespace = ty_scope.cast(Scope.Namespace).?;
                // const decl = namespace.types.get(id).?;
                // // have we already generated the instruction for this identifier?
                // if (hg.forward_map.get(decl)) |ref| {
                //     return ref;
                // } else {
                //     // nope, so just create a forward declaration
                //     unreachable;
                //     // return indexToRef(try addLoadInline(b, decl, node));
                // }
            },
            else => unreachable,
        }
    }
}

fn binaryRaw(b: *BlockEditor, node: Node.Index, op: Ast.TokenIndex, lref: Ref, rref: Ref) Error!Hir.Index {
    const tag: Inst.Tag = switch (b.hg.tree.tokenTag(op)) {
        .plus => .add,
        .plus_equal => .add,
        .minus => .sub,
        .minus_equal => .sub,
        .asterisk => .mul,
        .asterisk_equal => .mul,
        .slash => .div,
        .slash_equal => .div,
        .percent => .mod,
        .percent_equal => .mod,
        .equal_equal => .cmp_eq,
        .bang_equal => .cmp_ne,
        .l_angle_equal => .cmp_le,
        .r_angle_equal => .cmp_ge,
        .l_angle => .cmp_lt,
        .r_angle => .cmp_gt,
        else => return Error.UnexpectedToken,
    };

    return b.addBinary(lref, rref, tag, node);
}

// TODO: maybe try to get the AST to do this?
fn computeFunctionHash(tree: *const Ast, node: Node.Index, hash: []u8) void {
    const mainToken = tree.mainToken(node);
    const token_start = tree.tokens.items(.start)[mainToken];
    var hasher = std.crypto.hash.Blake3.init(.{});

    var brace_depth: u32 = 0;
    var seen_brace: bool = false;
    var token_index: u32 = token_start;
    while (!seen_brace or brace_depth > 0) : (token_index += 1) {
        const char: []const u8 = tree.source[token_index .. token_index + 1];
        hasher.update(char);
        switch (char[0]) {
            '{' => {
                seen_brace = true;
                brace_depth += 1;
            },
            '}' => brace_depth -= 1,
            else => {},
        }
    }

    hasher.final(hash);
}

fn binary(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const binary_expr = hg.tree.data(node).binary_expr;
    const operator_token = hg.tree.mainToken(node);
    return binaryRaw(b, node, operator_token, try expr(b, scope, binary_expr.left), try expr(b, scope, binary_expr.right));
}

fn fnDecl(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const arena = hg.arena;
    const fn_decl = hg.tree.data(node).fn_decl;
    const signature = hg.tree.extraData(fn_decl.signature, Node.FnSignature);

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    var s: *Scope = scope;
    const params = hg.tree.extra_data[signature.params_start..signature.params_end];
    try b.scratch.ensureUnusedCapacity(arena, params.len);
    for (params) |param| {
        const data = hg.tree.data(param).param;
        const param_token = hg.tree.mainToken(param);
        const param_str = hg.tree.tokenString(param_token);
        const param_id = try hg.interner.intern(param_str);

        const ty_ref = try ty(b, scope, data.ty);
        const param_index = try b.addParam(param_id, ty_ref, param);
        b.scratch.appendAssumeCapacity(param_index);

        const ref = Inst.indexToRef(param_index);
        var param_scope = try arena.create(Scope.LocalVal);
        param_scope.* = Scope.LocalVal.init(s, param_id, ref);
        s = &param_scope.base;
    }

    var body_scope = try Block.init(b, s);
    defer body_scope.deinit();

    const hir_params = b.scratch.items[scratch_top..];
    for (hir_params) |param| {
        try body_scope.editor.linkInst(param);
    }

    const return_type = try ty(&body_scope.editor, &body_scope.base, signature.return_ty);
    body_scope.return_ty = return_type;
    const body = try block(&body_scope.editor, &body_scope.base, fn_decl.body, true);
    var hash: u64 = undefined;
    computeFunctionHash(hg.tree, node, std.mem.asBytes(&hash));

    // unwind and free scope objects
    // TODO: idk if this is working properly
    while (s != scope) {
        const parent = s.parent().?;
        arena.destroy(s);
        s = parent;
    }

    return b.addFnDecl(hir_params, return_type, body, hash, node);
}

fn expr(b: *BlockEditor, scope: *Scope, node: Node.Index) !Ref {
    return switch (b.hg.tree.data(node)) {
        .integer_literal => integerLiteral(b, node),
        .float_literal => floatLiteral(b, node),
        .bool_literal => boolLiteral(b, node),
        .var_expr => variable(b, scope, node),
        .call_expr => indexToRef(try call(b, scope, node)),
        .binary_expr => indexToRef(try binary(b, scope, node)),
        .fn_decl => indexToRef(try fnDecl(b, scope, node)),
        else => {
            std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

// TODO: why do some of them not return?
fn statement(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!?Ref {
    _ = try switch (b.hg.tree.data(node)) {
        .const_decl => return try constDecl(b, scope, node),
        .const_decl_attr => return try constDeclAttr(b, scope, node),
        .var_decl => return try varDecl(b, scope, node),
        .assign_simple => assignSimple(b, scope, node),
        .assign_binary => assignBinary(b, scope, node),
        .if_simple => ifSimple(b, scope, node),
        .if_else => ifElse(b, scope, node),
        .if_chain => ifChain(b, scope, node),
        .return_val => returnStmt(b, scope, node),
        .loop_forever => loopForever(b, scope, node),
        .loop_conditional => loopConditional(b, scope, node),
        .loop_range => loopRange(b, scope, node),
        .loop_break => loopBreak(b, scope, node),
        else => {
            std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };

    return null;
}

fn globalStatement(hg: *HirGen, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const data = hg.tree.data(node);
    return try switch (data) {
        .const_decl => globalConst(hg, scope, node),
        .const_decl_attr => return try globalConstAttr(hg, scope, node),
        .var_decl => globalVar(hg, scope, node),
        else => {
            std.debug.print("Unexpected node: {}\n", .{hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn call(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const addr = try variable(b, scope, node);
    const call_expr = b.hg.tree.data(node).call_expr;

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    const arg_nodes = b.hg.tree.extra_data[call_expr.args_start..call_expr.args_end];
    try b.scratch.ensureUnusedCapacity(b.hg.arena, arg_nodes.len);
    for (arg_nodes) |arg_node| {
        const arg = try expr(b, scope, arg_node);
        b.scratch.appendAssumeCapacity(@intFromEnum(arg));
    }

    const args = b.scratch.items[scratch_top..];
    return b.addCall(addr, args, node);
}

fn block(b: *BlockEditor, scope: *Scope, node: Node.Index, comptime add_unlinked: bool) Error!Hir.Index {
    const hg = b.hg;
    const data = hg.tree.data(node).block;

    var s: *Scope = scope;
    const stmts = hg.tree.extra_data[data.stmts_start..data.stmts_end];
    for (stmts) |stmt| {
        // const ref = statement(b, s, stmt) catch continue;
        const ref = try statement(b, s, stmt);
        switch (b.hg.tree.data(stmt)) {
            .const_decl, .const_decl_attr => {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 1);
                const id = try hg.interner.intern(ident);
                const var_scope = try hg.arena.create(Scope.LocalVal);
                var_scope.* = Scope.LocalVal.init(s, id, ref.?);
                s = &var_scope.base;
            },
            .var_decl => {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                const id = try hg.interner.intern(ident);
                const var_scope = try hg.arena.create(Scope.LocalPtr);
                var_scope.* = Scope.LocalPtr.init(s, id, ref.?);
                s = &var_scope.base;
            },
            else => {},
        }
    }

    // unwind and free scope objects
    while (s != scope) {
        const parent = s.parent().?;
        hg.arena.destroy(s);
        s = parent;
    }

    if (add_unlinked) {
        return b.addBlockUnlinked(b, node);
        // return b.addBlock(b, node);
    } else {
        return undefined;
    }
}

fn coerce(b: *BlockEditor, s: *Scope, val: Ref, dest_ty: Ref, node: Node.Index) !Ref {
    _ = s;
    const coerce_data = try b.hg.addExtra(Inst.Coerce{
        .val = val,
        .ty = dest_ty,
    });

    // generate a "type validation" marker instruction
    // this is a passthrough which takes in the above value reference
    // and the type reference and returns the value reference
    // semantic analysis will validate that the type is as it should be
    // and then remove this instruction in the mir
    const coerce_inst = try b.addInst(.{
        .tag = .coerce,
        .data = .{ .pl_node = .{ .node = node, .pl = coerce_data } },
    });
    return indexToRef(coerce_inst);
}

fn constDecl(b: *BlockEditor, s: *Scope, node: Node.Index) !Ref {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope (by caller) so that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const hg = b.hg;
    const const_decl = hg.tree.data(node).const_decl;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index + 1);
    const id = try hg.interner.intern(ident_str);
    const ref = try expr(b, s, const_decl.val);
    _ = id;
    // _ = try b.addDebugValue(ref, id, node);

    if (const_decl.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try ty(b, s, const_decl.ty);
        return coerce(b, s, ref, dest_ty, node);
    }
}

fn constDeclAttr(b: *BlockEditor, s: *Scope, node: Node.Index) !Ref {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope such that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const hg = b.hg;
    const const_decl = hg.tree.data(node).const_decl_attr;
    const metadata = hg.tree.extraData(const_decl.metadata, Node.DeclMetadata);

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index + 1);
    const id = try hg.interner.intern(ident_str);
    const ref = try expr(b, s, const_decl.val);
    _ = id;
    // _ = try b.addDebugValue(ref, id, node);

    if (metadata.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try ty(b, s, metadata.ty);
        return coerce(b, s, ref, dest_ty, node);
    }
}

fn varDecl(b: *BlockEditor, s: *Scope, node: Node.Index) !Ref {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.hg.tree.data(node).var_decl;
    const val = try expr(b, s, var_decl.val);
    if (var_decl.ty == 0) {
        // untyped (inferred) declaration
        return indexToRef(try b.addPush(val, node));
    } else {
        // type annotated declaration
        const dest_ty = try ty(b, s, var_decl.ty);
        const coerced = try coerce(b, s, val, dest_ty, node);
        return indexToRef(try b.addPush(coerced, node));
    }
}

fn globalConst(hg: *HirGen, s: *Scope, node: Node.Index) !Hir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    const const_decl = hg.tree.data(node).const_decl;

    var inline_block = try Block.initInline(hg, s);
    defer inline_block.deinit();
    const scope = &inline_block.base;

    const rvalue_inner = try expr(&inline_block.editor, scope, const_decl.val);
    const rvalue = if (const_decl.ty == 0) rvalue_inner else ref: {
        // if this is typed, add a coerce
        const dest_ty = try ty(&inline_block.editor, scope, const_decl.ty);
        break :ref try coerce(&inline_block.editor, scope, rvalue_inner, dest_ty, node);
    };

    // const global = try inline_block.editor.addGlobal(rvalue, node);
    _ = try inline_block.editor.addYieldInline(rvalue, node);
    return BlockEditor.addBlockInlineUnlinked(hg, &inline_block.editor, node);
}

fn globalConstAttr(hg: *HirGen, s: *Scope, node: Node.Index) !Hir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    // this version supports attributes

    const const_decl = hg.tree.data(node).const_decl_attr;
    const metadata = hg.tree.extraData(const_decl.metadata, Node.DeclMetadata);

    var inline_block = try Block.initInline(hg, s);
    defer inline_block.deinit();
    const scope = &inline_block.base;
    const b = &inline_block.editor;

    var rvalue = try expr(b, scope, const_decl.val);
    if (metadata.ty != 0) {
        const dest_ty = try ty(b, scope, metadata.ty);
        rvalue = try coerce(b, scope, rvalue, dest_ty, node);
    }

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index + 1);
    const id = try hg.interner.intern(ident_str);
    _ = id;

    const attrs = hg.tree.extra_data[metadata.attrs_start..metadata.attrs_end];
    for (attrs) |attr| {
        switch (hg.tree.tokenTag(hg.tree.mainToken(attr))) {
            .a_export => rvalue = indexToRef(try b.addLinkExtern(rvalue, node)),
            else => unreachable,
        }
    }

    _ = try b.addYieldInline(rvalue, node);
    return BlockEditor.addBlockInlineUnlinked(hg, b, node);
}

fn globalVar(hg: *HirGen, s: *Scope, node: Node.Index) !Hir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = hg.tree.data(node).var_decl;

    var inline_block = try Block.initInline(hg, s);
    defer inline_block.deinit();
    const scope = &inline_block.base;

    const rvalue_inner = try expr(&inline_block.editor, scope, var_decl.val);
    const rvalue = if (var_decl.ty == 0) rvalue_inner else ref: {
        // if this is typed, add a coerce
        const dest_ty = try ty(&inline_block.editor, scope, var_decl.ty);
        break :ref try coerce(&inline_block.editor, scope, rvalue_inner, dest_ty, node);
    };

    const mut = try inline_block.editor.addGlobalMut(rvalue, node);
    _ = try inline_block.editor.addYieldInline(indexToRef(mut), node);
    return BlockEditor.addBlockInlineUnlinked(hg, &inline_block.editor, node);
}

fn assignLocalPtr(b: *BlockEditor, scope: *Scope, node: Node.Index, id: u32, val: Ref) !Hir.Index {
    const var_scope = (try scope.resolveVar(id)).?;
    const local_ptr = var_scope.cast(Scope.LocalPtr).?;
    const addr = refToIndex(local_ptr.ptr).?;

    return b.addStore(addr, val, node);
}

fn assignNamespace(b: *BlockEditor, scope: *Scope, node: Node.Index, id: Node.Index, val: Ref) !Hir.Index {
    _ = scope;
    const addr = try b.addLoadGlobal(id, node);
    return b.addStore(addr, val, node);
}

fn assignSimple(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_simple;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index);
    const id = try hg.interner.intern(ident_str);
    const var_scope = (try scope.resolveVar(id)) orelse {
        try hg.errors.append(b.hg.gpa, .{
            .tag = .invalid_identifer,
            .token = ident_index,
        });
        return error.InvalidIdentifier;
    };

    switch (var_scope.tag) {
        .local_val => {
            try hg.errors.append(hg.gpa, .{
                .tag = .const_assign,
                .token = ident_index,
            });
            return error.ConstAssign;
        },
        .local_ptr => {
            const valref = try expr(b, scope, assign.val);
            return assignLocalPtr(b, scope, node, id, valref);
        },
        .namespace => {
            const valref = try expr(b, scope, assign.val);
            return assignNamespace(b, scope, node, id, valref);
        },
        else => unreachable,
    }
}

fn branchCondition(b: *BlockEditor, scope: *Scope, node: Node.Index) !Ref {
    const ref = try expr(b, scope, node);
    return coerce(b, scope, ref, Ref.bool_ty, node);
}

fn assignBinary(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_binary;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index);
    const id = try hg.interner.intern(ident_str);
    const var_scope = (try scope.resolveVar(id)) orelse {
        try hg.errors.append(b.hg.gpa, .{
            .tag = .invalid_identifer,
            .token = ident_index,
        });
        return error.InvalidIdentifier;
    };
    switch (var_scope.tag) {
        .local_val => {
            try hg.errors.append(hg.gpa, .{
                .tag = .const_assign,
                .token = ident_index,
            });
            return error.ConstAssign;
        },
        .local_ptr => {
            const val = try variable(b, scope, node);
            const op = hg.tree.mainToken(node) + 1;
            const binIndex = try binaryRaw(b, node, op, val, try expr(b, scope, assign.val));
            return assignLocalPtr(b, scope, node, id, indexToRef(binIndex));
        },
        .namespace => {
            const val = try variable(b, scope, node);
            const op = hg.tree.mainToken(node) + 1;
            const binIndex = try binaryRaw(b, node, op, val, try expr(b, scope, assign.val));
            return assignNamespace(b, scope, node, id, indexToRef(binIndex));
        },
        else => unreachable,
    }
}

fn ifSimple(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_simple = b.hg.tree.data(node).if_simple;
    var block_scope = try Block.init(b, scope);
    defer block_scope.deinit();
    const s = &block_scope.base;

    const condition = try branchCondition(b, scope, if_simple.condition);
    const exec = try block(&block_scope.editor, s, if_simple.exec_true, true);
    return b.addBranchSingle(condition, exec, node);
}

fn ifElse(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_else = b.hg.tree.data(node).if_else;

    const condition = try branchCondition(b, scope, if_else.condition);
    const exec = b.hg.tree.extraData(if_else.exec, Node.IfElse);
    const exec_true = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope.editor, &block_scope.base, exec.exec_true, true);
    };
    const exec_false = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope.editor, &block_scope.base, exec.exec_false, true);
    };
    return b.addBranchDouble(condition, exec_true, exec_false, node);
}

fn ifChain(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_chain = b.hg.tree.data(node).if_chain;

    const condition = try branchCondition(b, scope, if_chain.condition);
    const chain = b.hg.tree.extraData(if_chain.chain, Node.IfChain);
    const exec_true = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope.editor, &block_scope.base, chain.exec_true, true);
    };
    const next = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();
        _ = switch (b.hg.tree.data(chain.next)) {
            .if_simple => try ifSimple(&block_scope.editor, &block_scope.base, chain.next),
            .if_else => try ifElse(&block_scope.editor, &block_scope.base, chain.next),
            else => unreachable,
        };

        break :block try b.addBlockUnlinked(&block_scope.editor, node);
    };

    return b.addBranchDouble(condition, exec_true, next, node);
}

fn returnStmt(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const return_val = b.hg.tree.data(node).return_val;
    const operand = if (return_val.val == 0) Ref.void_val else op: {
        const ref = try expr(b, scope, return_val.val);
        const bl = scope.resolveBlock().?;
        break :op try coerce(b, scope, ref, bl.return_ty, node);
    };

    return b.addRetNode(operand, node);
}

fn loopForever(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_forever = b.hg.tree.data(node).loop_forever;
    var loop_scope = try Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope.editor, &loop_scope.base, loop_forever.body, true);

    const condition = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();

        // TODO: should be implicit not node
        const condition = try coerce(&block_scope.editor, scope, Ref.btrue_val, Ref.bool_ty, node);
        // const index = refToIndex(condition).?;
        // break :block index;
        _ = try block_scope.editor.addYieldImplicit(condition, node);
        break :block try b.addBlockUnlinked(&block_scope.editor, node);
    };

    return b.addLoop(condition, body, node);
}

fn loopConditional(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_conditional = b.hg.tree.data(node).loop_conditional;

    const condition = block: {
        var block_scope = try Block.init(b, scope);
        defer block_scope.deinit();
        const s = &block_scope.base;

        const condition_inner = try expr(&block_scope.editor, s, loop_conditional.condition);
        const condition = try coerce(&block_scope.editor, s, condition_inner, Ref.bool_ty, node);
        _ = try block_scope.editor.addYieldImplicit(condition, node);
        break :block try b.addBlockUnlinked(&block_scope.editor, node);
    };

    var loop_scope = try Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope.editor, &loop_scope.base, loop_conditional.body, true);

    return b.addLoop(condition, body, node);
}

fn loopRange(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const loop_range = hg.tree.data(node).loop_range;
    const signature = hg.tree.extraData(loop_range.signature, Node.RangeSignature);

    var s: *Scope = if (try statement(b, scope, signature.binding)) |ref| var_scope: {
        const ident = hg.tree.tokenString(hg.tree.mainToken(signature.binding) + 2);
        const id = try hg.interner.intern(ident);
        var var_scope = Scope.LocalPtr.init(scope, id, ref);
        break :var_scope &var_scope.base;
    } else scope;

    const condition = block: {
        var block_scope = try Block.init(b, s);
        defer block_scope.deinit();
        const bs = &block_scope.base;

        const condition_inner = try expr(&block_scope.editor, bs, signature.condition);
        const condition = try coerce(&block_scope.editor, bs, condition_inner, Ref.bool_ty, node);
        _ = try block_scope.editor.addYieldImplicit(condition, undefined);
        break :block try b.addBlockUnlinked(&block_scope.editor, node);
    };

    // we have a block (loop outer body) that contains the afterthought
    // and then an inner nested block that contains the user loop body
    const body = body: {
        var outer_scope = try Block.init(b, s);
        defer outer_scope.deinit();

        var inner_scope = try Block.init(&outer_scope.editor, &outer_scope.base);
        defer inner_scope.deinit();

        _ = try block(&inner_scope.editor, &inner_scope.base, loop_range.body, false);
        _ = try outer_scope.editor.addBlock(&inner_scope.editor, node);

        if (try statement(&outer_scope.editor, &outer_scope.base, signature.afterthought)) |_| {
            return error.AfterthoughtDecl;
        }

        break :body try b.addBlockUnlinked(&outer_scope.editor, node);
    };

    return b.addLoop(condition, body, node);
}

fn loopBreak(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    _ = scope;
    return b.addBreak(node);
}

fn globalIdentId(hg: *HirGen, stmt: u32) !u32 {
    const ident = switch (hg.tree.data(stmt)) {
        .const_decl, .const_decl_attr => hg.tree.tokenString(hg.tree.mainToken(stmt) + 1),
        .var_decl => hg.tree.tokenString(hg.tree.mainToken(stmt) + 2),
        else => unreachable,
    };

    const id = try hg.interner.intern(ident);
    return id;
}

fn module(hg: *HirGen, node: Node.Index) !Hir.Index {
    const data = hg.tree.data(node).module;

    var module_scope = Scope.Module{}; // doesn't hold anything, just a toplevel sentinel
    var namespace = Scope.Namespace.init(&module_scope.base);

    // first pass through statements - "forward declare" all identifiers
    // in the toplevel namespace so that forward-referencing and recursive
    // function calls resolve
    // note that since we link to the node id, we can resolve identifiers
    // that haven't been irgen'ed yet
    var stmts = hg.tree.extra_data[data.stmts_start..data.stmts_end];
    for (stmts) |stmt| {
        const id = try globalIdentId(hg, stmt);
        try namespace.decls.put(hg.arena, id, {});
    }

    // second pass - generate the value/expression and update the resolution map
    // so we can link node ids to instructions
    for (stmts) |stmt| {
        const id = try globalIdentId(hg, stmt);
        const inst = try globalStatement(hg, &namespace.base, stmt);
        switch (hg.tree.data(stmt)) {
            .const_decl, .const_decl_attr, .var_decl => {
                try hg.untyped_decls.put(hg.gpa, id, inst);
            },
            else => {
                std.debug.print("Unexpected node: {}\n", .{hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        }
    }

    return try addModule(hg, node);
}

pub inline fn resolveType(hg: *HirGen, ref: Ref) Type {
    return hg.getTempHir().resolveType(ref);
}

fn getTempHir(hg: *HirGen) Hir {
    return .{
        .insts = hg.insts.slice(),
        .extra_data = hg.extra.items,
        .values = hg.values.items,
        .types = hg.types.items,
        .block_slices = hg.block_slices.items,
        .interner = hg.interner,
        .instmap = hg.instmap.items,
        .untyped_decls = hg.untyped_decls, // TODO: not good
        .errors = hg.errors.items,
        .module_index = undefined,
    };
}

pub inline fn refToInt(hg: *HirGen, ref: Hir.Ref) u64 {
    return hg.getTempHir().refToInt(ref);
}

pub inline fn refToFloat(hg: *HirGen, ref: Hir.Ref) f64 {
    return hg.getTempHir().refToFloat(ref);
}
