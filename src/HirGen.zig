const std = @import("std");
const Hir = @import ("Hir.zig");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseInt = @import("integerLiteral.zig").parseInt;
const parseFloat = @import("floatLiteral.zig").parseFloat;
const Scope = @import("scope.zig").Scope;
const interner = @import("interner.zig");
const Interner = interner.Interner;

const Allocator = std.mem.Allocator;
const Inst = Hir.Inst;
const Ref = Hir.Ref;
const Node = Ast.Node;
const Token = lex.Token;
const Block = Scope.Block;
const indexToRef = Inst.indexToRef;
const refToIndex = Inst.refToIndex;
const HirGen = @This();

pub const GenError = error {
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
instructions: std.MultiArrayList(Inst),
extra: std.ArrayListUnmanaged(u32),
interner: Interner,
forward_map: std.AutoHashMapUnmanaged(Node.Index, Hir.Ref),

pub fn generate(gpa: Allocator, tree: *const Ast) !Hir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var hirgen = HirGen {
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .instructions = .{},
        .extra = .{},
        .interner = Interner.init(gpa),
        .forward_map = .{},
    };

    var module_scope = Scope.Module {}; // doesn't hold anything, just a toplevel sentinel
    var b = Block {
        .parent = &module_scope.base,
        .instructions = .{},
        .scratch = .{},
        .hg = &hirgen,
        .force_comptime = true,
        .return_ty = @intToEnum(Hir.Ref, 0),
    };

    // post order format guarantees that the module node will be the last
    const module_node = @intCast(u32, tree.nodes.len - 1);
    try module(&b, &b.base, module_node);

    return Hir {
        .insts = hirgen.instructions.toOwnedSlice(),
        .extra_data = hirgen.extra.toOwnedSlice(gpa),
        .interner = hirgen.interner,
        .resolution_map = hirgen.forward_map,
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

fn addExtra(hg: *HirGen, extra: anytype) !Hir.ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try hg.extra.ensureUnusedCapacity(hg.gpa, fields.len);
    const len = @intCast(u32, hg.extra.items.len);
    inline for (fields) |field| {
        switch (field.field_type) {
            u32 => hg.extra.appendAssumeCapacity(@field(extra, field.name)),
            Hir.Ref => hg.extra.appendAssumeCapacity(@enumToInt(@field(extra, field.name))),
            else => unreachable,
        }
    }
    return len;
}

fn addInt(b: *Block, int: u64) !Hir.Index {
    return b.addInst(.{
        .tag = .int,
        .data = .{ .int = int },
    });
}

fn addFloat(b: *Block, float: f64) !Hir.Index {
    return b.addInst(.{
        .tag = .float,
        .data = .{ .float = float },
    });
}

fn addBinary(b: *Block, l: Hir.Ref, r: Hir.Ref, tag: Inst.Tag, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Binary {
        .lref = l,
        .rref = r,
    });

    return b.addInst(.{
        .tag = tag,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addBlock(b: *Block, insts: []Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Block {
        .len = @intCast(u32, insts.len),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, insts);

    return b.addInst(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addBlockInline(b: *Block, insts: []Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Block {
        .len = @intCast(u32, insts.len),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, insts);

    return b.addInst(.{
        .tag = .block_inline,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addCall(b: *Block, addr: Hir.Ref, args: []u32, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Call {
        .addr = addr,
        .args_len = @intCast(u32, args.len),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, args);

    return b.addInst(.{
        .tag = .call,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addFnDecl(b: *Block, params: []u32, return_type: Hir.Ref,
             body: Hir.Index, hash: u64, node: Node.Index) !Hir.Index {
    const params_start = @intCast(u32, b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, params);
    const params_end = @intCast(u32, b.hg.extra.items.len);

    const pl = try b.hg.addExtra(Inst.FnDecl {
        .params_start = params_start,
        .params_end = params_end,
        .return_type = return_type,
        .body = body,
        .hash_lower = @truncate(u32, hash),
        .hash_upper = @truncate(u32, hash >> 32),
    });

    return b.addInst(.{
        .tag = .fn_decl,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addParam(b: *Block, name: u32, ty_ref: Hir.Ref, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Param {
        .name = name,
        .ty = ty_ref,
    });

    const index = @intCast(u32, hg.instructions.len);
    try hg.instructions.append(hg.gpa, .{
        .tag = .param,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
    return index;
}

fn addDebugValue(b: *Block, val: Hir.Ref, name: u32, node: Node.Index) !Hir.Index {
    // TODO: enum and member naming
    const pl = try b.hg.addExtra(Inst.DebugValue {
        .name = name,
        .value = val,
    });

    return b.addInst(.{
        .tag = .dbg_value,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addAllocPush(b: *Block, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .alloc_push,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

fn addLoad(b: *Block, addr: Hir.Index, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .load,
        .data = .{ .pl_node = .{ .pl = addr, .node = node } },
    });
}

fn addLoadInline(b: *Block, decl: u32, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .load_inline,
        .data = .{ .pl_node = .{ .pl = decl, .node = node } },
    });
}

fn addStore(b: *Block, addr: Hir.Index, val: Hir.Ref, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Store {
        .addr = addr,
        .val = val,
    });

    return b.addInst(.{
        .tag = .store,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addBranchSingle(b: *Block, cond: Hir.Ref, exec: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.BranchSingle {
        .condition = cond,
        .exec_true = exec,
    });

    return b.addInst(.{
        .tag = .branch_single,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addBranchDouble(b: *Block, cond: Hir.Ref,
                   x: Hir.Index, y: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.BranchDouble {
        .condition = cond,
        .exec_true = x,
        .exec_false = y,
    });

    return b.addInst(.{
        .tag = .branch_double,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addRetNode(b: *Block, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .ret_node,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

fn addRetImplicit(b: *Block, val: Hir.Ref, tok: Ast.TokenIndex) !Hir.Index {
    return b.addInst(.{
        .tag = .ret_implicit,
        .data = .{ .un_tok = .{ .operand = val, .tok = tok } },
    });
}

fn addYieldImplicit(b: *Block, val: Hir.Ref, tok: Ast.TokenIndex) !Hir.Index {
    return b.addInst(.{
        .tag = .yield_implicit,
        .data = .{ .un_tok = .{ .operand = val, .tok = tok } },
    });
}

fn addYieldInline(b: *Block, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .yield_inline,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

fn addLoop(b: *Block, cond: Hir.Index, body: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Loop {
        .condition = cond,
        .body = body,
    });

    return b.addInst(.{
        .tag = .loop,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn addBreak(b: *Block, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .loop_break,
        .data = .{ .node = node },
    });
}

fn addContinue(b: *Block, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .loop_continue,
        .data = .{ .node = node },
    });
}

fn addDeclConst(b: *Block, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .decl_const,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

fn addDeclMut(b: *Block, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .decl_mut,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

fn addModule(b: *Block, members: []u32, node: Node.Index) !Hir.Index {
    std.debug.assert(members.len % 2 == 0);
    const pl = try b.hg.addExtra(Inst.Module {
        .len = @intCast(u32, members.len / 2),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, members);

    return b.addInst(.{
        .tag = .module,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

fn integerLiteral(b: *Block, node: Node.Index) !Ref {
    const int_token = b.hg.tree.mainToken(node);
    const int_str = b.hg.tree.tokenString(int_token);
    const int = try parseInt(int_str);

    return switch (int) {
        0 => .zero_val,
        1 => .one_val,
        else => indexToRef(try addInt(b, int)),
    };
}

fn floatLiteral(b: *Block, node: Node.Index) !Ref {
    const float_token = b.hg.tree.mainToken(node);
    const float_str = b.hg.tree.tokenString(float_token);
    const float = try parseFloat(float_str);

    return indexToRef(try addFloat(b, float));
}

fn boolLiteral(b: *Block, node: Node.Index) Ref {
    const bool_token = b.hg.tree.mainToken(node);

    return switch (b.hg.tree.tokenTag(bool_token)) {
        .k_true => .btrue_val,
        .k_false => .bfalse_val,
        else => unreachable,
    };
}

fn variable(b: *Block, scope: *Scope, node: Node.Index) !Ref {
    const hg = b.hg;
    const ident_token = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_token);
    const id = try hg.interner.intern(ident_str);
    const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;

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
            return indexToRef(try addLoad(b, addr, node));
        },
        .namespace => {
            const namespace = var_scope.cast(Scope.Namespace).?;
            const decl = namespace.decls.get(id).?;
            // TODO: load the declaration, don't return pointer
            // have we already generated the instruction for this identifier?
            if (hg.forward_map.get(decl)) |ref| {
                return ref;
            } else {
                // nope, so just create a forward declaration
                return indexToRef(try addLoadInline(b, decl, node));
            }
        },
        else => unreachable,
    }
}

fn ty(b: *Block, scope: *Scope, node: Node.Index) !Hir.Ref {
    const hg = b.hg;
    const nodes = hg.tree.nodes;
    const ident_index = nodes.items(.main_token)[node];
    const ident_str = hg.tree.tokenString(ident_index);

    if (builtin_types.get(ident_str)) |ref| {
        return ref;
    } else {
        const id = try hg.interner.intern(ident_str);
        const ty_scope = try scope.resolveType(ident_index) orelse return error.InvalidIdentifier;
        switch (ty_scope.tag) {
            .local_type => {
                const local_type = ty_scope.cast(Scope.LocalType).?;
                return local_type.ref;
            },
            .namespace => {
                const namespace = ty_scope.cast(Scope.Namespace).?;
                const decl = namespace.types.get(id).?;
                // have we already generated the instruction for this identifier?
                if (hg.forward_map.get(decl)) |ref| {
                    return ref;
                } else {
                    // nope, so just create a forward declaration
                    return indexToRef(try addLoadInline(b, decl, node));
                }
            },
            else => unreachable,
        }
    }
}

fn binaryRaw(b: *Block, node: Node.Index, op: Ast.TokenIndex, lref: Ref, rref: Ref) Error!Hir.Index {
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

    return addBinary(b, lref, rref, tag, node);
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
        const char: []const u8 = tree.source[token_index..token_index + 1];
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

fn binary(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const binary_expr = hg.tree.data(node).binary_expr;
    const operator_token = hg.tree.mainToken(node);
    return binaryRaw(b, node, operator_token, try expr(b, scope, binary_expr.left), try expr(b, scope, binary_expr.right));
}

fn fnDecl(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
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
        const param_index = try addParam(b, param_id, ty_ref, param);
        b.scratch.appendAssumeCapacity(param_index);

        const ref = Inst.indexToRef(param_index);
        var param_scope = try arena.create(Scope.LocalVal);
        param_scope.* = Scope.LocalVal.init(s, param_id, ref);
        s = &param_scope.base;
    }


    var block_scope = Block.init(b, s);
    defer block_scope.deinit();
    s = &block_scope.base;
    var body_scope = Scope.Body { .parent = s, .fn_node = node };
    s = &body_scope.base;

    const return_type = try ty(&block_scope, s, signature.return_ty);
    block_scope.return_ty = return_type;
    const body = try block(&block_scope, s, fn_decl.body);
    var hash: u64 = undefined;
    computeFunctionHash(hg.tree, node, std.mem.asBytes(&hash));

    // unwind and free scope objects
    while (s != scope) {
        const parent = s.parent().?;
        arena.destroy(s);
        s = parent;
    }

    const hir_params = b.scratch.items[scratch_top..];
    return addFnDecl(b, hir_params, return_type, body, hash, node);
}

fn expr(b: *Block, scope: *Scope, node: Node.Index) !Ref {
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
fn statement(b: *Block, scope: *Scope, node: Node.Index) Error!?Ref {
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
        .loop_continue => loopContinue(b, scope, node),
        else => {
            std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };

    return null;
}

fn global_statement(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const data = b.hg.tree.data(node);
    return try switch (data) {
        .const_decl => globalConst(b, scope, node),
        // .const_decl_attr => return try constDeclAttr(b, scope, node),
        .var_decl => globalVar(b, scope, node),
        else => {
            std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn call(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const addr = try variable(b, scope, node);
    const call_expr = b.hg.tree.data(node).call_expr;

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    const arg_nodes = b.hg.tree.extra_data[call_expr.args_start..call_expr.args_end];
    try b.scratch.ensureUnusedCapacity(b.hg.arena, arg_nodes.len);
    for (arg_nodes) |arg_node| {
        const arg = try expr(b, scope, arg_node);
        b.scratch.appendAssumeCapacity(@enumToInt(arg));
    }

    const args = b.scratch.items[scratch_top..];
    return addCall(b, addr, args, node);
}

fn block(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const data = hg.tree.data(node).block;

    var s: *Scope = scope;
    const stmts = hg.tree.extra_data[data.stmts_start..data.stmts_end];
    for (stmts) |stmt| {
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

    if ((scope.tag == .body) and !blockReturns(b)) {
        // insert implicit return
        // TODO: find the closing brace as token
        _ = try addRetImplicit(b, Ref.void_val, undefined);
    }

    return addBlock(b, b.instructions.items, node);
}

fn coerce(b: *Block, s: *Scope, val: Ref, dest_ty: Ref, node: Node.Index) !Ref {
    _ = s;
    const coerce_data = try b.hg.addExtra(Inst.Coerce {
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

fn constDecl(b: *Block, s: *Scope, node: Node.Index) !Ref {
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
    _ = try addDebugValue(b, ref, id, node);

    if (const_decl.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try ty(b, s, const_decl.ty);
        return coerce(b, s, ref, dest_ty, node);
    }
}

fn constDeclAttr(b: *Block, s: *Scope, node: Node.Index) !Ref {
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
    _ = try addDebugValue(b, ref, id, node);

    if (metadata.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try ty(b, s, metadata.ty);
        return coerce(b, s, ref, dest_ty, node);
    }
}

fn varDecl(b: *Block, s: *Scope, node: Node.Index) !Ref {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.hg.tree.data(node).var_decl;
    const val = try expr(b, s, var_decl.val);
    if (var_decl.ty == 0) {
        // untyped (inferred) declaration
        return indexToRef(try addAllocPush(b, val, node));
    } else {
        // type annotated declaration
        const dest_ty = try ty(b, s, var_decl.ty);
        const coerced = try coerce(b, s, val, dest_ty, node);
        return indexToRef(try addAllocPush(b, coerced, node));
    }
}

fn globalConst(b: *Block, s: *Scope, node: Node.Index) !Hir.Index {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope such that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const hg = b.hg;
    const const_decl = hg.tree.data(node).const_decl;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index + 1);
    const id = try hg.interner.intern(ident_str);
    _ = id;

    var block_inline = Block.init(b, s);
    const scope = &block_inline.base;
    defer block_inline.deinit();

    const ref = try expr(&block_inline, scope, const_decl.val);
    const val = if (const_decl.ty == 0) ref: {
        // untyped (inferred) declaration
        break :ref ref;
    } else ref: {
        const dest_ty = try ty(&block_inline, scope, const_decl.ty);
        break :ref try coerce(&block_inline, scope, ref, dest_ty, node);
    };

    const yield_val = val: {
        if (refToIndex(val)) |index| {
            if (hg.instructions.items(.tag)[index] == .fn_decl) break :val val;
        }

        break :val indexToRef(try addDeclConst(&block_inline, val, node));
    };

    _ = try addYieldInline(&block_inline, yield_val, node);
    return addBlockInline(b, block_inline.instructions.items, node);
}

fn globalVar(b: *Block, s: *Scope, node: Node.Index) !Hir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const hg = b.hg;
    const var_decl = hg.tree.data(node).var_decl;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index + 2);
    const id = try hg.interner.intern(ident_str);
    _ = id;

    var block_inline = Block.init(b, s);
    const scope = &block_inline.base;
    defer block_inline.deinit();

    const ref = try expr(&block_inline, scope, var_decl.val);
    const val = if (var_decl.ty == 0) ref: {
        // untyped (inferred) declaration
        break :ref ref;
    } else ref: {
        const dest_ty = try ty(&block_inline, scope, var_decl.ty);
        break :ref try coerce(&block_inline, scope, ref, dest_ty, node);
    };

    const yield_val = val: {
        if (refToIndex(val)) |index| {
            if (hg.instructions.items(.tag)[index] == .fn_decl) break :val val;
        }

        break :val indexToRef(try addDeclMut(&block_inline, val, node));
    };

    _ = try addYieldInline(&block_inline, yield_val, node);
    return addBlockInline(b, block_inline.instructions.items, node);
}

fn assignLocalPtr(b: *Block, scope: *Scope, node: Node.Index, id: Node.Index, val: Ref) !Hir.Index {
    const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;
    const local_ptr = var_scope.cast(Scope.LocalPtr).?;
    const addr = refToIndex(local_ptr.ptr).?;
    return addStore(b, addr, val, node);
}

fn assignNamespace(b: *Block, scope: *Scope, node: Node.Index, id: Node.Index, val: Ref) !Hir.Index {
    const hg = b.hg;
    const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;
    const namespace = var_scope.cast(Scope.Namespace).?;
    const decl = namespace.decls.get(id).?;

    if (hg.forward_map.get(decl)) |ref| {
        const addr = refToIndex(ref).?;
        return addStore(b, addr, val, node);
    } else {
        const addr = try addLoadInline(b, decl, node);
        return addStore(b, addr, val, node);
    }
}

fn assignSimple(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_simple;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index);
    const id = try hg.interner.intern(ident_str);
    const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;

    switch (var_scope.tag) {
        .local_val => return error.ConstAssign,
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

fn branchCondition(b: *Block, scope: *Scope, node: Node.Index) !Ref {
    const ref = try expr(b, scope, node);
    return coerce(b, scope, ref, Ref.bool_ty, node);
}

fn assignBinary(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_binary;

    const ident_index = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_index);
    const id = try hg.interner.intern(ident_str);
    const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;
    switch (var_scope.tag) {
        .local_val => return error.ConstAssign,
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

fn ifSimple(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_simple = b.hg.tree.data(node).if_simple;
    var block_scope = Block.init(b, scope);
    defer block_scope.deinit();
    const s = &block_scope.base;

    const condition = try branchCondition(b, scope, if_simple.condition);
    const exec = try block(&block_scope, s, if_simple.exec_true);
    return addBranchSingle(b, condition, exec, node);
}

fn ifElse(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_else = b.hg.tree.data(node).if_else;

    const condition = try branchCondition(b, scope, if_else.condition);
    const exec = b.hg.tree.extraData(if_else.exec, Node.IfElse);
    const exec_true = block: {
        var block_scope = Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, exec.exec_true);
    };
    const exec_false = block: {
        var block_scope = Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, exec.exec_false);
    };
    return addBranchDouble(b, condition, exec_true, exec_false, node);
}

fn ifChain(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_chain = b.hg.tree.data(node).if_chain;

    const condition = try branchCondition(b, scope, if_chain.condition);
    const chain = b.hg.tree.extraData(if_chain.chain, Node.IfChain);
    const exec_true = block: {
        var block_scope = Block.init(b, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, chain.exec_true);
    };
    const next = block: {
        var block_scope = Block.init(b, scope);
        defer block_scope.deinit();
        _ = switch (b.hg.tree.data(chain.next)) {
            .if_simple => try ifSimple(&block_scope, &block_scope.base, chain.next),
            .if_else => try ifElse(&block_scope, &block_scope.base, chain.next),
            else => unreachable,
        };

        // TODO: another regression that needs to be tested
        break :block try addBlock(&block_scope, block_scope.instructions.items, node);
    };

    return addBranchDouble(b, condition, exec_true, next, node);
}

fn returnStmt(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const return_val = b.hg.tree.data(node).return_val;

    const operand = if (return_val.val == 0) op: {
        break :op Ref.void_val;
    } else op: {
        const ref = try expr(b, scope, return_val.val);
        break :op try coerce(b, scope, ref, b.return_ty, node);
    };
    return addRetNode(b, operand, node);
}

fn loopForever(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_forever = b.hg.tree.data(node).loop_forever;
    var loop_scope = Scope.Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_forever.body);

    const condition = block: {
        var block_scope = Scope.Block.init(b, scope);
        defer block_scope.deinit();

        // TODO: should be implicit not node
        const condition = try coerce(b, scope, Ref.btrue_val, Ref.bool_ty, node);
        _ = try addYieldImplicit(&block_scope, condition, undefined); // TODO: specify token
        break :block try addBlock(&block_scope, block_scope.instructions.items, node);
    };

    return addLoop(b, condition, body, node);
}

fn loopConditional(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_conditional = b.hg.tree.data(node).loop_conditional;

    const condition = block: {
        var block_scope = Scope.Block.init(b, scope);
        defer block_scope.deinit();
        // TODO: coerce this to bool, waiting for test infra
        const condition = try expr(&block_scope, &block_scope.base, loop_conditional.condition);
        _ = try addYieldImplicit(&block_scope, condition, undefined);
        break :block try addBlock(&block_scope, block_scope.instructions.items, node);
    };

    var loop_scope = Scope.Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_conditional.body);

    return addLoop(b, condition, body, node);
}

fn loopRange(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
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
        var block_scope = Scope.Block.init(b, s);
        defer block_scope.deinit();
        const condition = try expr(&block_scope, &block_scope.base, signature.condition);
        _ = try addYieldImplicit(&block_scope, condition, undefined);
        break :block try addBlock(&block_scope, block_scope.instructions.items, node);
    };

    // we have a block (loop outer body) that contains the afterthought
    // and then an inner nested block that contains the user loop body
    const body = body: {
        var block_scope = Scope.Block.init(b, s);
        defer block_scope.deinit();
        {
            var body_scope = Scope.Block.init(&block_scope, &block_scope.base);
            defer body_scope.deinit();
            const index = try block(&body_scope, &body_scope.base, loop_range.body);
            try block_scope.instructions.append(hg.gpa, index);
        }

        if (try statement(&block_scope, &block_scope.base, signature.afterthought)) |_| {
            return error.AfterthoughtDecl;
        }

        break :body try addBlock(&block_scope, block_scope.instructions.items, node);
    };

    return addLoop(b, condition, body, node);
}

fn loopBreak(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    _ = scope;
    return addBreak(b, node);
}

fn loopContinue(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    _ = scope;
    return addContinue(b, node);
}

fn module(b: *Block, scope: *Scope, node: Node.Index) !void {
    const hg = b.hg;
    const data = b.hg.tree.data(node).module;
    var namespace = Scope.Namespace.init(scope);

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    // first pass through statements - "forward declare" all identifiers
    // in the toplevel namespace so that forward-referencing and recursive
    // function calls resolve
    // note that since we link to the node id, we can resolve identifiers
    // that haven't been irgen'ed yet
    var stmts = hg.tree.extra_data[data.stmts_start..data.stmts_end];
    for (stmts) |stmt| {
        const id = switch (hg.tree.data(stmt)) {
            .const_decl => id: {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 1);
                break :id try hg.interner.intern(ident);
            },
            .const_decl_attr => id: {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 1);
                break :id try hg.interner.intern(ident);
            },
            .var_decl => id: {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                break :id try hg.interner.intern(ident);
            },
            else => {
                std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        };

        try namespace.decls.put(hg.arena, id, stmt);
        try b.scratch.append(hg.arena, id);
    }

    // second pass - generate the value/expression and update the resolution map
    // so we can link node ids to instructions
    for (stmts) |stmt| {
        const inst = try global_statement(b, &namespace.base, stmt);
        const ref = indexToRef(inst);
        switch (hg.tree.data(stmt)) {
            .const_decl,
            .const_decl_attr,
            .var_decl => try hg.forward_map.put(hg.gpa, stmt, ref),
            else => {},
        }

        try b.scratch.append(hg.arena, inst);
    }

    const members = b.scratch.items[scratch_top..];
    _ = try addModule(b, members, node); // TODO: maybe just return this
}

fn instructionReturns(hg: *HirGen, inst: u32) bool {
    switch (hg.instructions.items(.tag)[inst]) {
        .block => {
            const data = hg.instructions.items(.data)[inst];
            const insts_len = hg.extra.items[data.pl_node.pl];
            if (insts_len == 0) return false;
            return instructionReturns(hg, hg.extra.items[data.pl_node.pl + insts_len]);
        },
        .branch_single => return false,
        .branch_double => {
            const data = hg.instructions.items(.data)[inst];
            const lret = instructionReturns(hg, hg.extra.items[data.pl_node.pl + 1]);
            const rret = instructionReturns(hg, hg.extra.items[data.pl_node.pl + 2]);
            return lret and rret;
        },
        .ret_implicit, .ret_node => return true,
        else => return false,
    }
}

fn blockReturns(b: *Block) bool {
    const inst = b.instructions.items[b.instructions.items.len - 1];
    return instructionReturns(b.hg, inst);
}
