const std = @import("std");
const Hir = @import ("Hir.zig");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseInt = @import("integerLiteral.zig").parseInt;
const parseFloat = @import("floatLiteral.zig").parseFloat;
const Scope = @import("scope.zig").Scope;
const Interner = @import("interner.zig").Interner;

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

const Error = GenError || @import("interner.zig").Error || Allocator.Error || @import("integerLiteral.zig").ParseError;

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

fn parseNumberToken(hg: *HirGen, index: Token.Index) !union { int: u64, float: f64 } {
    const source = hg.tree.source;
    const start = hg.tree.tokens.items(.start)[@intCast(u32, index)];
    var lexer = lex.Lexer {
        .buffer = source,
        .index = start,
        .pending_invalid_token = null,
    };
    const token = lexer.next();
    return switch (token.tag) {
        .int_lit => parseInt(source[start..token.loc.end]),
        .float_lit => parseFloat(source[start..token.loc.end]),
        else => GenError.UnexpectedToken,
    };
}

fn addExtra(hg: *HirGen, extra: anytype) !Hir.ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try hg.extra.ensureUnusedCapacity(hg.gpa, fields.len);
    const len = @intCast(u32, hg.extra.items.len);
    inline for (fields) |field| {
        if (field.field_type == Hir.ExtraIndex) {
            hg.extra.appendAssumeCapacity(@field(extra, field.name));
        } else if (field.field_type == Hir.Ref) {
            hg.extra.appendAssumeCapacity(@enumToInt(@field(extra, field.name)));
        } else {
            unreachable;
        }
    }
    return len;
}

fn integerLiteral(b: *Block, node: Node.Index) !Ref {
    const hg = b.hg;
    const int_token = hg.tree.mainToken(node);
    const int_str = hg.tree.tokenString(int_token);
    const value = try parseInt(int_str);

    return switch (value) {
        0 => .zero_val,
        1 => .one_val,
        else => {
            const index = try b.addInst(.{
                .tag = .int,
                .data = .{ .int = value },
            });
            return indexToRef(index);
        },
    };
}

fn floatLiteral(b: *Block, node: Node.Index) !Ref {
    const hg = b.hg;
    const float_token = hg.tree.mainToken(node);
    const float_str = hg.tree.tokenString(float_token);
    const value = try parseFloat(float_str);

    const index = try b.addInst(.{
        .tag = .float,
        .data = .{ .float = value },
    });
    return indexToRef(index);
}

fn boolLiteral(b: *Block, node: Node.Index) Ref {
    const hg = b.hg;
    const bool_token = hg.tree.mainToken(node);
    return switch (hg.tree.tokenTag(bool_token)) {
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
            const index = try b.addInst(.{
                .tag = .load,
                .data = .{ .un_node = .{ .node = node, .operand = local_ptr.ptr } },
            });
            return indexToRef(index);
        },
        .namespace => {
            const namespace = var_scope.cast(Scope.Namespace).?;
            const decl = namespace.decls.get(id).?;
            // have we already generated the instruction for this identifier?
            if (hg.forward_map.get(decl)) |ref| {
                return ref;
            } else {
                // nope, so just create a forward declaration
                const index = try b.addInst(.{
                    .tag = .load_inline,
                    .data = .{ .pl_node = .{ .node = node, .pl = decl } },
                });
                return indexToRef(index);
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
                    const index = try b.addInst(.{
                        .tag = .load_inline,
                        .data = .{ .pl_node = .{ .node = node, .pl = decl } },
                    });
                    return indexToRef(index);
                }
            },
            else => unreachable,
        }
    }
}

fn call(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const arena = hg.arena;
    const addr_ref = try variable(b, scope, node);
    const call_expr = hg.tree.data(node).call_expr;

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    var extra_index = call_expr.args_start;
    try b.scratch.ensureUnusedCapacity(arena, call_expr.args_end - call_expr.args_start);
    while (extra_index < call_expr.args_end) : (extra_index += 1) {
        const arg = hg.tree.extra_data[extra_index];
        const arg_ref = try expr(b, scope, arg);
        b.scratch.appendAssumeCapacity(@enumToInt(arg_ref));
    }

    const args = b.scratch.items[scratch_top..];
    const pl = try hg.addExtra(Inst.Call {
        .addr = addr_ref,
        .args_len = @intCast(u32, args.len),
    });
    try hg.extra.appendSlice(hg.gpa, args);

    const index = try b.addInst(.{ 
        .tag = .call,
        .data = .{ .pl_node = .{ .node = node, .pl = pl, } },
    });
    return index;
}

fn binary(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const binary_expr = hg.tree.data(node).binary_expr;

    const lref = try expr(b, scope, binary_expr.left);
    const rref = try expr(b, scope, binary_expr.right);
    const bin = try hg.addExtra(Inst.Binary {
        .lref = lref,
        .rref = rref,
    });

    const operator_token = hg.tree.mainToken(node);
    const index = try b.addInst(.{
        .tag = switch (hg.tree.tokenTag(operator_token)) {
            .plus => .add,
            .minus => .sub,
            .asterisk => .mul,
            .slash => .div,
            .percent => .mod,
            .equal_equal => .cmp_eq,
            .bang_equal => .cmp_ne,
            .l_angle_equal => .cmp_le,
            .r_angle_equal => .cmp_ge,
            .l_angle => .cmp_lt,
            .r_angle => .cmp_gt,
            else => return Error.UnexpectedToken,
        },
        .data = .{ .pl_node = .{ .node = node, .pl = bin, } },
    });
    return index;
}

fn fnDecl(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const arena = hg.arena;
    const fn_decl = hg.tree.data(node).fn_decl;
    const signature = hg.tree.extraData(fn_decl.signature, Node.FnSignature);

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    var s: *Scope = scope;
    var extra_index = signature.params_start;
    try b.scratch.ensureUnusedCapacity(arena, signature.params_end - signature.params_start);
    while (extra_index < signature.params_end) : (extra_index += 1) {
        const param = hg.tree.extra_data[extra_index];
        const data = hg.tree.data(param).param;
        const param_token = hg.tree.mainToken(param);
        const param_str = hg.tree.tokenString(param_token);
        const param_id = try hg.interner.intern(param_str);

        const ty_ref = try ty(b, scope, data.ty);
        const param_data = try hg.addExtra(Hir.Inst.Param {
            .name = param_id,
            .ty = ty_ref,
        });
        const param_inst = @intCast(u32, hg.instructions.len);
        try hg.instructions.append(hg.gpa, .{
            .tag = .param,
            .data = .{ .pl_node = .{ .node = param, .pl = param_data } },
        });
        b.scratch.appendAssumeCapacity(param_inst);

        const ref = Inst.indexToRef(param_inst);
        var param_scope = try hg.arena.create(Scope.LocalVal);
        param_scope.* = Scope.LocalVal.init(s, param_id, ref);
        s = &param_scope.base;
    }

    const params = b.scratch.items[scratch_top..];
    const param_base = hg.extra.items.len;
    try hg.extra.appendSlice(hg.gpa, params);
    const param_top = hg.extra.items.len;

    var block_scope = Block.init(b, s);
    defer block_scope.deinit();
    s = &block_scope.base;
    var body_scope = Scope.Body { .parent = s };
    s = &body_scope.base;

    const return_ty = try ty(&block_scope, s, signature.return_ty);
    const body = try block(&block_scope, s, fn_decl.body);

    // unwind and free scope objects
    while (s != scope) {
        const parent = s.parent().?;
        hg.arena.destroy(s);
        s = parent;
    }

    const decl = try hg.addExtra(Inst.FnDecl {
        .params_start = @intCast(u32, param_base),
        .params_end = @intCast(u32, param_top),
        .return_ty = return_ty,
        .body = body,
    });
    const index = try b.addInst(.{
        .tag = .fn_decl,
        .data = .{ .pl_node = .{ .node = node, .pl = decl, } },
    });
    return index;
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

fn statement(b: *Block, scope: *Scope, node: Node.Index) Error!?Ref {
    const data = b.hg.tree.data(node);
    _ = try switch (data) {
        .const_decl => return try constDecl(b, scope, node),
        .var_decl => return try varDecl(b, scope, node),
        .assign_simple => assignSimple(b, scope, node),
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

fn block(b: *Block, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const data = hg.tree.data(node).block;

    var s: *Scope = scope;
    var extra_index = data.stmts_start;
    try b.scratch.ensureUnusedCapacity(hg.arena, data.stmts_end - data.stmts_start);
    while (extra_index < data.stmts_end) : (extra_index += 1) {
        const stmt = hg.tree.extra_data[extra_index];
        const ref = try statement(b, s, stmt);
        switch (hg.tree.data(stmt)) {
            .const_decl => {
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
        _ = try b.addInst(.{
            .tag = .ret_implicit,
            .data = .{ .un_tok = .{ .tok = 0, .operand = Ref.void_val, } },
        });
    }

    const ref = try hg.addExtra(Inst.Block {
        .len = @intCast(u32, b.instructions.items.len),
    });
    try hg.extra.appendSlice(hg.gpa, b.instructions.items);

    return b.addInst(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .node = node, .pl = ref, } },
    });
}

fn constDecl(b: *Block, s: *Scope, node: Node.Index) !Ref {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope such that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const const_decl = b.hg.tree.data(node).const_decl;
    if (const_decl.ty == 0) {
        // untyped (inferred) declaration
        return expr(b, s, const_decl.val);
    } else {
        const ref = try expr(b, s, const_decl.val);
        const validate_data = try b.hg.addExtra(Inst.ValidateTy {
            .ref = ref,
            .ty = try ty(b, s, const_decl.ty),
        });

        // generate a "type validation" marker instruction
        // this is a passthrough which takes in the above value reference
        // and the type reference and returns the value reference
        // semantic analysis will validate that the type is as it should be
        // and then remove this instruction in the mir
        const validate_ty = try b.addInst(.{
            .tag = .validate_ty,
            .data = .{ .pl_node = .{ .node = node, .pl = validate_data, } },
        });

        return indexToRef(validate_ty);
    }
}

fn varDecl(b: *Block, s: *Scope, node: Node.Index) !Ref {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.hg.tree.data(node).var_decl;
    if (var_decl.ty == 0) {
        // untyped (inferred) declaration
        const ref = try expr(b, s, var_decl.val);
        const alloc = try b.addInst(.{
            .tag = .alloc,
            .data = .{ .un_node = .{ .node = node, .operand = ref } },
        });
        return indexToRef(alloc);
    } else {
        // type annotated declaration
        const ref = try expr(b, s, var_decl.val);

        // generate a "type validation" marker instruction
        // this is a passthrough which takes in the above value reference
        // and the type reference and returns the value reference
        // semantic analysis will validate that the type is as it should be
        // and then remove this instruction in the mir
        const validate_data = try b.hg.addExtra(Inst.ValidateTy {
            .ref = ref,
            .ty = try ty(b, s, var_decl.ty),
        });
        const validate_ty = try b.addInst(.{
            .tag = .validate_ty,
            .data = .{ .pl_node = .{ .node = node, .pl = validate_data, } },
        });

        const alloc = try b.addInst(.{
            .tag = .alloc,
            .data = .{ .un_node = .{ .node = node, .operand = indexToRef(validate_ty), } },
        });
        return indexToRef(alloc);
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
            const local_ptr = var_scope.cast(Scope.LocalPtr).?;
            const valref = try expr(b, scope, assign.val);
            const store = try hg.addExtra(Inst.Store {
                .addr = refToIndex(local_ptr.ptr).?,
                .val = valref,
            });
            return b.addInst(.{
                .tag = .store,
                .data = .{ .pl_node = .{ .node = node, .pl = store } },
            });
        },
        .namespace => {
            const namespace = var_scope.cast(Scope.Namespace).?;
            const decl = namespace.decls.get(id).?;
            const valref = try expr(b, scope, assign.val);

            if (hg.forward_map.get(decl)) |ref| {
                const store = try hg.addExtra(Inst.Store {
                    .addr = refToIndex(ref).?,
                    .val = valref,
                });
                return b.addInst(.{
                    .tag = .store,
                    .data = .{ .pl_node = .{ .node = node, .pl = store } },
                });
            } else {
                const index = try b.addInst(.{
                    .tag = .load_inline,
                    .data = .{ .pl_node = .{ .node = node, .pl = decl } },
                });
                const store = try hg.addExtra(Inst.Store {
                    .addr = index,
                    .val = valref,
                });
                return b.addInst(.{
                    .tag = .store,
                    .data = .{ .pl_node = .{ .node = node, .pl = store } },
                });
            }
        },
        else => unreachable,
    }
}

fn ifSimple(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_simple = b.hg.tree.data(node).if_simple;
    var block_scope = Block.init(b, scope);
    defer block_scope.deinit();
    const s = &block_scope.base;

    const condition_ref = try expr(b, s, if_simple.condition);
    const exec_ref = try block(&block_scope, s, if_simple.exec_true);
    const branch = try b.hg.addExtra(Inst.BranchSingle {
        .condition = condition_ref,
        .exec_true = exec_ref,
    });

    return b.addInst(.{
        .tag = .branch_single,
        .data = .{ .pl_node = .{ .node = node, .pl = branch, } },
    });
}

fn ifElse(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_else = b.hg.tree.data(node).if_else;

    const condition_ref = try expr(b, scope, if_else.condition);
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
    const branch = try b.hg.addExtra(Inst.BranchDouble {
        .condition = condition_ref,
        .exec_true = exec_true,
        .exec_false = exec_false,
    });

    return b.addInst(.{
        .tag = .branch_double,
        .data = .{ .pl_node = .{ .node = node, .pl = branch, } },
    });
}

fn ifChain(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_chain = b.hg.tree.data(node).if_chain;

    const condition_ref = try expr(b, scope, if_chain.condition);
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

        const ref = try b.hg.addExtra(Inst.Block {
            .len = @intCast(u32, block_scope.instructions.items.len),
        });
        try b.hg.extra.appendSlice(b.hg.gpa, block_scope.instructions.items);

        break :block try block_scope.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = ref, } },
        });
    };

    const branch = try b.hg.addExtra(Inst.BranchDouble {
        .condition = condition_ref,
        .exec_true = exec_true,
        .exec_false = next,
    });
    return b.addInst(.{
        .tag = .branch_double,
        .data = .{ .pl_node = .{ .node = node, .pl = branch} },
    });
}

fn returnStmt(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const return_val = b.hg.tree.data(node).return_val;

    const ref = if (return_val.val == 0) Ref.void_val else try expr(b, scope, return_val.val);
    return b.addInst(.{
        .tag = .ret_node,
        .data = .{ .un_node = .{ .node = node, .operand = ref, } },
    });
}

fn loopForever(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_forever = b.hg.tree.data(node).loop_forever;
    var loop_scope = Scope.Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_forever.body);

    const condition = block: {
        var block_scope = Scope.Block.init(b, scope);
        defer block_scope.deinit();
        _ = try block_scope.addInst(.{
            .tag = .yield_implicit,
            .data = .{ .un_tok = .{ .tok = undefined, .operand = Ref.btrue_val } },
        });

        const data = try block_scope.hg.addExtra(Inst.Block {
            .len = @intCast(u32, block_scope.instructions.items.len),
        });
        try block_scope.hg.extra.appendSlice(block_scope.hg.gpa, block_scope.instructions.items);
        break :block try block_scope.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = data } },
        });
    };

    const data = try b.hg.addExtra(Inst.Loop {
        .condition = condition,
        .body = body,
    });
    return b.addInst(.{
        .tag = .loop,
        .data = .{ .pl_node = .{ .node = node, .pl = data } },
    });
}

fn loopConditional(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    const loop_conditional = b.hg.tree.data(node).loop_conditional;

    const condition = block: {
        var block_scope = Scope.Block.init(b, scope);
        defer block_scope.deinit();
        const condition = try expr(&block_scope, &block_scope.base, loop_conditional.condition);
        _ = try block_scope.addInst(.{
            .tag = .yield_implicit,
            .data = .{ .un_tok = .{ .tok = undefined, .operand = condition } },
        });

        const data = try block_scope.hg.addExtra(Inst.Block {
            .len = @intCast(u32, block_scope.instructions.items.len),
        });
        try block_scope.hg.extra.appendSlice(block_scope.hg.gpa, block_scope.instructions.items);
        break :block try block_scope.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = data } },
        });
    };

    var loop_scope = Scope.Block.init(b, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_conditional.body);

    const data = try b.hg.addExtra(Inst.Loop {
        .condition = condition,
        .body = body,
    });
    return b.addInst(.{
        .tag = .loop,
        .data = .{ .pl_node = .{ .node = node, .pl = data } },
    });
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
        _ = try block_scope.addInst(.{
            .tag = .yield_implicit,
            .data = .{ .un_tok = .{ .tok = undefined, .operand = condition } },
        });

        const data = try block_scope.hg.addExtra(Inst.Block {
            .len = @intCast(u32, block_scope.instructions.items.len),
        });
        try block_scope.hg.extra.appendSlice(block_scope.hg.gpa, block_scope.instructions.items);
        break :block try block_scope.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = data } },
        });
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

        const data = try block_scope.hg.addExtra(Inst.Block {
            .len = @intCast(u32, block_scope.instructions.items.len),
        });
        try block_scope.hg.extra.appendSlice(block_scope.hg.gpa, block_scope.instructions.items);
        break :body try block_scope.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = data } },
        });
    };

    const data = try b.hg.addExtra(Inst.Loop {
        .condition = condition,
        .body = body,
    });
    return b.addInst(.{
        .tag = .loop,
        .data = .{ .pl_node = .{ .node = node, .pl = data } },
    });
}

fn loopBreak(b: *Block, scope: *Scope, node: Node.Index) !Hir.Index {
    _ = scope;
    return b.addInst(.{
        .tag = .loop_break,
        .data = .{ .node = node },
    });
}

fn module(b: *Block, scope: *Scope, node: Node.Index) !void {
    const hg = b.hg;
    const data = b.hg.tree.data(node).module;
    var namespace = Scope.Namespace.init(scope);

    // first pass through statements - "forward declare" all identifiers
    // in the toplevel namespace so that forward-referencing and recursive
    // function calls resolve
    // note that since we link to the node id, we can resolve identifiers
    // that haven't been irgen'ed yet
    var extra_index = data.stmts_start;
    while (extra_index < data.stmts_end) : (extra_index += 1) {
        var stmt = hg.tree.extra_data[extra_index];
        switch (hg.tree.data(stmt)) {
            .const_decl => {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 1);
                const id = try hg.interner.intern(ident);
                try namespace.decls.put(hg.arena, id, stmt);
            },
            .var_decl => {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                const id = try hg.interner.intern(ident);
                try namespace.decls.put(hg.arena, id, stmt);
            },
            else => {
                std.debug.print("Unexpected node: {}\n", .{b.hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        }
    }

    // second pass - generate the value/expression and update the resolution map
    // so we can link node ids to instructions
    extra_index = data.stmts_start;
    while (extra_index < data.stmts_end) : (extra_index += 1) {
        var stmt = hg.tree.extra_data[extra_index];
        var ref = try statement(b, &namespace.base, stmt);
        switch (hg.tree.data(stmt)) {
            .const_decl => try hg.forward_map.put(hg.gpa, stmt, ref.?),
            .var_decl => try hg.forward_map.put(hg.gpa, stmt, ref.?),
            else => {},
        }
    }

    const ref = try hg.addExtra(Inst.Module {
        .len = @intCast(u32, b.instructions.items.len),
    });
    try hg.extra.appendSlice(hg.gpa, b.instructions.items);

    _ = try b.addInst(.{
        .tag = .module,
        .data = .{ .pl_node = .{ .node = node, .pl = ref } },
    });
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
