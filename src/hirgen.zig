const std = @import("std");
const hir = @import ("hir.zig");
const ast = @import("ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseInt = @import("integerLiteral.zig").parseInt;
const parseFloat = @import("floatLiteral.zig").parseFloat;
const Scope = @import("scope.zig").Scope;
const Interner = @import("interner.zig").Interner;

const Allocator = std.mem.Allocator;
const Hir = hir.Hir;
const Inst = hir.Inst;
const Ast = ast.Ast;
const Node = ast.Node;
const Token = lex.Token;

pub const GenError = error { UnexpectedToken, InvalidIdentifier, InvalidRef };
const Error = GenError || @import("scope.zig").IdentifierError || @import("interner.zig").Error || Allocator.Error;

pub fn generate(gpa: Allocator, tree: *const Ast) !Hir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var hirgen = HirGen {
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .inst = std.ArrayList(Inst).init(gpa),
        .extra_data = std.ArrayList(Inst.Extra).init(gpa),
        .scratch = std.ArrayList(Inst.Extra).init(arena.allocator()),
        .interner = Interner.init(gpa),
    };

    const ref = try hirgen.toplevel(@intCast(u32, tree.nodes.len - 1));
    _ = ref;

    return Hir {
        .inst = hirgen.inst.toOwnedSlice(),
        .extra_data = hirgen.extra_data.toOwnedSlice(),
    };
}

const builtin_types = std.ComptimeStringMap(Inst.Ref, .{
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
});

pub const HirGen = struct {
    gpa: Allocator,
    arena: Allocator,

    tree: *const Ast,
    inst: std.ArrayList(Inst),
    extra_data: std.ArrayList(Inst.Extra),
    scratch: std.ArrayList(Inst.Extra),
    interner: Interner,

    fn parseIntToken(hg: *HirGen, index: ast.TokenIndex) !u64 {
        const int_str = hg.tree.tokenString(index);
        return parseInt(int_str);
    }

    fn parseFloatToken(hg: *HirGen, index: ast.TokenIndex) !f64 {
        const float_str = hg.tree.tokenString(index);
        return parseFloat(float_str);
    }

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

    fn addInst(hg: *HirGen, inst: Inst) !Inst.Ref {
        const result = @intCast(Inst.Index, hg.inst.items.len);
        std.debug.print("{}\n", .{inst});
        try hg.inst.append(inst);
        return Inst.indexToRef(result);
    }

    fn addExtra(hg: *HirGen, extra: anytype) Allocator.Error!Inst.ExtraIndex {
        const fields = std.meta.fields(@TypeOf(extra));
        try hg.extra_data.ensureUnusedCapacity(fields.len);
        const len = @intCast(u32, hg.extra_data.items.len);
        inline for (fields) |field| {
            if (field.field_type == Inst.ExtraIndex) {
                hg.extra_data.appendAssumeCapacity(.{ .index = @field(extra, field.name) });
            } else if (field.field_type == Inst.Ref) {
                hg.extra_data.appendAssumeCapacity(.{ .ref = @field(extra, field.name) });
            } else {
                unreachable;
            }
        }
        return len;
    }

    fn integerLiteral(hg: *HirGen, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const int_token = nodes.items(.main_token)[index];
        const value = hg.parseIntToken(int_token) catch return GenError.UnexpectedToken;
        return switch (value) {
            0 => .izero_val,
            1 => .ione_val,
            else => try hg.addInst(.{
                .tag = .int,
                .data = .{ .int = value },
            }),
        };
    }

    fn floatLiteral(hg: *HirGen, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const float_token = nodes.items(.main_token)[index];
        const value = hg.parseFloatToken(float_token) catch return GenError.UnexpectedToken;
        return hg.addInst(.{ 
            .tag = .float,
            .data = .{ .float = value },
        });
    }

    fn boolLiteral(hg: *HirGen, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const bool_token = nodes.items(.main_token)[index];

        return switch (hg.tree.tokens.items(.tag)[bool_token]) {
            .k_true => Inst.Ref.btrue_val,
            .k_false => Inst.Ref.bfalse_val,
            else => unreachable,
        };
    }

    fn identifier(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index];
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);
        const ref = scope.resolveVar(id);

        return ref;
    }

    fn ty(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index];
        const ident_str = hg.tree.tokenString(ident_index);

        _ = scope;
        return builtin_types.get(ident_str) orelse GenError.InvalidIdentifier;
        // const uniq = try hg.getStringIndex(ident_str);
        // return Scope.resolveVar(scope, uniq) orelse GenError.InvalidIdentifier;
    }

    fn call(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const ref = try hg.identifier(scope, index);
        const call_expr = hg.tree.nodes.items(.data)[index].call_expr;

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var arg_index = call_expr.args_start;
        while (arg_index < call_expr.args_end) : (arg_index += 1) {
            try hg.scratch.append(.{ .ref = try hg.expr(scope, arg_index) });
        }

        const args = hg.scratch.items[scratch_top..];
        const extra_top = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(args);

        const pl = try hg.addExtra(Inst.Call {
            .addr = ref,
            .args_start = @intCast(u32, extra_top),
            .args_end = @intCast(u32, hg.extra_data.items.len),
        });

        return hg.addInst(.{ 
            .tag = .call,
            .data = .{ .pl_node = .{ .node = index, .pl = pl, } },
        });
    }

    fn binary(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const main_token = nodes.items(.main_token)[index];
        const binary_expr = nodes.items(.data)[index].binary_expr;

        const lref = try hg.expr(scope, binary_expr.left);
        const rref = try hg.expr(scope, binary_expr.right);
        const bin = try hg.addExtra(Inst.Binary {
            .lref = lref,
            .rref = rref,
        });

        return hg.addInst(.{
            .tag = switch (hg.tree.tokens.items(.tag)[main_token]) {
                .plus => .add,
                .minus => .sub,
                .asterisk => .mul,
                .slash => .div,
                .percent => .mod,
                .equal_equal => .eq,
                .bang_equal => .neq,
                else => return Error.UnexpectedToken,
            },
            .data = .{ .pl_node = .{ .node = index, .pl = bin, } },
        });
    }

    fn fnDecl(hg: *HirGen, scope: *Scope, index: Node.Index) Error!Inst.Ref {
        const nodes = hg.tree.nodes;

        const fn_decl = nodes.items(.data)[index].fn_decl;
        const signature = hg.tree.extraData(fn_decl.signature, Node.FnSignature);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var s: *Scope = scope;
        var param_index = signature.params_start;
        while (param_index < signature.params_end) : (param_index += 1) {
            const param = nodes.items(.data)[hg.tree.extra_data[param_index]].param;
            const param_token = nodes.items(.main_token)[param_index];
            const param_str = hg.tree.tokenString(param_token);
            const param_id = try hg.interner.intern(param_str);

            const param_extra = try hg.addExtra(Inst.Param {
                .name = param_id,
                .ty = try hg.ty(scope, param.ty),
            });
            const param_ref = Inst.indexToRef(param_extra);
            try hg.scratch.append(.{ .ref = param_ref });

            var param_var = try hg.arena.create(Scope.LocalVar);
            param_var.* = Scope.LocalVar.init(s, param_id, param_ref);
            s = &param_var.base;
        }

        const params = hg.scratch.items[scratch_top..];
        const param_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(params);
        const param_top = hg.extra_data.items.len;

        const body = try hg.block(s, fn_decl.body);

        const decl = try hg.addExtra(Inst.FnDecl {
            .params_start = @intCast(u32, param_base),
            .params_end = @intCast(u32, param_top),
            .body = body,
        });
        return hg.addInst(.{
            .tag = .fn_decl,
            .data = .{ .pl_node = .{ .node = index, .pl = decl, } },
        });
    }

    fn expr(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        return switch (hg.tree.nodes.items(.data)[index]) {
            .integer_literal => hg.integerLiteral(index),
            .float_literal => hg.floatLiteral(index),
            .bool_literal => hg.boolLiteral(index),
            .var_expr => hg.identifier(scope, index),
            .call_expr => hg.call(scope, index),
            .binary_expr => hg.binary(scope, index),
            .fn_decl => hg.fnDecl(scope, index),
            else => GenError.UnexpectedToken,
        };
    }

    fn block(hg: *HirGen, scope: *Scope, index: Node.Index) Error!Inst.Ref {
        const block_data = hg.tree.nodes.items(.data)[index].block;
        var block_scope = Scope.Block.init(scope);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var s: *Scope = &block_scope.base;
        var stmt_index = block_data.stmts_start;
        while (stmt_index < block_data.stmts_end) : (stmt_index += 1) {
            var stmt_node = hg.tree.extra_data[stmt_index];
            switch (hg.tree.nodes.items(.data)[stmt_node]) {
                .const_decl => {
                    const ref = try hg.constDecl(s, stmt_node);

                    const ident_index = hg.tree.mainToken(stmt_node) + 1;
                    const ident_str = hg.tree.tokenString(ident_index);
                    const id = try hg.interner.intern(ident_str);
                    const var_scope = try hg.arena.create(Scope.LocalVar);
                    var_scope.* = Scope.LocalVar.init(s, id, ref);

                    s = &var_scope.base;
                },
                .var_decl => {
                    const ref = try hg.varDecl(s, stmt_node);

                    const ident_index = hg.tree.mainToken(stmt_node) + 2;
                    const ident_str = hg.tree.tokenString(ident_index);
                    const id = try hg.interner.intern(ident_str);
                    const var_scope = try hg.arena.create(Scope.LocalVar);
                    var_scope.* = Scope.LocalVar.init(s, id, ref);

                    s = &var_scope.base;
                },
                .assign_simple => {
                    const ref = try hg.assignSimple(s, stmt_node);
                    try hg.scratch.append(.{ .ref = ref });
                },
                .if_simple => {
                    const ref = try hg.ifSimple(s, stmt_node);
                    try hg.scratch.append(.{ .ref = ref });
                },
                .return_val => {
                    const ref = try hg.returnStmt(s, stmt_node);
                    try hg.scratch.append(.{ .ref = ref });
                },
                else => {
                    std.debug.print("{}\n", .{hg.tree.nodes.items(.data)[stmt_node]});
                    unreachable;
                },
            }
        }

        const insts = hg.scratch.items[scratch_top..];
        const insts_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(insts);

        const ref = try hg.addExtra(Inst.Block {
            .insts_start = @intCast(u32, insts_base),
            .insts_end = @intCast(u32, hg.extra_data.items.len),
        });
        return hg.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = index, .pl = ref, } },
        });
    }

    fn constDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        // "initializes" constant variables
        // this doesn't actually create any instructions for declaring the constant
        // instead, the value to set the constant to is computed, and the resulting
        // instruction return value is stored in the scope such that future
        // code that needs to access this constant can simply look up the identifier
        // and refer to the associated value instruction
        const nodes = hg.tree.nodes;

        const const_decl = nodes.items(.data)[index].const_decl;
        if (const_decl.ty == 0) {
            // untyped (inferred) declaration
            return hg.expr(scope, const_decl.val);
        } else {
            const ref = try hg.expr(scope, const_decl.val);
            const validate_ty = try hg.addExtra(Inst.ValidateTy {
                .ref = ref,
                .ty = try hg.ty(scope, const_decl.ty),
            });

            // generate a "type validation" marker instruction
            // this is a passthrough which takes in the above value reference
            // and the type reference and returns the value reference
            // semantic analysis will validate that the type is as it should be
            // and then remove this instruction in the mir
            return hg.addInst(.{
                .tag = .validate_ty,
                .data = .{ .pl_node = .{ .node = index, .pl = validate_ty, } },
            });
        }
    }

    fn varDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        // "initializes" mutable variables
        // unlike constant declarations, mutable variables are stored in "memory"
        // so we have to create alloc instructions in addition to computing the value
        // otherwise, this function operates like constDecl
        // this doesn't actually create any instructions for declaring the constant
        // instead, the value to set the constant to is computed, and the resulting
        // instruction return value is stored in the scope such that future
        // code that needs to access this constant can simply look up the identifier
        // and refer to the associated value instruction
        const nodes = hg.tree.nodes;

        const const_decl = nodes.items(.data)[index].var_decl;
        const valref = if (const_decl.ty == 0) ref: {
            // untyped (inferred) declaration
            break :ref try hg.expr(scope, const_decl.val);
        } else ref: {
            const ref = try hg.expr(scope, const_decl.val);
            const validate_ty = try hg.addExtra(Inst.ValidateTy {
                .ref = ref,
                .ty = try hg.ty(scope, const_decl.ty),
            });

            // generate a "type validation" marker instruction
            // this is a passthrough which takes in the above value reference
            // and the type reference and returns the value reference
            // semantic analysis will validate that the type is as it should be
            // and then remove this instruction in the mir
            break :ref try hg.addInst(.{
                .tag = .validate_ty,
                .data = .{ .pl_node = .{ .node = index, .pl = validate_ty, } },
            });
        };

        return hg.addInst(.{
            .tag = .alloc,
            .data = .{ .un_node = .{ .node = index, .operand = valref, } },
        });
    }

    fn assignSimple(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const assign = nodes.items(.data)[index].assign_simple;

        const ident_index = nodes.items(.main_token)[index];
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);
        const ref = try scope.resolveVar(id);
        const valref = try hg.expr(scope, assign.val);

        const store = try hg.addExtra(Inst.Store {
            .addr = ref,
            .val = valref,
        });
        return hg.addInst(.{
            .tag = .store,
            .data = .{ .pl_node = .{ .node = index, .pl = store } },
        });
    }

    fn ifSimple(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const if_simple = nodes.items(.data)[index].if_simple;

        const condition_ref = try hg.expr(scope, if_simple.condition);
        const exec_ref = try hg.block(scope, if_simple.exec_true);
        const branch = try hg.addExtra(Inst.BranchSingle {
            .condition = condition_ref,
            .exec_true = exec_ref,
        });

        return hg.addInst(.{
            .tag = .branch_single,
            .data = .{ .pl_node = .{ .node = index, .pl = branch, } },
        });
    }

    fn returnStmt(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const return_val = nodes.items(.data)[index].return_val;

        const expr_ref = if (return_val.val == 0) ref: {
            break :ref Inst.Ref.void_val;
        } else ref: {
            break :ref try hg.expr(scope, return_val.val);
        };
        return hg.addInst(.{
            .tag = .ret_node,
            .data = .{ .un_node = .{ .node = index, .operand = expr_ref, } },
        });
    }

    fn toplevel(hg: *HirGen, index: Node.Index) Error!Inst.Ref {
        const toplevel_data = hg.tree.nodes.items(.data)[index].toplevel;
        var toplevel_scope = Scope.Toplevel {};
        var namespace = Scope.Namespace.init(hg.arena, &toplevel_scope.base);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var stmt_index = toplevel_data.stmts_start;
        while (stmt_index < toplevel_data.stmts_end) : (stmt_index += 1) {
            var stmt_node = hg.tree.extra_data[stmt_index];
            switch (hg.tree.nodes.items(.data)[stmt_node]) {
                .const_decl => {
                    const ref = try hg.constDecl(&namespace.base, stmt_node);

                    const ident_index = hg.tree.nodes.items(.main_token)[index] + 1;
                    const ident_str = hg.tree.tokenString(ident_index);
                    const id = try hg.interner.intern(ident_str);
                    try namespace.decls.put(id, ref);
                },
                else => {
                    std.debug.print("{}\n", .{hg.tree.nodes.items(.data)[stmt_node]});
                    return Error.UnexpectedToken;
                },
            }
        }

        const insts = hg.scratch.items[scratch_top..];
        const insts_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(insts);

        const ref = try hg.addExtra(Inst.Toplevel {
            .insts_start = @intCast(u32, insts_base),
            .insts_end = @intCast(u32, hg.extra_data.items.len),
        });
        return hg.addInst(.{
            .tag = .toplevel,
            .data = .{ .pl_node = .{ .node = index, .pl = ref, } },
        });
    }
};
