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
        .extra_data = std.ArrayList(Inst.Ref).init(gpa),
        .scratch = std.ArrayList(Inst.Ref).init(arena.allocator()),
        .interner = Interner.init(gpa),
    };

    // const ref = try hirgen.toplevel(@intCast(u32, tree.nodes.len - 1));
    var toplevel = Scope.Toplevel {};
    var namespace = Scope.Namespace.init(arena.allocator(), &toplevel.base);
    const ref = try hirgen.globalConstDecl(&namespace.base, @intCast(u32, tree.nodes.len - 1));
    _ = ref;

    return Hir {
        .inst = hirgen.inst.toOwnedSlice(),
        .extra_data = hirgen.extra_data.toOwnedSlice(),
    };
}

const builtin_types = std.ComptimeStringMap(Inst.Ref, .{
    .{ "u8", .u8 },
    .{ "u16", .u16 },
    .{ "u32", .u32 },
    .{ "u64", .u64 },
    .{ "i8", .i8 },
    .{ "i16", .i16 },
    .{ "i32", .i32 },
    .{ "i64", .i64 },
    .{ "f32", .f32 },
    .{ "f64", .f64 },
    .{ "bool", .bool },
});

pub const HirGen = struct {
    gpa: Allocator,
    arena: Allocator,

    tree: *const Ast,
    inst: std.ArrayList(Inst),
    extra_data: std.ArrayList(Inst.Ref),
    scratch: std.ArrayList(Inst.Ref),
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

    fn addExtra(hg: *HirGen, extra: anytype) Allocator.Error!Inst.Ref {
        const fields = std.meta.fields(@TypeOf(extra));
        try hg.extra_data.ensureUnusedCapacity(fields.len);
        const len = @intCast(u32, hg.extra_data.items.len);
        inline for (fields) |field| {
            if (field.field_type == Inst.ExtraIndex) {
                hg.extra_data.appendAssumeCapacity(Inst.indexToRef(@field(extra, field.name)));
            } else if (field.field_type == Inst.Ref) {
                hg.extra_data.appendAssumeCapacity(@field(extra, field.name));
            } else {
                unreachable;
            }
        }
        return Inst.indexToRef(len);
    }

    fn integerLiteral(hg: *HirGen, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const int_token = nodes.items(.main_token)[index];
        const value = hg.parseIntToken(int_token) catch return GenError.UnexpectedToken;
        return switch (value) {
            0 => .zero,
            1 => .one,
            else => try hg.addInst(.{ .int = value }),
        };
    }

    fn floatLiteral(hg: *HirGen, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const float_token = nodes.items(.main_token)[index];
        const value = hg.parseFloatToken(float_token) catch return GenError.UnexpectedToken;
        return hg.addInst(.{ .float = value });
    }

    fn identifier(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index];
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);
        const ref = scope.resolveVar(id);

        // std.debug.print("{s}\n", .{ident_str});
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
            try hg.scratch.append(try hg.expr(scope, arg_index));
        }

        const args = hg.scratch.items[scratch_top..];
        const extra_top = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(args);

        return hg.addInst(.{ .call = try hg.addExtra(Inst.Call {
            .addr = ref,
            .args_start = extra_top,
            .args_end = hg.extra_data.items.len,
        })});
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

        return hg.addInst(switch (hg.tree.tokens.items(.tag)[main_token]) {
            .plus => .{ .add = bin },
            .minus => .{ .sub = bin },
            .asterisk => .{ .mul = bin },
            .slash => .{ .div = bin },
            .equal_equal => .{ .eq = bin },
            .bang_equal => .{ .neq = bin },
            else => unreachable,
        });
    }

    fn fnDecl(hg: *HirGen, scope: *Scope, index: Node.Index) Error!Inst.Ref {
        const nodes = hg.tree.nodes;

        const fn_decl = nodes.items(.data)[index].fn_decl;
        const proto = nodes.items(.data)[fn_decl.proto].fn_proto;
        const params_range = hg.tree.extraData(proto.params, Node.FnProto);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var s: *Scope = scope;
        var param_index = params_range.params_start;
        while (param_index < params_range.params_end) : (param_index += 1) {
            const param = nodes.items(.data)[hg.tree.extra_data[param_index]].param;
            const param_token = nodes.items(.main_token)[param_index] - 2;
            const param_str = hg.tree.tokenString(param_token);
            const param_id = try hg.interner.intern(param_str);

            const param_ref = try hg.addExtra(Inst.Param {
                .name = param_id,
                .ty = try hg.ty(scope, param.ty),
            });
            try hg.scratch.append(param_ref);

            var param_var = try hg.arena.alloc(Scope.LocalVar, 1);
            param_var[0] = Scope.LocalVar.init(s, param_id, param_ref);
            s = &param_var[0].base;
        }

        const params = hg.scratch.items[scratch_top..];
        const param_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(params);
        const param_top = hg.extra_data.items.len;

        const body = try hg.block(s, fn_decl.body);

        return hg.addInst(.{ .fn_decl = try hg.addExtra(Inst.FnDecl {
            .params_start = param_base,
            .params_end = param_top,
            .body = body,
        })});
    }

    fn expr(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        return switch (hg.tree.nodes.items(.data)[index]) {
            .integer_literal => hg.integerLiteral(index),
            .float_literal => hg.floatLiteral(index),
            .var_expr => hg.identifier(scope, index),
            .call_expr => hg.call(scope, index),
            .binary_expr => hg.binary(scope, index),
            .fn_decl => hg.fnDecl(scope, index),
            else => unreachable,
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
                    var var_scope = try hg.arena.alloc(Scope.LocalVar, 1);
                    var_scope[0] = try hg.constDecl(s, stmt_node);
                    s = &var_scope[0].base;
                },
                .if_stmt => {
                    const ref = try hg.ifStmt(s, stmt_node);
                    try hg.scratch.append(ref);
                },
                .return_void => {
                    const ref = try hg.returnVoid(s, stmt_node);
                    try hg.scratch.append(ref);
                },
                .return_val => {
                    const ref = try hg.returnVal(s, stmt_node);
                    try hg.scratch.append(ref);
                },
                else => {
                    unreachable;
                },
            }
        }

        const insts = hg.scratch.items[scratch_top..];
        const insts_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(insts);

        return hg.addInst(.{ .block = try hg.addExtra(Inst.Block {
            .insts_start = insts_base,
            .insts_end = hg.extra_data.items.len,
        })});
    }

    fn constDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !Scope.LocalVar {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index] + 1;
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);

        const const_decl = hg.tree.nodes.items(.data)[index].const_decl;
        // const ty = if (const_decl.ty == 0) hg.ty(scope, const_decl.ty);
        const ref = try hg.expr(scope, const_decl.val);

        var var_scope = Scope.LocalVar.init(scope, id, ref);
        return var_scope;
    }

    fn globalConstDecl(hg: *HirGen, scope: *Scope, index: Node.Index) !void {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index] + 1;
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);

        const const_decl = hg.tree.nodes.items(.data)[index].const_decl;
        const ref = try hg.expr(scope, const_decl.val);

        const namespace = scope.cast(Scope.Namespace).?;
        try namespace.decls.put(id, ref);
    }

    fn ifStmt(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const if_stmt = nodes.items(.data)[index].if_stmt;

        const condition_ref = try hg.expr(scope, if_stmt.condition);
        const body_ref = try hg.block(scope, if_stmt.body);

        return hg.addInst(.{
            .if_stmt = try hg.addExtra(Inst.If {
                .condition = condition_ref,
                .body = body_ref,
            }),
        });
    }

    fn returnVoid(hg: *HirGen, _: *Scope, _: Node.Index) !Inst.Ref {
        return hg.addInst(.{
            .return_void = {},
        });
    }

    fn returnVal(hg: *HirGen, scope: *Scope, index: Node.Index) !Inst.Ref {
        const nodes = hg.tree.nodes;
        const return_val = nodes.items(.data)[index].return_val;

        const expr_ref = try hg.expr(scope, return_val.val);
        return hg.addInst(.{
            .return_val = expr_ref,
        });
    }
};
