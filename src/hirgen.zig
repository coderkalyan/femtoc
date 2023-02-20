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
const indexToRef = Inst.indexToRef;
const refToIndex = Inst.refToIndex;

pub const GenError = error {
    NotImplemented,
    UnexpectedToken,
    InvalidIdentifier,
    InvalidRef,
    IdentifierShadowed,
    ConstAssign,
};

const Error = GenError || @import("interner.zig").Error || Allocator.Error || @import("integerLiteral.zig").ParseError;

pub fn generate(gpa: Allocator, tree: *const Ast) !Hir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var hirgen = HirGen {
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .insts = std.MultiArrayList(Inst){},
        .extra_data = std.ArrayList(Hir.Index).init(gpa),
        .scratch = std.ArrayList(Hir.Index).init(arena.allocator()),
        .interner = Interner.init(gpa),
        .forward_map = .{},
    };

    const ref = try hirgen.module(@intCast(u32, tree.nodes.len - 1));
    _ = ref;

    return Hir {
        .insts = hirgen.insts.toOwnedSlice(),
        .extra_data = hirgen.extra_data.toOwnedSlice(),
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

pub const HirGen = struct {
    gpa: Allocator,
    arena: Allocator,

    tree: *const Ast,
    insts: std.MultiArrayList(Inst),
    extra_data: std.ArrayList(Hir.Index),
    scratch: std.ArrayList(Hir.Index),
    interner: Interner,
    forward_map: std.AutoHashMapUnmanaged(Node.Index, Hir.Ref),

    fn parseIntToken(hg: *HirGen, index: Ast.TokenIndex) !u64 {
        const int_str = hg.tree.tokenString(index);
        return parseInt(int_str);
    }

    fn parseFloatToken(hg: *HirGen, index: Ast.TokenIndex) !f64 {
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

    fn addInst(hg: *HirGen, inst: Inst) !Hir.Index {
        const result = @intCast(Hir.Index, hg.insts.len);
        try hg.insts.append(hg.gpa, inst);
        return result;
    }

    fn addExtra(hg: *HirGen, extra: anytype) !Hir.ExtraIndex {
        const fields = std.meta.fields(@TypeOf(extra));
        try hg.extra_data.ensureUnusedCapacity(fields.len);
        const len = @intCast(u32, hg.extra_data.items.len);
        inline for (fields) |field| {
            if (field.field_type == Hir.ExtraIndex) {
                hg.extra_data.appendAssumeCapacity(@field(extra, field.name));
            } else if (field.field_type == Hir.Ref) {
                hg.extra_data.appendAssumeCapacity(@enumToInt(@field(extra, field.name)));
            } else {
                unreachable;
            }
        }
        return len;
    }

    fn integerLiteral(hg: *HirGen, gh: *Scope.GenHir, node: Node.Index) !Ref {
        const int_token = hg.tree.mainToken(node);
        const int_str = hg.tree.tokenString(int_token);
        const value = try parseInt(int_str);

        return switch (value) {
            0 => .zero_val,
            1 => .one_val,
            else => {
                const index = try hg.addInst(.{
                    .tag = .int,
                    .data = .{ .int = value },
                });
                try gh.addInst(index);
                return indexToRef(index);
            },
        };
    }

    fn floatLiteral(hg: *HirGen, gh: *Scope.GenHir, node: Node.Index) !Ref {
        const float_token = hg.tree.mainToken(node);
        const float_str = hg.tree.tokenString(float_token);
        const value = try parseFloat(float_str);

        const index = try hg.addInst(.{
            .tag = .float,
            .data = .{ .float = value },
        });
        try gh.addInst(index);
        return indexToRef(index);
    }

    fn boolLiteral(hg: *HirGen, node: Node.Index) Ref {
        const bool_token = hg.tree.mainToken(node);
        return switch (hg.tree.tokenTag(bool_token)) {
            .k_true => .btrue_val,
            .k_false => .bfalse_val,
            else => unreachable,
        };
    }

    fn identifier(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Ref {
        const ident_token = hg.tree.mainToken(node);
        const ident_str = hg.tree.tokenString(ident_token);
        const id = try hg.interner.intern(ident_str);
        const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;

        // for constants, the ref points to the instruction that returned its value
        // so we don't have to do anything else
        // but for variables, the ref points to the address in memory so we have to
        // generate a load instruction to get the value at that address
        switch (var_scope.tag) {
            .local_val => {
                const local_val = var_scope.cast(Scope.LocalVal).?;
                return local_val.ref;
            },
            .local_ptr => {
                const local_ptr = var_scope.cast(Scope.LocalPtr).?;
                const index = try hg.addInst(.{
                    .tag = .load,
                    .data = .{ .un_node = .{ .node = node, .operand = local_ptr.ptr } },
                });
                try gh.addInst(index);
                return indexToRef(index);
            },
            .namespace => {
                const namespace = var_scope.cast(Scope.Namespace).?;
                const decl = namespace.decls.get(id).?;
                if (hg.forward_map.get(decl)) |ref| {
                    return ref;
                } else {
                    const index = try hg.addInst(.{
                        .tag = .load_inline,
                        .data = .{ .pl_node = .{ .node = node, .pl = decl } },
                    });
                    try gh.addInst(index);
                    return indexToRef(index);
                }
            },
            else => unreachable,
        }
    }

    // TODO: cleanup and finish
    fn ty(hg: *HirGen, scope: *Scope, index: Node.Index) !Hir.Ref {
        const nodes = hg.tree.nodes;
        const ident_index = nodes.items(.main_token)[index];
        const ident_str = hg.tree.tokenString(ident_index);

        _ = scope;
        return builtin_types.get(ident_str) orelse GenError.InvalidIdentifier;
        // const uniq = try hg.getStringIndex(ident_str);
        // return Scope.resolveVar(scope, uniq) orelse GenError.InvalidIdentifier;
    }

    fn call(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) Error!Hir.Index {
        const ref = try hg.identifier(gh, scope, node);
        const call_expr = hg.tree.data(node).call_expr;

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var extra_index = call_expr.args_start;
        while (extra_index < call_expr.args_end) : (extra_index += 1) {
            const arg = hg.tree.extra_data[extra_index];
            try hg.scratch.append(@enumToInt(try hg.expr(gh, scope, arg)));
        }

        const args = hg.scratch.items[scratch_top..];
        const pl = try hg.addExtra(Inst.Call {
            .addr = ref,
            .args_len = @intCast(u32, args.len),
        });
        try hg.extra_data.appendSlice(args);

        const index = try hg.addInst(.{ 
            .tag = .call,
            .data = .{ .pl_node = .{ .node = node, .pl = pl, } },
        });
        try gh.addInst(index);
        return index;
    }

    fn binary(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) Error!Hir.Index {
        const binary_expr = hg.tree.data(node).binary_expr;

        const lref = try hg.expr(gh, scope, binary_expr.left);
        const rref = try hg.expr(gh, scope, binary_expr.right);
        const bin = try hg.addExtra(Inst.Binary {
            .lref = lref,
            .rref = rref,
        });

        const index = try hg.addInst(.{
            .tag = switch (hg.tree.tokenTag(hg.tree.mainToken(node))) {
                .plus => .add,
                .minus => .sub,
                .asterisk => .mul,
                .slash => .div,
                .percent => .mod,
                .equal_equal => .eq,
                .bang_equal => .neq,
                .l_angle_equal => .leq,
                .r_angle_equal => .geq,
                .l_angle => .lt,
                .r_angle => .gt,
                else => return Error.UnexpectedToken,
            },
            .data = .{ .pl_node = .{ .node = node, .pl = bin, } },
        });
        try gh.addInst(index);
        return index;
    }

    fn fnDecl(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) Error!Hir.Index {
        const fn_decl = hg.tree.data(node).fn_decl;
        const signature = hg.tree.extraData(fn_decl.signature, Node.FnSignature);
        var arena = std.heap.ArenaAllocator.init(hg.arena);
        defer arena.deinit();
        var genhir = Scope.GenHir.init(arena.allocator(), scope);

        const scratch_top = hg.scratch.items.len;
        defer hg.scratch.shrinkRetainingCapacity(scratch_top);

        var s: *Scope = &genhir.base;
        var extra_index = signature.params_start;
        while (extra_index < signature.params_end) : (extra_index += 1) {
            const param = hg.tree.extra_data[extra_index];
            const data = hg.tree.data(param).param;
            const param_token = hg.tree.mainToken(param);
            const param_str = hg.tree.tokenString(param_token);
            const param_id = try hg.interner.intern(param_str);

            const param_extra = try hg.addExtra(Inst.Param {
                .name = param_id,
                .ty = try hg.ty(scope, data.ty),
            });
            const ref = Inst.indexToRef(param_extra);
            try hg.scratch.append(param_extra);

            var param_var = try hg.arena.create(Scope.LocalVal);
            param_var.* = Scope.LocalVal.init(s, param_id, ref);
            s = &param_var.base;
        }

        const params = hg.scratch.items[scratch_top..];
        const param_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(params);
        const param_top = hg.extra_data.items.len;

        const return_ty = try hg.ty(&genhir.base, signature.return_ty);
        var block_scope = Scope.Block.init(s, .function);
        const body = try hg.block(&genhir, &block_scope.base, fn_decl.body);

        const decl = try hg.addExtra(Inst.FnDecl {
            .params_start = @intCast(u32, param_base),
            .params_end = @intCast(u32, param_top),
            .return_ty = return_ty,
            .body = body,
        });
        const index = try hg.addInst(.{
            .tag = .fn_decl,
            .data = .{ .pl_node = .{ .node = node, .pl = decl, } },
        });
        try gh.addInst(index);
        return index;
    }

    fn expr(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Ref {
        return switch (hg.tree.data(node)) {
            .integer_literal => hg.integerLiteral(gh, node),
            .float_literal => hg.floatLiteral(gh, node),
            .bool_literal => hg.boolLiteral(node),
            .var_expr => hg.identifier(gh, scope, node),
            .call_expr => indexToRef(try hg.call(gh, scope, node)),
            .binary_expr => indexToRef(try hg.binary(gh, scope, node)),
            .fn_decl => indexToRef(try hg.fnDecl(gh, scope, node)),
            else => {
                std.debug.print("Unexpected node: {}\n", .{hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        };
    }

    fn block(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) Error!Hir.Index {
        const data = hg.tree.data(node).block;
        std.debug.assert(scope.tag == .block);

        const scratch_top = gh.scratch.items.len;
        defer gh.scratch.shrinkRetainingCapacity(scratch_top);

        var s: *Scope = scope;
        var extra_index = data.stmts_start;
        while (extra_index < data.stmts_end) : (extra_index += 1) {
            const stmt = hg.tree.extra_data[extra_index];
            switch (hg.tree.data(stmt)) {
                .const_decl => {
                    const ref = try hg.constDecl(gh, s, stmt);
                    const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 1);
                    const id = try hg.interner.intern(ident);
                    const var_scope = try hg.arena.create(Scope.LocalVal);
                    var_scope.* = Scope.LocalVal.init(s, id, ref);
                    s = &var_scope.base;
                },
                .var_decl => {
                    const ref = try hg.varDecl(gh, s, stmt);
                    const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                    const id = try hg.interner.intern(ident);
                    const var_scope = try hg.arena.create(Scope.LocalPtr);
                    var_scope.* = Scope.LocalPtr.init(s, id, ref);
                    s = &var_scope.base;
                },
                .assign_simple => {
                    const ref = try hg.assignSimple(gh, s, stmt);
                    try gh.addInst(ref);
                },
                .if_simple => {
                    const ref = try hg.ifSimple(gh, s, stmt);
                    try gh.addInst(ref);
                },
                .if_else => {
                    const ref = try hg.ifElse(gh, s, stmt);
                    try gh.addInst(ref);
                },
                .if_chain => {
                    const ref = try hg.ifChain(gh, s, stmt);
                    try gh.addInst(ref);
                },
                .return_val => {
                    const ref = try hg.returnStmt(gh, s, stmt);
                    try gh.addInst(ref);
                },
                else => {
                    std.debug.print("Unexpected node: {}\n", .{hg.tree.data(stmt)});
                    return GenError.NotImplemented;
                },
            }
        }

        // const ret_implicit = try hg.addInst(.{
        //     .tag = .ret_implicit,
        //     .data = .{ .un_node = .{ .node = node, .operand = Ref.void_val, } },
        // });
        // try gh.addInst(ret_implicit);

        const insts = gh.scratch.items[scratch_top..];
        // std.debug.print("{any}\n", .{insts});
        // std.debug.print("{any}\n", .{insts});
        // const insts = block_scope.insts.items;
        // std.debug.print("block insts: {any}\n", .{insts});
        const ref = try hg.addExtra(Inst.Block {
            .len = @intCast(u32, insts.len),
        });
        // const insts_base = hg.extra_data.items.len;
        try hg.extra_data.appendSlice(insts);

        // const ref = try hg.addExtra(Inst.Block {
        //     .insts_start = @intCast(u32, insts_base),
        //     .insts_end = @intCast(u32, hg.extra_data.items.len),
        // });
        return hg.addInst(.{
            .tag = .block,
            .data = .{ .pl_node = .{ .node = node, .pl = ref, } },
        });
    }

    fn constDecl(hg: *HirGen, gh: *Scope.GenHir, s: *Scope, node: Node.Index) !Ref {
        // "initializes" constant variables
        // this doesn't actually create any instructions for declaring the constant
        // instead, the value to set the constant to is computed, and the resulting
        // instruction return value is stored in the scope such that future
        // code that needs to access this constant can simply look up the identifier
        // and refer to the associated value instruction
        const const_decl = hg.tree.data(node).const_decl;
        if (const_decl.ty == 0) {
            // untyped (inferred) declaration
            return try hg.expr(gh, s, const_decl.val);
        } else {
            const ref = try hg.expr(gh, s, const_decl.val);
            const validate_data = try hg.addExtra(Inst.ValidateTy {
                .ref = ref,
                .ty = try hg.ty(s, const_decl.ty),
            });

            // generate a "type validation" marker instruction
            // this is a passthrough which takes in the above value reference
            // and the type reference and returns the value reference
            // semantic analysis will validate that the type is as it should be
            // and then remove this instruction in the mir
            const validate_ty = try hg.addInst(.{
                .tag = .validate_ty,
                .data = .{ .pl_node = .{ .node = node, .pl = validate_data, } },
            });
            try gh.addInst(validate_ty);

            return indexToRef(validate_ty);
        }
    }

    fn varDecl(hg: *HirGen, gh: *Scope.GenHir, s: *Scope, node: Node.Index) !Ref {
        // "initializes" mutable variables
        // unlike constant declarations, mutable variables are stored in "memory"
        // so we have to create alloc instructions in addition to computing the value
        // otherwise, this function operates like constDecl
        // this doesn't actually create any instructions for declaring the constant
        // instead, the value to set the constant to is computed, and the resulting
        // instruction return value is stored in the scope such that future
        // code that needs to access this constant can simply look up the identifier
        // and refer to the associated value instruction
        const var_decl = hg.tree.data(node).var_decl;
        if (var_decl.ty == 0) {
            // untyped (inferred) declaration
            const ref = try hg.expr(gh, s, var_decl.val);
            const alloc = try hg.addInst(.{
                .tag = .alloc,
                .data = .{ .un_node = .{ .node = node, .operand = ref, } },
            });
            try gh.addInst(alloc);
            return indexToRef(alloc);
        } else {
            // type annotated declaration
            const ref = try hg.expr(gh, s, var_decl.val);

            // generate a "type validation" marker instruction
            // this is a passthrough which takes in the above value reference
            // and the type reference and returns the value reference
            // semantic analysis will validate that the type is as it should be
            // and then remove this instruction in the mir
            const validate_data = try hg.addExtra(Inst.ValidateTy {
                .ref = ref,
                .ty = try hg.ty(s, var_decl.ty),
            });
            const validate_ty = try hg.addInst(.{
                .tag = .validate_ty,
                .data = .{ .pl_node = .{ .node = node, .pl = validate_data, } },
            });
            try gh.addInst(validate_ty);

            const alloc = try hg.addInst(.{
                .tag = .alloc,
                .data = .{ .un_node = .{ .node = node, .operand = indexToRef(validate_ty), } },
            });
            try gh.addInst(alloc);
            return indexToRef(alloc);
        }
    }

    fn assignSimple(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Hir.Index {
        const assign = hg.tree.data(node).assign_simple;

        const ident_index = hg.tree.mainToken(node);
        const ident_str = hg.tree.tokenString(ident_index);
        const id = try hg.interner.intern(ident_str);
        const var_scope = try scope.resolveVar(id) orelse return error.InvalidIdentifier;

        switch (var_scope.tag) {
            .local_val => return error.ConstAssign,
            .local_ptr => {
                const local_ptr = var_scope.cast(Scope.LocalPtr).?;
                const valref = try hg.expr(gh, scope, assign.val);
                const store = try hg.addExtra(Inst.Store {
                    .addr = refToIndex(local_ptr.ptr).?,
                    .val = valref,
                });
                return hg.addInst(.{
                    .tag = .store,
                    .data = .{ .pl_node = .{ .node = node, .pl = store } },
                });
            },
            .namespace => {
                const namespace = var_scope.cast(Scope.Namespace).?;
                const decl = namespace.decls.get(id).?;
                const valref = try hg.expr(gh, scope, assign.val);

                if (hg.forward_map.get(decl)) |ref| {
                    const store = try hg.addExtra(Inst.Store {
                        .addr = refToIndex(ref).?,
                        .val = valref,
                    });
                    return hg.addInst(.{
                        .tag = .store,
                        .data = .{ .pl_node = .{ .node = node, .pl = store } },
                    });
                } else {
                    const index = try hg.addInst(.{
                        .tag = .load_inline,
                        .data = .{ .pl_node = .{ .node = node, .pl = decl } },
                    });
                    const store = try hg.addExtra(Inst.Store {
                        .addr = index,
                        .val = valref,
                    });
                    return hg.addInst(.{
                        .tag = .store,
                        .data = .{ .pl_node = .{ .node = node, .pl = store } },
                    });
                }
            },
            else => unreachable,
        }
        // const ref = hg.resolution_map.get(try scope.resolveVar(id)).?;
    }

    fn ifSimple(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Hir.Index {
        const if_simple = hg.tree.data(node).if_simple;

        const condition_ref = try hg.expr(gh, scope, if_simple.condition);
        const exec_ref = try hg.block(gh, scope, if_simple.exec_true);
        const branch = try hg.addExtra(Inst.BranchSingle {
            .condition = condition_ref,
            .exec_true = exec_ref,
        });

        return hg.addInst(.{
            .tag = .branch_single,
            .data = .{ .pl_node = .{ .node = node, .pl = branch, } },
        });
    }

    fn ifElse(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Hir.Index {
        const if_else = hg.tree.data(node).if_else;

        const condition_ref = try hg.expr(gh, scope, if_else.condition);
        // if (Inst.refToIndex(condition_ref)) |index| try gh.addInst(index);

        const exec = hg.tree.extraData(if_else.exec, Node.IfElse);
        const exec_true = try hg.block(gh, scope, exec.exec_true);
        const exec_false = try hg.block(gh, scope, exec.exec_false);
        const branch = try hg.addExtra(Inst.BranchDouble {
            .condition = condition_ref,
            .exec_true = exec_true,
            .exec_false = exec_false,
        });

        return hg.addInst(.{
            .tag = .branch_double,
            .data = .{ .pl_node = .{ .node = node, .pl = branch, } },
        });
    }

    fn ifChain(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Hir.Index {
        const if_chain = hg.tree.data(node).if_chain;

        const condition_ref = try hg.expr(gh, scope, if_chain.condition);
        // if (Inst.refToIndex(condition_ref)) |index| try gh.addInst(index);

        const chain = hg.tree.extraData(if_chain.chain, Node.IfChain);
        const exec_true = try hg.block(gh, scope, chain.exec_true);
        const next = switch (hg.tree.data(chain.next)) {
            .if_simple => try hg.ifSimple(gh, scope, chain.next),
            .if_else => try hg.ifElse(gh, scope, chain.next),
            else => unreachable,
        };

        const branch = try hg.addExtra(Inst.BranchDouble {
            .condition = condition_ref,
            .exec_true = exec_true,
            .exec_false = next,
        });
        return hg.addInst(.{
            .tag = .branch_double,
            .data = .{ .pl_node = .{ .node = node, .pl = branch} },
        });
    }

    fn returnStmt(hg: *HirGen, gh: *Scope.GenHir, scope: *Scope, node: Node.Index) !Hir.Index {
        const return_val = hg.tree.data(node).return_val;

        const ref = if (return_val.val == 0) Ref.void_val else try hg.expr(gh, scope, return_val.val);
        return hg.addInst(.{
            .tag = .ret_node,
            .data = .{ .un_node = .{ .node = node, .operand = ref, } },
        });
    }

    fn module(hg: *HirGen, node: Node.Index) !Hir.Index {
        const data = hg.tree.data(node).module;
        var toplevel_scope = Scope.Module {};
        var arena = std.heap.ArenaAllocator.init(hg.arena);
        defer arena.deinit();
        var genhir = Scope.GenHir.init(arena.allocator(), &toplevel_scope.base);
        var block_scope = Scope.Block.init(&genhir.base, .toplevel);
        var namespace = Scope.Namespace.init(arena.allocator(), &block_scope.base);

        const scratch_top = genhir.scratch.items.len;
        defer genhir.scratch.shrinkRetainingCapacity(scratch_top);
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
                    try namespace.decls.put(id, stmt);
                },
                .var_decl => {
                    const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                    const id = try hg.interner.intern(ident);
                    try namespace.decls.put(id, stmt);
                },
                else => {
                    std.debug.print("Unexpected node: {}\n", .{hg.tree.data(stmt)});
                    return Error.NotImplemented;
                },
            }
        }

        // second pass - generate the value/expression and update the resolution map
        // so we can link node ids to instructions
        extra_index = data.stmts_start;
        while (extra_index < data.stmts_end) : (extra_index += 1) {
            var stmt = hg.tree.extra_data[extra_index];
            switch (hg.tree.data(stmt)) {
                .const_decl => {
                    const ref = try hg.constDecl(&genhir, &namespace.base, stmt);
                    try hg.forward_map.put(hg.gpa, stmt, ref);
                },
                .var_decl => {
                    const ref = try hg.varDecl(&genhir, &namespace.base, stmt);
                    try hg.forward_map.put(hg.gpa, stmt, ref);
                },
                else => {
                    std.debug.print("Unexpected node: {}\n", .{hg.tree.data(stmt)});
                    return Error.NotImplemented;
                },
            }
        }

        const insts = genhir.scratch.items[scratch_top..];
        const ref = try hg.addExtra(Inst.Module {
            .len = @intCast(u32, insts.len),
        });
        try hg.extra_data.appendSlice(insts);

        return hg.addInst(.{
            .tag = .toplevel,
            .data = .{ .pl_node = .{ .node = node, .pl = ref, } },
        });
    }
};
