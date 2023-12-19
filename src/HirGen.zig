const std = @import("std");
const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const lex = @import("lex.zig");
const parse = @import("parse.zig");
const parseFloat = @import("floatLiteral.zig").parseFloat;
const Scope = @import("scope.zig").Scope;
const interner = @import("interner.zig");
const Interner = interner.Interner;
const error_handler = @import("error_handler.zig");
const Value = @import("value.zig").Value;
const Type = @import("hir/type.zig").Type;
const BlockEditor = @import("hir/BlockEditor.zig");
const InternPool = @import("InternPool.zig");

const implicit_return = @import("hir/implicit_return.zig");
const type_analysis = @import("hir/type_analysis.zig");
const dead_code_elimination = @import("hir/dead_code_elimination.zig");
const stack_analysis = @import("hir/stack_analysis.zig");

const Allocator = std.mem.Allocator;
const Inst = Hir.Inst;
const Node = Ast.Node;
const Token = lex.Token;
const Block = Scope.Block;
const HirGen = @This();

pub const GenError = error{
    NotImplemented,
    UnexpectedToken,
    InvalidIdentifier,
    InvalidRef,
    IdentifierShadowed,
    ConstAssign,
    AfterthoughtDecl,
    InvalidCharacter, // TODO: this doesn't belong here or even need to exist
    HandledUserError,
    InvalidLvalue,
};

const Error = GenError || interner.Error || Allocator.Error;

gpa: Allocator,
arena: Allocator,
tree: *const Ast,
insts: std.MultiArrayList(Inst),
extra: std.ArrayListUnmanaged(u32),
block_slices: std.ArrayListUnmanaged([]Hir.Index),
pool: InternPool,
types: std.ArrayListUnmanaged(Type),
untyped_decls: std.AutoHashMapUnmanaged(u32, Hir.Index),
interner: Interner,
errors: std.ArrayListUnmanaged(error_handler.SourceError),
instmap: std.ArrayListUnmanaged(Hir.Index),
module_index: Hir.Index,

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
        .pool = InternPool.init(gpa), // TODO: actually use an arena
        .types = .{},
        .untyped_decls = .{},
        .interner = Interner.init(gpa),
        .errors = .{},
        .instmap = .{},
        .module_index = undefined,
    };

    // post order format guarantees that the module node will be the last
    const module_node: u32 = @intCast(tree.nodes.len - 1);
    hirgen.module_index = try module(&hirgen, module_node);
    try implicit_return.executePass(&hirgen, hirgen.module_index);
    try type_analysis.executePass(&hirgen, hirgen.module_index); // catch {};
    try stack_analysis.executePass(&hirgen, hirgen.module_index);
    // try dead_code_elimination.executePass(&hirgen, module_index);element

    return Hir{
        .tree = tree,
        .insts = hirgen.insts.toOwnedSlice(),
        .module_index = hirgen.module_index,
        .block_slices = try hirgen.block_slices.toOwnedSlice(gpa),
        .extra_data = try hirgen.extra.toOwnedSlice(gpa),
        .interner = hirgen.interner,
        .types = try hirgen.types.toOwnedSlice(gpa),
        .pool = try hirgen.pool.ptr(),
        .untyped_decls = try hirgen.untyped_decls.clone(gpa),
        .errors = try hirgen.errors.toOwnedSlice(gpa),
        .instmap = try hirgen.instmap.toOwnedSlice(gpa),
    };
}

const builtin_types = std.ComptimeStringMap(Type, .{
    .{ "u8", Type.Common.u8_type },
    .{ "u16", Type.Common.u16_type },
    .{ "u32", Type.Common.u32_type },
    .{ "u64", Type.Common.u64_type },
    .{ "i8", Type.Common.i8_type },
    .{ "i16", Type.Common.i16_type },
    .{ "i32", Type.Common.i32_type },
    .{ "i64", Type.Common.i64_type },
    .{ "f32", Type.Common.f32_type },
    .{ "f64", Type.Common.f64_type },
    .{ "bool", Type.Common.u1_type },
    .{ "void", Type.Common.void_type },
});

const ResultInfo = struct {
    semantics: Semantics,

    const Semantics = enum {
        val,
        ref,
        ty,
    };
};

pub fn addExtra(hg: *HirGen, extra: anytype) !Hir.ExtraIndex {
    const fields = std.meta.fields(@TypeOf(extra));
    try hg.extra.ensureUnusedCapacity(hg.gpa, fields.len);
    const len: u32 = @intCast(hg.extra.items.len);
    inline for (fields) |field| {
        switch (field.type) {
            u32 => hg.extra.appendAssumeCapacity(@field(extra, field.name)),
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
            else => unreachable,
        };
    }
    return result;
}

pub fn get(hg: *HirGen, index: Hir.Index, comptime tag: Inst.Tag) Hir.InstData(tag) {
    const data = hg.insts.items(.data)[index];
    const active_field = comptime Hir.activeDataField(tag);
    switch (active_field) {
        .placeholder => return {},
        .int,
        .float,
        .ty,
        .node,
        .token,
        => {
            // construct an anonymous struct with just a single
            // field, with the same name as this tag
            var result: Hir.InstData(tag) = undefined;
            const field_name = @tagName(active_field);
            @field(result, field_name) = @field(data, field_name);
            return result;
        },
        .un_node,
        .un_tok,
        => {
            var result: Hir.InstData(tag) = undefined;
            const data_type = std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
            const source_data = @field(data, @tagName(active_field));
            const fields = std.meta.fields(data_type);
            inline for (fields) |field| {
                @field(result, field.name) = @field(source_data, field.name);
            }
            return result;
        },
        .pl_node => {
            var result: Hir.InstData(tag) = undefined;
            result.pl = data.pl_node.pl;
            result.node = data.pl_node.node;
            const payload_type = Hir.payloadType(tag);
            const payload = hg.extraData(data.pl_node.pl, payload_type);
            const fields = std.meta.fields(payload_type);
            inline for (fields) |field| {
                @field(result, field.name) = @field(payload, field.name);
            }
            return result;
        },
        .pl_tok => unreachable,
    }
}

fn ErrorSetType(comptime cb: type) type {
    const cb_type = @typeInfo(cb).Fn;
    const error_set = @typeInfo(cb_type.return_type.?).ErrorUnion.error_set;
    return error_set;
}
// runs a callback on any nested blocks of the instruction, such as the body
// of a branch or loop
// no-op on any instruction that doesn't contain a nested body
pub fn explore(hg: *HirGen, inst: Hir.Index, cb: anytype, args: anytype) ErrorSetType(@TypeOf(cb))!void {
    switch (hg.insts.items(.tag)[inst]) {
        .branch_single => {
            const data = hg.get(inst, .branch_single);
            try @call(.auto, cb, args ++ .{data.exec_true});
        },
        .branch_double => {
            const data = hg.get(inst, .branch_double);
            try @call(.auto, cb, args ++ .{data.exec_true});
            try @call(.auto, cb, args ++ .{data.exec_false});
        },
        .loop => {
            const data = hg.get(inst, .loop);
            try @call(.auto, cb, args ++ .{data.condition});
            try @call(.auto, cb, args ++ .{data.body});
        },
        .fn_decl => {
            const data = hg.get(inst, .fn_decl);
            try @call(.auto, cb, args ++ .{data.body});
        },
        .constant => {
            const data = hg.get(inst, .constant);
            const ty = try hg.resolveType(data.ty);
            if (ty.kind() == .function) {
                const val = hg.pool.getValue(data.val);
                const payload = val.function;
                // const payload = val.extended.cast(Value.Function).?;
                try @call(.auto, cb, args ++ .{payload.body});
            }
        },
        .block, .block_inline => try @call(.auto, cb, args ++ .{inst}),
        else => {},
    }
}

pub inline fn addInstUnlinked(hg: *HirGen, inst: Inst) !Hir.Index {
    const array_index: Hir.Index = @intCast(hg.insts.len);

    try hg.insts.ensureUnusedCapacity(hg.gpa, 1);
    try hg.instmap.ensureUnusedCapacity(hg.gpa, 1);

    hg.insts.appendAssumeCapacity(inst);
    hg.instmap.appendAssumeCapacity(array_index);

    return array_index;
}

pub inline fn updateInst(hg: *HirGen, index: Hir.Index, new: Hir.Index) void {
    hg.instmap.items[index] = new;
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

fn integerLiteral(b: *BlockEditor, node: Node.Index) !Hir.Index {
    const int = b.hg.tree.data(node).integer_literal;
    return try b.add(.int, .{ .int = int });
}

fn floatLiteral(b: *BlockEditor, node: Node.Index) !Hir.Index {
    const float_token = b.hg.tree.mainToken(node);
    const float_str = b.hg.tree.tokenString(float_token);
    const float = try parseFloat(float_str);
    return try b.add(.float, .{ .float = float });
}

fn boolLiteral(b: *BlockEditor, node: Node.Index) !Hir.Index {
    const bool_token = b.hg.tree.mainToken(node);

    return switch (b.hg.tree.tokenTag(bool_token)) {
        .k_true => try b.add(.int, .{ .int = 1 }),
        .k_false => try b.add(.int, .{ .int = 0 }),
        else => unreachable,
    };
}

fn charLiteral(b: *BlockEditor, node: Node.Index) !Hir.Index {
    const char_token = b.hg.tree.mainToken(node);
    const char_str = b.hg.tree.tokenString(char_token);

    return try b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = Type.Common.u8_type }),
        .val = try b.addIntValue(Type.Common.u8_type, char_str[1]),
        .node = node,
    });
}

fn stringLiteral(b: *BlockEditor, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const string_token = hg.tree.mainToken(node);
    const string_text = hg.tree.tokenString(string_token);
    const string_literal = string_text[1 .. string_text.len - 1];
    const id = try hg.interner.intern(string_literal);

    const inner = try hg.gpa.create(Type.Slice);
    inner.* = .{ .element = Type.Common.u8_type };
    const string_type = Type{ .extended = &inner.base };
    const string_value = try hg.pool.internValue(.{ .string = id });

    // underlying string, gets emitted to .rodata as a global
    _ = try b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = string_type }),
        .val = string_value,
        .node = node,
    });

    const len_value = try hg.pool.internValue(.{ .integer = string_literal.len });
    const slice = try Value.createSlice(hg.pool.arena, string_value, len_value);
    const slice_value = try hg.pool.internValue(slice);

    return b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = string_type }),
        .val = slice_value,
        .node = node,
    });

    // const inner_slice = try hg.gpa.create(Value.Slice);
    // inner_slice.* = .{ .ptr = string, .len = string_literal.len };
    // const slice = Value{ .extended = &inner_slice.base };
    // // comptime slice to that string
    // return try b.add(.constant, .{
    //     .ty = try b.add(.ty, .{ .ty = string_type }),
    //     .val = try b.addValue(slice),
    //     .node = node,
    // });
}

fn identExpr(b: *BlockEditor, scope: *Scope, ri: ResultInfo, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const ident_token = hg.tree.mainToken(node);
    const ident_str = hg.tree.tokenString(ident_token);

    // check identifier against builtin types, and if match found,
    // make sure we're using type semantics
    if (builtin_types.get(ident_str)) |builtin_type| {
        switch (ri.semantics) {
            .ty => return try b.addType(builtin_type),
            else => {
                try b.hg.errors.append(b.hg.gpa, .{
                    .tag = .invalid_identifer,
                    .token = ident_token,
                });
                return error.HandledUserError;
            },
        }
    }

    const id = try hg.interner.intern(ident_str);
    const ident_scope = (try scope.resolveIdent(id)) orelse {
        try b.hg.errors.append(b.hg.gpa, .{
            .tag = .invalid_identifer,
            .token = ident_token,
        });
        return error.HandledUserError;
    };

    switch (ident_scope.tag) {
        // for constants, the ref points to the instruction that returned its value
        // so we need to check that this is an rvalue context
        // we can't use consts in an lvalue context, aka on the left side of an assignment
        .local_val => {
            switch (ri.semantics) {
                .val => {
                    const local_val = ident_scope.cast(Scope.LocalVal).?;
                    return local_val.ref;
                },
                .ref => {
                    // TODO: this could be a nicer constant assignment error
                    try b.hg.errors.append(b.hg.gpa, .{
                        .tag = .invalid_lvalue,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
                .ty => {
                    try b.hg.errors.append(b.hg.gpa, .{
                        .tag = .invalid_type,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        // but for mutable variables, the ref points to the address in memory so we have to
        // generate a load instruction to get the value at that address
        .local_ptr => {
            const local_ptr = ident_scope.cast(Scope.LocalPtr).?;
            switch (ri.semantics) {
                .val => {
                    const ptr = local_ptr.ptr;
                    return try b.add(.load, .{ .operand = ptr, .node = node });
                },
                .ref => return local_ptr.ptr,
                .ty => {
                    try b.hg.errors.append(b.hg.gpa, .{
                        .tag = .invalid_type,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        .local_type => {
            switch (ri.semantics) {
                .ty => {
                    const local_type = ident_scope.cast(Scope.LocalType).?;
                    return local_type.ref;
                },
                .val, .ref => {
                    try b.hg.errors.append(b.hg.gpa, .{
                        .tag = .invalid_lvalue,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        .namespace => {
            // TODO: semantics for this
            return try b.add(.load_global, .{ .operand = id, .node = node });
        },
        else => unreachable,
    }
}

fn binaryInner(b: *BlockEditor, node: Node.Index, op: Ast.TokenIndex, lref: Hir.Index, rref: Hir.Index) Error!Hir.Index {
    const tag: Inst.Tag = switch (b.hg.tree.tokenTag(op)) {
        .plus, .plus_equal => .add,
        .minus, .minus_equal => .sub,
        .asterisk, .asterisk_equal => .mul,
        .slash, .slash_equal => .div,
        .percent, .percent_equal => .mod,
        .pipe, .pipe_equal => .bitwise_or,
        .ampersand, .ampersand_equal => .bitwise_and,
        .caret, .caret_equal => .bitwise_xor,
        .equal_equal => .cmp_eq,
        .bang_equal => .cmp_ne,
        .r_angle => .cmp_gt,
        .l_angle => .cmp_lt,
        .r_angle_equal => .cmp_ge,
        .l_angle_equal => .cmp_le,
        .r_angle_r_angle, .r_angle_r_angle_equal => .sr,
        .l_angle_l_angle, .l_angle_l_angle_equal => .sl,
        else => return Error.UnexpectedToken,
    };

    switch (tag) {
        inline .add,
        .sub,
        .mul,
        .div,
        .mod,
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
        .cmp_eq,
        .cmp_ne,
        .cmp_gt,
        .cmp_lt,
        .cmp_ge,
        .cmp_le,
        => |t| return b.add(t, .{ .lref = lref, .rref = rref, .node = node }),
        else => unreachable,
    }
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
    const operator_token = hg.tree.mainToken(node);
    const tag = hg.tree.tokenTag(operator_token);
    switch (tag) {
        .k_or,
        .k_and,
        .k_xor,
        .k_implies,
        => return binaryShort(b, scope, node),
        else => {
            const binary_expr = hg.tree.data(node).binary;
            const left = try valExpr(b, scope, binary_expr.left);
            const right = try valExpr(b, scope, binary_expr.right);
            return binaryInner(b, node, operator_token, left, right);
        },
    }
}

fn binaryShort(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const operator_token = hg.tree.mainToken(node);
    const binary_expr = hg.tree.data(node).binary;

    const bool_type = try b.addType(Type.Common.u1_type);
    const left_inner = try valExpr(b, scope, binary_expr.left);
    const left = try coerce(b, scope, left_inner, bool_type, node);

    _ = operator_token;
    _ = left;
    unreachable;
    // switch (operator_token) {
    //     .k_or => {
    //         // if left, yield true, else return and yield false
    //     },
    // }
    // const tag: Inst.Tag = switch (operator_token) {
    //     .k_or => .
    // }
}

fn unary(b: *BlockEditor, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const unary_expr = hg.tree.data(node).unary;
    const operator_token = hg.tree.mainToken(node);

    switch (hg.tree.tokenTag(operator_token)) {
        // calculate a reference to the pointer and return it
        .ampersand => return refExpr(b, scope, unary_expr),
        .asterisk => {
            const ptr = try valExpr(b, scope, unary_expr);
            // TODO: load everywhere
            switch (ri.semantics) {
                .val => return b.add(.load, .{ .operand = ptr, .node = node }),
                .ref => return ptr,
                .ty => unreachable,
            }
        },
        // nop - +x is just x
        // TODO: consider removing this syntax entirely
        .plus => return node,
        .minus => {
            const operand = try valExpr(b, scope, unary_expr);
            return b.add(.neg, .{ .node = node, .operand = operand });
        },
        .bang => {
            // reduces to a bool comparison to 0
            // just coerce this to a u1 so we know we're operating on a bool
            const inner = try valExpr(b, scope, unary_expr);
            const bool_type = try b.add(.ty, .{ .ty = Type.Common.u1_type });
            const operand = try coerce(b, scope, inner, bool_type, node);
            const other = try b.add(.constant, .{
                .ty = bool_type,
                .val = try hg.pool.internValue(Value.Common.zero),
                .node = node,
            });
            return b.add(.cmp_eq, .{ .node = node, .lref = operand, .rref = other });
        },
        .tilde => {
            const operand = try valExpr(b, scope, unary_expr);
            return b.add(.bit_not, .{ .node = node, .operand = operand });
        },
        else => {
            std.log.err("unary: unexpected token: {}\n", .{hg.tree.tokenTag(operator_token)});
            return Error.UnexpectedToken;
        },
    }
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

        const ty_ref = try typeExpr(b, scope, data);
        const param_index = try b.addParam(param_id, ty_ref, param);
        b.scratch.appendAssumeCapacity(param_index);

        const ref = param_index;
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

    const return_type = try typeExpr(&body_scope.editor, &body_scope.base, signature.return_ty);
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

fn arrayInitializer(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const arena = hg.arena;
    const array_initializer = hg.tree.data(node).array_init;

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    const elements = hg.tree.extra_data[array_initializer.elements_start..array_initializer.elements_end];
    try b.scratch.ensureUnusedCapacity(arena, elements.len);
    for (elements) |element| {
        const element_index = try valExpr(b, scope, element);
        b.scratch.appendAssumeCapacity(element_index);
    }

    const hir_elements = b.scratch.items[scratch_top..];
    return b.addArray(hir_elements, node);
}

fn expr(b: *BlockEditor, scope: *Scope, ri: ResultInfo, node: Node.Index) !Hir.Index {
    return switch (ri.semantics) {
        .val => switch (b.hg.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .char_literal => charLiteral(b, node),
            .string_literal => stringLiteral(b, node),
            .ident => identExpr(b, scope, ri, node),
            .call => try call(b, scope, node),
            .binary => try binary(b, scope, node),
            .unary => try unary(b, scope, ri, node),
            .fn_decl => try fnDecl(b, scope, node),
            .array_init => try arrayInitializer(b, scope, node),
            .index => try indexAccess(b, scope, ri, node),
            .field => try fieldAccess(b, scope, ri, node),
            .block => {
                var block_scope = try Block.init(b, scope);
                defer block_scope.deinit();
                const bl = try block(&block_scope.editor, &block_scope.base, node, true);
                try b.linkInst(bl);
                return bl;
            },
            .if_else => try ifElse(b, scope, node),
            else => {
                std.log.err("expr (val semantics): unexpected node: {}\n", .{b.hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .ref => switch (b.hg.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .ident => identExpr(b, scope, ri, node),
            .call => try call(b, scope, node),
            .binary => try binary(b, scope, node),
            .unary => try unary(b, scope, ri, node),
            .index => try indexAccess(b, scope, ri, node),
            .field => try fieldAccess(b, scope, ri, node),
            else => {
                std.log.err("expr (ref semantics): unexpected node: {}\n", .{b.hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .ty => switch (b.hg.tree.data(node)) {
            .ident => identExpr(b, scope, ri, node),
            .unary => try unary(b, scope, ri, node),
            .pointer => try pointerType(b, scope, node),
            .many_pointer => try manyPointerType(b, scope, node),
            .array => try arrayType(b, scope, node),
            .function => try fnType(b, scope, node),
            else => {
                std.log.err("expr (type semantics): unexpected node: {}\n", .{b.hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
    };
}

inline fn valExpr(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const ri: ResultInfo = .{ .semantics = .val };
    return expr(b, scope, ri, node);
}

inline fn refExpr(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const ri: ResultInfo = .{ .semantics = .ref };
    return expr(b, scope, ri, node);
}

inline fn typeExpr(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const ri: ResultInfo = .{ .semantics = .ty };
    return expr(b, scope, ri, node);
}

fn pointerType(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const pointee = hg.tree.data(node).pointer;
    const inner = try typeExpr(b, scope, pointee);
    return b.add(.create_pointer_type, .{ .operand = inner, .node = node });
}

fn manyPointerType(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const pointee = hg.tree.data(node).many_pointer;
    const inner = try typeExpr(b, scope, pointee);
    return b.add(.create_unsafe_pointer_type, .{ .operand = inner, .node = node });
}

fn sliceType(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const element_type = hg.tree.data(node).slice;
    const inner = try typeExpr(b, scope, element_type);
    return b.add(.create_slice_type, .{ .operand = inner, .node = node });
}

fn fnType(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const fn_type = hg.tree.data(node).function;
    const signature = hg.tree.extraData(fn_type, Node.FnSignature);

    const params = hg.tree.extra_data[signature.params_start..signature.params_end];
    const param_types = try hg.arena.alloc(Hir.Index, signature.params_end - signature.params_start);
    defer hg.arena.free(param_types);
    for (params, 0..) |param, i| {
        const data = hg.tree.data(param).param;
        param_types[i] = try typeExpr(b, scope, data);
    }
    const return_type = try typeExpr(b, scope, signature.return_ty);

    return b.addFunctionType(return_type, param_types, node);
}

fn arrayType(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const array_type = hg.tree.data(node).array;
    const element_type = try typeExpr(b, scope, array_type.element_type);
    const count = try valExpr(b, scope, array_type.count_expr);
    // TODO: make sure count is actually a comptime_int
    return b.add(.create_array_type, .{ .element_type = element_type, .count = count, .node = node });
}

fn indexAccess(b: *BlockEditor, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const index_access = hg.tree.data(node).index;
    const array = try refExpr(b, scope, index_access.operand);
    const inner = try valExpr(b, scope, index_access.index);
    // TODO: actual variable length usize
    const usize_type = try b.add(.ty, .{ .ty = Type.Common.u64_type });
    const access_index = try b.add(.coerce, .{
        .ty = usize_type,
        .val = inner,
        .node = index_access.index,
    });
    const ptr = try b.add(.array_access, .{ .array = array, .index = access_index, .node = node });

    switch (ri.semantics) {
        .val => return b.add(.load, .{ .operand = ptr, .node = node }),
        .ref => return ptr,
        .ty => unreachable,
    }
}

fn fieldAccess(b: *BlockEditor, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Hir.Index {
    const hg = b.hg;
    const field_access = hg.tree.data(node).field;
    const operand = try refExpr(b, scope, field_access);
    const ptr = try b.add(.field_access, .{ .operand = operand, .node = node });

    switch (ri.semantics) {
        .val => return b.add(.load, .{ .operand = ptr, .node = node }),
        .ref => return ptr,
        .ty => unreachable,
    }
}

fn statement(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    return try switch (b.hg.tree.data(node)) {
        .const_decl => constDecl(b, scope, node),
        .const_decl_attr => constDeclAttr(b, scope, node),
        .var_decl => varDecl(b, scope, node),
        .assign_simple => assignSimple(b, scope, node),
        .assign_binary => assignBinary(b, scope, node),
        .if_simple => ifSimple(b, scope, node),
        .if_else => ifElse(b, scope, node),
        .if_chain => ifChain(b, scope, node),
        .return_val => returnStmt(b, scope, node),
        .yield_val => yield(b, scope, node),
        .loop_forever => loopForever(b, scope, node),
        .loop_conditional => loopConditional(b, scope, node),
        .loop_range => loopRange(b, scope, node),
        .@"break" => loopBreak(b, scope, node),
        .call => call(b, scope, node),
        else => {
            std.log.err("statement: unexpected node: {}\n", .{b.hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn globalStatement(hg: *HirGen, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const data = hg.tree.data(node);
    return try switch (data) {
        .const_decl => globalConst(hg, scope, node),
        .const_decl_attr => globalConstAttr(hg, scope, node),
        .var_decl => globalVar(hg, scope, node),
        else => {
            std.log.err("global statement: unexpected node: {}\n", .{hg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn call(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const call_expr = b.hg.tree.data(node).call;
    const ri: ResultInfo = .{ .semantics = .ref };
    const ptr = try identExpr(b, scope, ri, call_expr.ptr);

    const scratch_top = b.scratch.items.len;
    defer b.scratch.shrinkRetainingCapacity(scratch_top);

    const arg_nodes = b.hg.tree.extra_data[call_expr.args_start..call_expr.args_end];
    try b.scratch.ensureUnusedCapacity(b.hg.arena, arg_nodes.len);
    for (arg_nodes, 0..) |arg_node, i| {
        const arg_inner = try valExpr(b, scope, arg_node);
        const param_index: u32 = @intCast(i);
        const param_type = try b.add(.param_type_of, .{
            .ptr = ptr,
            .param_index = param_index,
            .node = arg_node,
        });
        const arg = try b.add(.coerce, .{ .ty = param_type, .val = arg_inner, .node = arg_node });
        b.scratch.appendAssumeCapacity(arg);
    }

    const args = b.scratch.items[scratch_top..];
    return b.addCall(ptr, args, node);
}

fn block(b: *BlockEditor, scope: *Scope, node: Node.Index, comptime add_unlinked: bool) Error!Hir.Index {
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
                var_scope.* = Scope.LocalVal.init(s, id, ref);
                s = &var_scope.base;
            },
            .var_decl => {
                const ident = hg.tree.tokenString(hg.tree.mainToken(stmt) + 2);
                const id = try hg.interner.intern(ident);
                const var_scope = try hg.arena.create(Scope.LocalPtr);
                var_scope.* = Scope.LocalPtr.init(s, id, ref);
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

fn coerce(b: *BlockEditor, s: *Scope, val: Hir.Index, dest_ty: Hir.Index, node: Node.Index) !Hir.Index {
    _ = s;
    // generate a "type validation" marker instruction
    // this is a passthrough which takes in the above value reference
    // and the type reference and returns the value reference
    // semantic analysis will validate that the type is as it should be
    // and then remove this instruction in the mir
    return b.add(.coerce, .{ .val = val, .ty = dest_ty, .node = node });
}

fn constDecl(b: *BlockEditor, s: *Scope, node: Node.Index) !Hir.Index {
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
    const ref = try valExpr(b, s, const_decl.val);
    _ = id;
    // _ = try b.addDebugValue(ref, id, node);

    if (const_decl.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try typeExpr(b, s, const_decl.ty);
        return try coerce(b, s, ref, dest_ty, node);
    }
}

fn constDeclAttr(b: *BlockEditor, s: *Scope, node: Node.Index) !Hir.Index {
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
    const ref = try valExpr(b, s, const_decl.val);
    _ = id;
    // _ = try b.addDebugValue(ref, id, node);

    if (metadata.ty == 0) {
        // untyped (inferred) declaration
        return ref;
    } else {
        const dest_ty = try typeExpr(b, s, metadata.ty);
        return try coerce(b, s, ref, dest_ty, node);
    }
}

fn varDecl(b: *BlockEditor, s: *Scope, node: Node.Index) !Hir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.hg.tree.data(node).var_decl;
    const val = try valExpr(b, s, var_decl.val);
    if (var_decl.ty == 0) {
        // untyped (inferred) declaration
        return try b.add(.push, .{ .operand = val, .node = node });
    } else {
        // type annotated declaration
        const dest_ty = try typeExpr(b, s, var_decl.ty);
        const coerced = try coerce(b, s, val, dest_ty, node);
        return try b.add(.push, .{ .operand = coerced, .node = node });
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
    const b = &inline_block.editor;

    const handle = try b.add(.global_handle, .{ .node = node });

    var rvalue = try valExpr(&inline_block.editor, scope, const_decl.val);
    if (const_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&inline_block.editor, scope, const_decl.ty);
        _ = try b.add(.global_set_type, .{
            .handle = handle,
            .operand = type_annotation,
            .node = node,
        });

        rvalue = try coerce(b, scope, rvalue, type_annotation, node);
    } else {
        // generate a type inference, since global decls can't
        // postpone the problem like locals can
        const type_inference = try b.add(.type_of, .{ .operand = rvalue, .node = node });
        _ = try b.add(.global_set_type, .{
            .handle = handle,
            .operand = type_inference,
            .node = node,
        });
    }

    _ = try b.add(.global_set_init, .{ .handle = handle, .operand = rvalue, .node = node });
    _ = try b.add(.yield_inline, .{ .operand = handle, .node = node });
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

    const handle = try b.add(.global_handle, .{ .node = node });

    const attrs = hg.tree.extra_data[metadata.attrs_start..metadata.attrs_end];

    var has_initializer = true;
    for (attrs) |attr| {
        switch (hg.tree.tokenTag(hg.tree.mainToken(attr))) {
            .a_export => {
                _ = try b.add(.global_set_linkage_external, .{ .operand = handle, .node = node });
            },
            .a_import => {
                _ = try b.add(.global_set_linkage_external, .{ .operand = handle, .node = node });
                has_initializer = false;
            },
            else => unreachable,
        }
    }

    var rvalue: ?u32 = null;
    if (has_initializer) {
        std.debug.assert(const_decl.val != 0);
        rvalue = try valExpr(b, scope, const_decl.val);
    } else {
        std.debug.assert(const_decl.val == 0);
        std.debug.assert(metadata.ty != 0); // this should be a handled user error
    }

    if (metadata.ty != 0) {
        const type_annotation = try typeExpr(b, scope, metadata.ty);
        _ = try b.add(.global_set_type, .{
            .handle = handle,
            .operand = type_annotation,
            .node = node,
        });

        if (rvalue) |val| {
            rvalue = try coerce(b, scope, val, type_annotation, node);
        }
    } else {
        // generate a type inference, since global decls can't
        // postpone the problem like locals can
        const type_inference = try b.add(.type_of, .{ .operand = rvalue.?, .node = node });
        _ = try b.add(.global_set_type, .{
            .handle = handle,
            .operand = type_inference,
            .node = node,
        });
    }

    if (rvalue) |val| {
        _ = try b.add(.global_set_init, .{
            .handle = handle,
            .operand = val,
            .node = node,
        });
    }

    _ = try b.add(.yield_inline, .{ .operand = handle, .node = node });
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
    const b = &inline_block.editor;

    const handle = try b.add(.global_handle, .{ .node = node });
    _ = try b.add(.global_set_mutable, .{ .operand = handle, .node = node });

    var rvalue = try valExpr(&inline_block.editor, scope, var_decl.val);
    if (var_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&inline_block.editor, scope, var_decl.ty);
        _ = try b.add(.global_set_type, .{
            .handle = handle,
            .operand = type_annotation,
            .node = node,
        });

        rvalue = try coerce(b, scope, rvalue, type_annotation, node);
        _ = try b.add(.global_set_init, .{ .handle = handle, .operand = rvalue, .node = node });
    }

    _ = try b.add(.yield_inline, .{ .operand = handle, .node = node });
    return BlockEditor.addBlockInlineUnlinked(hg, b, node);
}

fn assignSimple(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_simple;

    const ptr = refExpr(b, scope, assign.ptr) catch |err| {
        if (err == error.InvalidLvalue) {
            // TODO: not really accurate?
            try hg.errors.append(hg.gpa, .{
                .tag = .const_assign,
                .token = hg.tree.mainToken(node),
            });
            return error.ConstAssign;
        } else {
            return err;
        }
    };
    const val = try valExpr(b, scope, assign.val);
    return b.add(.store, .{ .ptr = ptr, .val = val, .node = node });
}

fn conditionExpr(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const ref = try valExpr(b, scope, node);
    const condition_type = try b.addType(Type.Common.u1_type);
    return try coerce(b, scope, ref, condition_type, node);
}

fn assignBinary(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const assign = hg.tree.data(node).assign_binary;
    const op = hg.tree.mainToken(node);

    const ptr = refExpr(b, scope, assign.ptr) catch |err| {
        if (err == error.InvalidLvalue) {
            // TODO: not really accurate?
            try hg.errors.append(hg.gpa, .{
                .tag = .const_assign,
                .token = hg.tree.mainToken(node),
            });
            return error.ConstAssign;
        } else {
            return err;
        }
    };
    const base = try valExpr(b, scope, assign.ptr);
    const val = try valExpr(b, scope, assign.val);
    const bin = try binaryInner(b, node, op, base, val);

    return b.add(.store, .{ .ptr = ptr, .val = bin, .node = node });
}

fn ifSimple(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_simple = b.hg.tree.data(node).if_simple;
    var block_scope = try Block.init(b, scope);
    defer block_scope.deinit();
    const s = &block_scope.base;

    const condition = try conditionExpr(b, scope, if_simple.condition);
    const exec = try block(&block_scope.editor, s, if_simple.exec_true, true);
    return b.add(.branch_single, .{ .condition = condition, .exec_true = exec, .node = node });
}

fn ifElse(b: *BlockEditor, scope: *Scope, node: Node.Index) Error!Hir.Index {
    const if_else = b.hg.tree.data(node).if_else;

    const condition = try conditionExpr(b, scope, if_else.condition);
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
    return b.add(.branch_double, .{
        .condition = condition,
        .exec_true = exec_true,
        .exec_false = exec_false,
        .node = node,
    });
}

fn ifChain(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const if_chain = b.hg.tree.data(node).if_chain;

    const condition = try conditionExpr(b, scope, if_chain.condition);
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

    return b.add(.branch_double, .{
        .condition = condition,
        .exec_true = exec_true,
        .exec_false = next,
        .node = node,
    });
}

fn returnStmt(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const return_val = b.hg.tree.data(node).return_val;
    const return_type = try b.add(.ret_type, .{ .node = node });
    const operand = if (return_val == 0) try b.add(.none, .{}) else op: {
        const ref = try valExpr(b, scope, return_val);
        break :op try coerce(b, scope, ref, return_type, node);
    };

    return b.add(.ret_node, .{ .operand = operand, .node = node });
}

fn yield(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    const yield_val = b.hg.tree.data(node).yield_val;
    const operand = try valExpr(b, scope, yield_val);

    return b.add(.yield_node, .{ .operand = operand, .node = node });
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
        const condition = try b.addConstant(Type.Common.u1_type, Value.Common.one, node);
        _ = try block_scope.editor.add(.yield_implicit, .{ .operand = condition, .tok = undefined });
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

        const condition = try conditionExpr(&block_scope.editor, s, loop_conditional.condition);
        _ = try block_scope.editor.add(.yield_implicit, .{ .operand = condition, .tok = undefined });
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

    const ref = try statement(b, scope, signature.binding);
    var s: *Scope = var_scope: {
        const ident = hg.tree.tokenString(hg.tree.mainToken(signature.binding) + 2);
        const id = try hg.interner.intern(ident);
        var var_scope = Scope.LocalPtr.init(scope, id, ref);
        break :var_scope &var_scope.base;
    };

    const condition = block: {
        var block_scope = try Block.init(b, s);
        defer block_scope.deinit();
        const bs = &block_scope.base;

        const condition_inner = try valExpr(&block_scope.editor, bs, signature.condition);
        const condition_type = try b.addType(Type.Common.u1_type);
        const condition = try coerce(&block_scope.editor, bs, condition_inner, condition_type, node);
        _ = try block_scope.editor.add(.yield_implicit, .{ .operand = condition, .tok = undefined });
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

        _ = try statement(&outer_scope.editor, &outer_scope.base, signature.afterthought);
        // if (try statement(&outer_scope.editor, &outer_scope.base, signature.afterthought)) |_| {
        // return error.AfterthoughtDecl;
        // }

        break :body try b.addBlockUnlinked(&outer_scope.editor, node);
    };

    return b.addLoop(condition, body, node);
}

fn loopBreak(b: *BlockEditor, scope: *Scope, node: Node.Index) !Hir.Index {
    _ = scope;
    return b.add(.loop_break, .{ .node = node });
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
                std.log.err("module: unexpected node: {}\n", .{hg.tree.data(node)});
                return GenError.NotImplemented;
            },
        }
    }

    return try addModule(hg, node);
}

pub inline fn resolveType(hg: *HirGen, index: Hir.Index) !Type {
    return hg.getTempHir().resolveType(hg.gpa, index);
}

pub fn getTempHir(hg: *HirGen) Hir {
    return .{
        .tree = hg.tree,
        .insts = hg.insts.slice(),
        .extra_data = hg.extra.items,
        .pool = .{ .values = hg.pool.values.slice() },
        .types = hg.types.items,
        .block_slices = hg.block_slices.items,
        .interner = hg.interner,
        .instmap = hg.instmap.items,
        .untyped_decls = hg.untyped_decls, // TODO: not good
        .errors = hg.errors.items,
        .module_index = hg.module_index,
    };
}

pub inline fn instToInt(hg: *HirGen, inst: Hir.Index) u64 {
    return hg.getTempHir().instToInt(inst);
}

pub inline fn instToFloat(hg: *HirGen, inst: Hir.Index) f64 {
    return hg.getTempHir().instToFloat(inst);
}
