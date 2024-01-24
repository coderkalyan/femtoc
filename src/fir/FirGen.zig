const std = @import("std");
const Ast = @import("../Ast.zig");
const Fir = @import("Fir.zig");
const Scope = @import("Scope.zig");
const InternPool = @import("../InternPool.zig");
const parseFloat = @import("../floatLiteral.zig").parseFloat;
const error_handler = @import("../error_handler.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const Inst = Fir.Inst;
const Block = Scope.Block;
const FirGen = @This();

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

const Error = GenError || Allocator.Error;

// for allocating instrutions and data that lasts the lifetime of the compiler
// or at least, as long as the driver cares about it
gpa: Allocator,
// for allocation scratch data that lasts the lifetime of this lowering stage
arena: Allocator,
tree: *const Ast,
insts: std.MultiArrayList(Inst.Data),
locs: std.ArrayListUnmanaged(Inst.Loc),
extra: std.ArrayListUnmanaged(u32),
scratch: std.ArrayListUnmanaged(u32),
pool: *InternPool,
errors: std.ArrayListUnmanaged(error_handler.SourceError),
lazy_src_insts: std.ArrayListUnmanaged(Fir.Index),

pub fn lowerAst(gpa: Allocator, pool: *InternPool, tree: *const Ast) !Fir {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var fg: FirGen = .{
        .gpa = gpa,
        .arena = arena.allocator(),
        .tree = tree,
        .insts = .{},
        .locs = .{},
        .extra = .{},
        .scratch = .{},
        .pool = pool,
        .errors = .{},
        .lazy_src_insts = .{},
    };

    // post order format guarantees that the module node will be the last
    const module_node: u32 = @intCast(tree.nodes.len - 1);
    const module_index = fg.module(module_node) catch return .{
        .tree = tree,
        .insts = fg.insts.toOwnedSlice(),
        .locs = try fg.locs.toOwnedSlice(gpa),
        .extra = try fg.extra.toOwnedSlice(gpa),
        .module_index = undefined,
        .errors = try fg.errors.toOwnedSlice(gpa),
        .pool = pool,
    };

    return .{
        .tree = tree,
        .insts = fg.insts.toOwnedSlice(),
        .locs = try fg.locs.toOwnedSlice(gpa),
        .extra = try fg.extra.toOwnedSlice(gpa),
        .errors = try fg.errors.toOwnedSlice(gpa),
        .module_index = module_index,
        .pool = pool,
    };
}

const builtin_types = std.ComptimeStringMap(void, .{
    .{ "u8", {} },
    .{ "u16", {} },
    .{ "u32", {} },
    .{ "u64", {} },
    .{ "i8", {} },
    .{ "i16", {} },
    .{ "i32", {} },
    .{ "i64", {} },
    .{ "f32", {} },
    .{ "f64", {} },
    .{ "bool", {} },
    .{ "void", {} },
});

const ResultInfo = struct {
    semantics: Semantics,
    ctx: Context,

    const Semantics = enum {
        // the expression should generate an rvalue that can be used, loading from memory if necessary
        val,
        // the expression should generate an lvalue reference/pointer to be used as a location to assign to
        ptr,
        // similar to ref, but applies to non-lvalues. the reference (&) operator uses this
        ref,
        ty,
        none,
    };

    const Context = enum {
        none,
        elem,
        store,
    };
};

pub fn addExtra(fg: *FirGen, extra: anytype) !Fir.ExtraIndex {
    const len: u32 = @intCast(fg.extra.items.len);
    const fields = std.meta.fields(@TypeOf(extra));
    try fg.extra.ensureUnusedCapacity(fg.gpa, fields.len);
    inline for (fields) |field| {
        switch (field.type) {
            inline else => {
                const num: u32 = @intFromEnum(@field(extra, field.name));
                fg.extra.appendAssumeCapacity(@bitCast(num));
            },
        }
    }
    return @enumFromInt(len);
}

pub fn extraData(fg: *FirGen, comptime T: type, index: Fir.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(fg.extra.items[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(fg: *FirGen, slice: Fir.Inst.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return fg.extra.items[start..end];
}

pub fn addSlice(fg: *FirGen, slice: []const u32) !Fir.ExtraIndex {
    const start: u32 = @intCast(fg.extra.items.len);
    try fg.extra.appendSlice(fg.gpa, slice);
    const end: u32 = @intCast(fg.extra.items.len);
    return fg.addExtra(Inst.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    });
}

// constructs a Fir.Inst by consolidating data insts and locs arrays
pub fn get(fg: *FirGen, index: Fir.Index) Fir.Inst {
    const i: u32 = @intFromEnum(index);
    std.debug.assert(i < fg.insts.len);
    return .{
        .data = fg.insts.get(i),
        .loc = fg.locs.items[i],
    };
}

pub fn add(fg: *FirGen, inst: Fir.Inst) !Fir.Index {
    const len: u32 = @intCast(fg.insts.len);
    // make sure both arrays can reserve before we append to either
    // so we can append atomically and not go out of sync
    try fg.insts.ensureUnusedCapacity(fg.gpa, 1);
    try fg.locs.ensureUnusedCapacity(fg.gpa, 1);
    fg.insts.appendAssumeCapacity(inst.data);
    fg.locs.appendAssumeCapacity(inst.loc);
    std.debug.assert(fg.insts.len == fg.locs.items.len);
    return @enumFromInt(len);
}

pub fn reserve(fg: *FirGen, comptime tag: Fir.Inst.Tag) !Fir.Index {
    const len: u32 = @intCast(fg.insts.len);
    // make sure both arrays can reserve before we append to either
    // so we can append atomically and not go out of sync
    try fg.insts.ensureUnusedCapacity(fg.gpa, 1);
    try fg.locs.ensureUnusedCapacity(fg.gpa, 1);
    fg.insts.appendAssumeCapacity(@unionInit(Fir.Inst.Data, @tagName(tag), undefined));
    fg.locs.addOneAssumeCapacity();
    std.debug.assert(fg.insts.len == fg.locs.items.len);
    return @enumFromInt(len);
}

pub fn reserveLoc(fg: *FirGen, comptime tag: Fir.Inst.Tag, loc: Fir.Inst.Loc) !Fir.Index {
    const len: u32 = @intCast(fg.insts.len);
    // make sure both arrays can reserve before we append to either
    // so we can append atomically and not go out of sync
    try fg.insts.ensureUnusedCapacity(fg.gpa, 1);
    try fg.locs.ensureUnusedCapacity(fg.gpa, 1);
    fg.insts.appendAssumeCapacity(@unionInit(Fir.Inst.Data, @tagName(tag), undefined));
    fg.locs.appendAssumeCapacity(loc);
    std.debug.assert(fg.insts.len == fg.locs.items.len);
    return @enumFromInt(len);
}

pub fn update(fg: *FirGen, index: Fir.Index, inst: Fir.Inst) void {
    fg.insts[@intFromEnum(index)] = inst.data;
    fg.locs[@intFromEnum(index)] = inst.loc;
}

fn module(fg: *FirGen, node: Node.Index) !Fir.Index {
    const data = fg.tree.data(node).module;

    var module_scope: Scope.Module = .{};
    var namespace = Scope.Namespace.init(&module_scope.base);
    var s = &namespace.base;

    // first pass through statements - "forward declare" all identifiers
    // in the toplevel namespace so that forward-referencing and recursive
    // function calls resolve
    // note that since we link to the node id, we can resolve identifiers
    // that haven't been irgen'ed yet
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const sl = fg.tree.extraData(data.stmts, Node.ExtraSlice);
    var stmts = fg.tree.extraSlice(sl);
    try fg.scratch.ensureUnusedCapacity(fg.arena, stmts.len);

    for (stmts) |stmt| {
        const id = try fg.globalIdentId(stmt);
        try namespace.decls.put(fg.arena, id, {});
    }

    // second pass - generate the value/expression and update the resolution map
    // so we can link node ids to instructions
    for (stmts) |stmt| {
        const id = try fg.globalIdentId(stmt);
        const inst = try globalStatement(fg, &s, stmt);
        const global = try fg.add(.{
            .data = .{ .global = .{ .name = id, .block = inst } },
            .loc = .{ .node = stmt },
        });
        fg.scratch.appendAssumeCapacity(@intFromEnum(global));
    }

    const insts = fg.scratch.items[scratch_top..];
    const pl = try fg.addSlice(insts);
    return fg.add(.{
        .data = .{ .module = .{ .insts = pl } },
        .loc = .{ .node = node },
    });
}

fn integerLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const int = b.tree.data(node).integer_literal;
    return b.addNodeLoc(.{ .int = int }, node);
}

fn floatLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const float_token = b.tree.mainToken(node);
    const float_str = b.tree.tokenString(float_token);
    const float = try parseFloat(float_str);
    return b.addNodeLoc(.{ .float = float }, node);
}

fn boolLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const bool_token = b.tree.mainToken(node);
    const value: bool = switch (b.tree.tokenTag(bool_token)) {
        .k_true => true,
        .k_false => false,
        else => unreachable,
    };
    return b.addNodeLoc(.{ .bool = value }, node);
}

fn charLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const char_token = b.tree.mainToken(node);
    const char_str = b.tree.tokenString(char_token);
    return b.addNodeLoc(.{ .int = char_str[1] }, node);
}

fn stringLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const string_token = b.tree.mainToken(node);
    const string_text = b.tree.tokenString(string_token);
    const string_literal = string_text[1 .. string_text.len - 1];
    const id = try fg.pool.getOrPutString(string_literal);

    return b.addNodeLoc(.{ .string = id }, node);
}

fn identExpr(b: *Block, s: **Scope, ri: ResultInfo, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const ident_token = b.tree.mainToken(node);
    const ident_str = b.tree.tokenString(ident_token);
    const scope = s.*;

    // check identifier against builtin types
    if (builtin_types.has(ident_str)) {
        switch (ri.semantics) {
            .ty => return b.add(.{
                .data = .{ .builtin_type = {} },
                .loc = .{ .node = node },
            }),
            else => {
                // TODO: this check should maybe happen during declaration
                try fg.errors.append(fg.gpa, .{
                    .tag = .shadows_builtin_type,
                    .token = ident_token,
                });
                return error.HandledUserError;
            },
        }
    }

    const id = try fg.pool.getOrPutString(ident_str);
    const ident_scope = (try scope.resolveIdent(id)) orelse {
        try fg.errors.append(fg.gpa, .{
            .tag = .unknown_identifier,
            .token = ident_token,
        });
        return error.HandledUserError;
    };

    switch (ident_scope.tag) {
        // for constants, the ref points to the instruction that returned its value
        // so we need to check that this is an rvalue context
        // we can't use consts in an lvalue context, aka on the left side of an assignment
        .local_val => {
            const local_val = ident_scope.cast(Scope.LocalVal).?;
            switch (ri.semantics) {
                // rvalue - return the instruction
                .val, .none => return local_val.inst,
                // lvalue - emit an error
                .ptr => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .const_variable_assign,
                        .token = ident_token,
                    });
                    return error.HandledUserError;
                },
                // upgrade to const pointer
                .ref => {
                    const ptr = try b.add(.{
                        .data = .{ .push = local_val.inst },
                        .loc = .{ .node = node },
                    });
                    const ptr_scope = try b.fg.arena.create(Scope.LocalPtr);
                    ptr_scope.* = Scope.LocalPtr.init(scope, local_val.ident, ptr);
                    s.* = &ptr_scope.base;
                    return ptr;
                },
                .ty => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .named_var_type_context,
                        .token = ident_token,
                    });
                    return error.HandledUserError;
                },
            }
        },
        .local_ptr => {
            const local_ptr = ident_scope.cast(Scope.LocalPtr).?;
            switch (ri.semantics) {
                // but for mutable variables, the ref points to the address
                // in memory so we have to generate a load instruction to
                // get the value at that address
                .val => switch (ri.ctx) {
                    .none => return b.add(.{
                        .data = .{ .load = .{ .ptr = local_ptr.ptr } },
                        .loc = .{ .node = node },
                    }),
                    .elem, .store => return b.add(.{
                        .data = .{ .load_lazy = .{ .ptr = local_ptr.ptr } },
                        .loc = .{ .node = node },
                    }),
                },
                .ptr, .ref, .none => return local_ptr.ptr,
                .ty => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .named_var_type_context,
                        .token = ident_token,
                    });
                    return error.HandledUserError;
                },
            }
        },
        .local_type => {
            const local_type = ident_scope.cast(Scope.LocalType).?;
            switch (ri.semantics) {
                .ty, .none => return local_type.inst,
                .val, .ptr, .ref => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .named_type_var_context,
                        .token = ident_token,
                    });
                    return error.HandledUserError;
                },
            }
        },
        .namespace => {
            const ptr = try b.add(.{
                .data = .{ .load_global = .{ .name = id } },
                .loc = .{ .node = node },
            });

            switch (ri.semantics) {
                .val => return b.add(.{
                    .data = .{ .load = .{ .ptr = ptr } },
                    .loc = .{ .node = node },
                }),
                .ptr, .ref, .none => return ptr,
                .ty => unreachable,
            }
        },
        else => unreachable,
    }
}

fn binaryInner(b: *Block, node: Node.Index, op: Ast.TokenIndex, l: Fir.Index, r: Fir.Index) Error!Fir.Index {
    const tag: std.meta.Tag(Inst.Data) = switch (b.tree.tokenTag(op)) {
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
        .k_xor => .bitwise_xor,
        else => return error.UnexpectedToken,
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
        .cmp_ge,
        .cmp_lt,
        .cmp_le,
        .sr,
        .sl,
        => |t| return b.addNodeLoc(@unionInit(Inst.Data, @tagName(t), .{ .l = l, .r = r }), node),
        else => unreachable,
    }
}

fn binary(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const operator_token = b.tree.mainToken(node);
    const tag = b.tree.tokenTag(operator_token);
    switch (tag) {
        .k_or, .k_and, .k_xor, .k_implies => return binaryShort(b, scope, node),
        else => {},
    }

    const binary_expr = b.tree.data(node).binary;
    const left = try valExpr(b, scope, binary_expr.left);
    const right = try valExpr(b, scope, binary_expr.right);
    return binaryInner(b, node, operator_token, left, right);
}

fn binaryShort(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const binary_expr = b.tree.data(node).binary;
    const operator_token = b.tree.mainToken(node);
    const tag = b.tree.tokenTag(operator_token);

    // emit a branch expression
    switch (tag) {
        // if a { yield true } else { yield b }
        .k_or => {
            const a = try valExpr(b, scope, binary_expr.left);
            const short = try b.addNodeLoc(.{ .bool = true }, node);
            const exec_true = try wrapInst(b, scope, short, node);
            const exec_false = try wrapExpression(b, scope, binary_expr.right);
            return b.addBranchDouble(a, exec_true, exec_false, node);
        },
        // if !a { yield false } else { yield b }
        .k_and => {
            const a = try valExpr(b, scope, binary_expr.left);
            const not = try b.addNodeLoc(.{ .logical_not = a }, node);
            const short = try b.addNodeLoc(.{ .bool = false }, node);
            const exec_true = try wrapInst(b, scope, short, node);
            const exec_false = try wrapExpression(b, scope, binary_expr.right);
            return b.addBranchDouble(not, exec_true, exec_false, node);
        },
        // not lossy, cannot short circuit
        .k_xor => {
            const left = try valExpr(b, scope, binary_expr.left);
            const right = try valExpr(b, scope, binary_expr.right);
            return binaryInner(b, node, operator_token, left, right);
        },
        // if !a { yield true } else { yield b }
        .k_implies => {
            const a = try valExpr(b, scope, binary_expr.left);
            const not = try b.addNodeLoc(.{ .logical_not = a }, node);
            const short = try b.addNodeLoc(.{ .bool = true }, node);
            const exec_true = try wrapInst(b, scope, short, node);
            const exec_false = try wrapExpression(b, scope, binary_expr.right);
            return b.addBranchDouble(not, exec_true, exec_false, node);
        },
        else => unreachable,
    }
}

fn unary(b: *Block, scope: **Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
    const unary_expr = b.tree.data(node).unary;
    const operator_token = b.tree.mainToken(node);

    switch (b.tree.tokenTag(operator_token)) {
        // calculate a reference to the pointer and return it
        .ampersand => return b.addNodeLoc(.{ .reftoptr = try refExpr(b, scope, unary_expr) }, node),
        .asterisk => {
            const ref = try b.addNodeLoc(.{ .ptrtoref = try valExpr(b, scope, unary_expr) }, node);
            switch (ri.semantics) {
                .val => return b.addNodeLoc(.{ .load = .{ .ptr = ref } }, node),
                .ptr, .ref, .none => return ref,
                .ty => unreachable,
            }
        },
        .minus => return b.addNodeLoc(.{ .neg = try valExpr(b, scope, unary_expr) }, node),
        .bang => return b.addNodeLoc(.{ .logical_not = try valExpr(b, scope, unary_expr) }, node),
        .tilde => return b.addNodeLoc(.{ .bitwise_not = try valExpr(b, scope, unary_expr) }, node),
        else => {
            std.log.err("unary: unexpected token: {}\n", .{b.tree.tokenTag(operator_token)});
            return error.UnexpectedToken;
        },
    }
}

fn functionLiteral(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const function_literal = b.tree.data(node).function_literal;
    const function_type = b.tree.data(function_literal.ty).function_type;

    // used to store parameters temporarily
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const sl = b.tree.extraData(function_type.params, Node.ExtraSlice);
    const params = b.tree.extraSlice(sl);
    try fg.scratch.ensureUnusedCapacity(fg.arena, params.len);

    var s: **Scope = scope;
    for (params) |param| {
        const data = b.tree.data(param).param;
        const param_token = b.tree.mainToken(param);
        const param_str = b.tree.tokenString(param_token);
        const param_id = try fg.pool.getOrPutString(param_str);

        const ty_ref = try typeExpr(b, scope, data);
        const param_index = try b.addUnlinked(.{
            .data = .{ .param = .{ .name = param_id, .ty = ty_ref } },
            .loc = .{ .node = param },
        });
        fg.scratch.appendAssumeCapacity(@intFromEnum(param_index));

        const ref = param_index;
        var param_scope = try fg.arena.create(Scope.LocalVal);
        param_scope.* = Scope.LocalVal.init(s.*, param_id, ref);
        s.* = &param_scope.base;
    }

    var body_scope = Block.init(b.fg, s.*);
    defer body_scope.deinit();
    s.* = &body_scope.base;

    const fir_params = fg.scratch.items[scratch_top..];
    const scratch_top_new = fg.scratch.items.len;
    try fg.scratch.ensureUnusedCapacity(fg.arena, fir_params.len);
    for (fir_params) |param| {
        try body_scope.linkInst(@enumFromInt(param));
        const param_inst = fg.get(@enumFromInt(param));
        fg.scratch.appendAssumeCapacity(@intFromEnum(param_inst.data.param.ty));
    }

    const return_type = try typeExpr(b, scope, function_type.@"return");
    const pl = try fg.addSlice(fg.scratch.items[scratch_top_new..]);
    const sig = try b.add(.{
        .data = .{ .function_type = .{ .params = pl, .@"return" = return_type } },
        .loc = .{ .node = node },
    });

    // const body = try block(&body_scope, &body_scope.base, function_literal.body, true);
    try blockInner(&body_scope, s, function_literal.body);
    if (!blockReturns(&body_scope)) {
        // insert implicit return
        // TODO: find the closing brace as token
        const none = try body_scope.add(.{
            .data = .{ .none = {} },
            .loc = .{ .token = undefined },
        });
        _ = try body_scope.add(.{
            .data = .{ .return_implicit = none },
            .loc = .{ .token = undefined },
        });
    }

    const body = try b.addBlockUnlinked(&body_scope, function_literal.body);
    return b.add(.{
        .data = .{ .function = .{ .signature = sig, .body = body } },
        .loc = .{ .node = node },
    });
}

fn arrayLiteral(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const array_literal = b.tree.data(node).array_literal;

    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const sl = b.tree.extraData(array_literal.elements, Node.ExtraSlice);
    const elements = b.tree.extraSlice(sl);
    try fg.scratch.ensureUnusedCapacity(fg.arena, elements.len);
    for (elements) |element| {
        const element_index = try valExpr(b, scope, element);
        fg.scratch.appendAssumeCapacity(@intFromEnum(element_index));
    }

    const pl = try fg.addSlice(fg.scratch.items[scratch_top..]);
    return b.add(.{
        .data = .{ .array = pl },
        .loc = .{ .node = node },
    });
}

fn structLiteral(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const struct_literal = b.tree.data(node).struct_literal;

    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const sl = b.tree.extraData(struct_literal.fields, Node.ExtraSlice);
    const fields = b.tree.extraSlice(sl);
    try fg.scratch.ensureUnusedCapacity(fg.arena, fields.len);
    for (fields) |field| {
        const field_data = b.tree.data(field).field_initializer;
        const field_token = b.tree.mainToken(field) + 1;
        const field_str = b.tree.tokenString(field_token);
        std.debug.print("literal field: {s}\n", .{field_str});
        const field_id = try fg.pool.getOrPutString(field_str);
        const val = try valExpr(b, scope, field_data);
        const pl = try fg.addExtra(Inst.StructFieldInitializer{
            .name = field_id,
            .val = val,
        });
        fg.scratch.appendAssumeCapacity(@intFromEnum(pl));
    }

    const pl = try fg.addSlice(fg.scratch.items[scratch_top..]);
    const struct_type = try typeExpr(b, scope, struct_literal.struct_type);
    return b.add(.{
        .data = .{ .@"struct" = .{ .ty = struct_type, .fields = pl } },
        .loc = .{ .node = node },
    });
}

fn expr(b: *Block, s: **Scope, ri: ResultInfo, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    // const scope = s.*;
    return switch (ri.semantics) {
        // anything that can be an rvalue (most things except types)
        .val => switch (b.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .char_literal => charLiteral(b, node),
            .string_literal => stringLiteral(b, node),
            .function_literal => try functionLiteral(b, s, node),
            .array_literal => try arrayLiteral(b, s, node),
            .struct_literal => try structLiteral(b, s, node),
            .ident => identExpr(b, s, ri, node),
            .call => try call(b, s, node),
            .binary => try binary(b, s, node),
            .unary => try unary(b, s, ri, node),
            .subscript => try opSubscript(b, s, ri, node),
            .member => try opMember(b, s, ri, node),
            .slice => try opSlice(b, s, node),
            .paren => try expr(b, s, ri, b.tree.data(node).paren),
            .block => {
                const block = try blockUnlinked(b, s, node);
                try b.linkInst(block);
                return block;
            },
            .if_else => try ifElse(b, s, node),
            else => {
                std.log.err("expr (val semantics): unexpected node: {}\n", .{b.tree.data(node)});
                try fg.errors.append(fg.gpa, .{
                    .tag = .invalid_val_expr,
                    .token = b.tree.mainToken(node),
                });
                return error.HandledUserError;
            },
        },
        // anything that can be an lvalue (assigned to)
        .ptr => switch (b.tree.data(node)) {
            .ident => identExpr(b, s, ri, node),
            // some types of unaries (like pointer assignment *apple = banana)
            .unary => try unary(b, s, ri, node),
            .subscript => try opSubscript(b, s, ri, node),
            .member => try opMember(b, s, ri, node),
            .paren => try expr(b, s, ri, b.tree.data(node).paren),
            else => {
                std.log.err("expr (ptr semantics): unexpected node: {}\n", .{b.tree.data(node)});
                try fg.errors.append(fg.gpa, .{
                    .tag = .invalid_ptr_expr,
                    .token = b.tree.mainToken(node),
                });
                return error.HandledUserError;
            },
        },
        // can be used in any context that `val` can, but generates ptr semantics
        .ref => switch (b.tree.data(node)) {
            .ident => identExpr(b, s, ri, node),
            .subscript => try opSubscript(b, s, ri, node),
            .member => try opMember(b, s, ri, node),
            .paren => try expr(b, s, ri, b.tree.data(node).paren),
            .unary => try unary(b, s, ri, node),
            // since temporaries can be referenced in femto, we upgrade these
            // to const pointers using a push
            .slice,
            .integer_literal,
            .float_literal,
            .bool_literal,
            .char_literal,
            .string_literal,
            .call,
            .binary,
            .function_literal,
            .array_literal,
            .block,
            .if_else,
            => {
                const inner = switch (b.tree.data(node)) {
                    .slice => try opSlice(b, s, node),
                    .integer_literal => try integerLiteral(b, node),
                    .float_literal => try floatLiteral(b, node),
                    .bool_literal => try boolLiteral(b, node),
                    .char_literal => try charLiteral(b, node),
                    .string_literal => try stringLiteral(b, node),
                    .call => try call(b, s, node),
                    .binary => try binary(b, s, node),
                    .function_literal => try functionLiteral(b, s, node),
                    .array_literal => try arrayLiteral(b, s, node),
                    .block => bl: {
                        const block = try blockUnlinked(b, s, node);
                        try b.linkInst(block);
                        break :bl block;
                    },
                    .if_else => try ifElse(b, s, node),
                    else => unreachable,
                };

                return b.add(.{
                    .data = .{ .push = inner },
                    .loc = .{ .node = node },
                });
            },
            else => {
                std.log.err("expr (ref semantics): unexpected node: {}\n", .{b.tree.data(node)});
                try fg.errors.append(fg.gpa, .{
                    .tag = .invalid_ref_expr,
                    .token = b.tree.mainToken(node),
                });
                return error.HandledUserError;
            },
        },
        .ty => switch (b.tree.data(node)) {
            .ident => identExpr(b, s, ri, node),
            .unary => try unary(b, s, ri, node),
            .pointer_type, .mut_pointer_type => try pointerType(b, s, node),
            .many_pointer_type => try manyPointerType(b, s, node),
            .array_type => try arrayType(b, s, node),
            .slice_type => try sliceType(b, s, node),
            .function_type => try functionType(b, s, node),
            .struct_type => try structType(b, s, node),
            else => {
                std.log.err("expr (type semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .none => switch (b.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .ident => identExpr(b, s, ri, node),
            .call => try call(b, s, node),
            .binary => try binary(b, s, node),
            .unary => try unary(b, s, ri, node),
            // .index => try opSubscript(b, scope, ri, node),
            // .member => try opMember(b, scope, ri, node),
            else => {
                std.log.err("expr (none semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
    };
}

inline fn valExpr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .val, .ctx = .none };
    return expr(b, s, ri, node);
}

inline fn ptrExpr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .ptr, .ctx = .none };
    return expr(b, s, ri, node);
}

inline fn refExpr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .ref, .ctx = .none };
    return expr(b, s, ri, node);
}

inline fn typeExpr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .ty, .ctx = .none };
    return expr(b, s, ri, node);
}

inline fn noneExpr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .none, .ctx = .none };
    return expr(b, s, ri, node);
}

fn pointerType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    switch (b.tree.data(node)) {
        .pointer_type => |pointee| {
            const inner = try typeExpr(b, scope, pointee);
            return b.add(.{
                .data = .{ .pointer_type = .{ .pointee = inner, .mutable = false } },
                .loc = .{ .node = node },
            });
        },
        .mut_pointer_type => |pointee| {
            const inner = try typeExpr(b, scope, pointee);
            return b.add(.{
                .data = .{ .pointer_type = .{ .pointee = inner, .mutable = true } },
                .loc = .{ .node = node },
            });
        },
        else => unreachable,
    }
}

fn manyPointerType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const pointee = b.tree.data(node).many_pointer_type;
    const inner = try typeExpr(b, scope, pointee);
    return b.add(.{
        .data = .{ .many_pointer_type = .{ .pointee = inner } },
        .loc = .{ .node = node },
    });
}

fn arrayType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const array_type = b.tree.data(node).array_type;
    const element_type = try typeExpr(b, scope, array_type.element_type);
    const count = try valExpr(b, scope, array_type.count_expr);
    // TODO: make sure count is actually a comptime_int
    return b.add(.{
        .data = .{ .array_type = .{ .element = element_type, .count = count } },
        .loc = .{ .node = node },
    });
}

fn sliceType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const element_type = b.tree.data(node).slice_type;
    const inner = try typeExpr(b, scope, element_type);
    return b.add(.{
        .data = .{ .slice_type = .{ .element = inner } },
        .loc = .{ .node = node },
    });
}

// TODO: try to merge with functionLiteral
fn functionType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const function_type = b.tree.data(node).function_type;

    const sl = b.tree.extraData(function_type.params, Node.ExtraSlice);
    const params = b.tree.extraSlice(sl);
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    try fg.scratch.ensureUnusedCapacity(fg.arena, params.len);

    for (params) |param| {
        const data = b.tree.data(param).param;
        const param_type = try typeExpr(b, scope, data);
        fg.scratch.appendAssumeCapacity(@intFromEnum(param_type));
    }

    const param_types = fg.scratch.items[scratch_top..];
    const return_type = try typeExpr(b, scope, function_type.@"return");
    const pl = try fg.addSlice(param_types);
    return b.add(.{
        .data = .{ .function_type = .{ .params = pl, .@"return" = return_type } },
        .loc = .{ .node = node },
    });
}

fn structType(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const struct_type = b.tree.data(node).struct_type;

    const sl = b.tree.extraData(struct_type.fields, Node.ExtraSlice);
    const fields = b.tree.extraSlice(sl);
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    try fg.scratch.ensureUnusedCapacity(fg.arena, fields.len);

    for (fields) |field| {
        const data = b.tree.data(field).field;
        const field_token = b.tree.mainToken(field);
        const field_name = b.tree.tokenString(field_token);
        const id = try fg.pool.getOrPutString(field_name);
        const field_type = try typeExpr(b, scope, data);
        const struct_field = try fg.addExtra(Inst.StructField{
            .name = id,
            .ty = field_type,
        });
        fg.scratch.appendAssumeCapacity(@intFromEnum(struct_field));
    }

    const struct_fields = fg.scratch.items[scratch_top..];
    const pl = try fg.addSlice(struct_fields);
    return b.add(.{
        .data = .{ .struct_type = .{ .fields = pl } },
        .loc = .{ .node = node },
    });
}

fn opSubscript(b: *Block, scope: **Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
    const subscript = b.tree.data(node).subscript;

    const operand = try expr(b, scope, .{ .semantics = ri.semantics, .ctx = .elem }, subscript.operand);
    const index = try valExpr(b, scope, subscript.index);
    switch (ri.semantics) {
        .val => return b.add(.{
            .data = .{ .index_val = .{ .base = operand, .index = index } },
            .loc = .{ .node = node },
        }),
        .ptr, .ref => return b.add(.{
            .data = .{ .index_ref = .{ .base = operand, .index = index } },
            .loc = .{ .node = node },
        }),
        .none, .ty => unreachable,
    }
}

fn opSlice(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const slice = b.tree.data(node).slice;
    const payload = b.tree.extraData(slice.range, Node.GetSlice);
    const base = try noneExpr(b, scope, slice.operand);
    const start = try valExpr(b, scope, payload.start);
    const end = try valExpr(b, scope, payload.end);

    const pl = try fg.addExtra(Fir.Inst.Slice{
        .base = base,
        .start = start,
        .end = end,
    });
    return b.add(.{
        .data = .{ .slice = .{ .pl = pl } },
        .loc = .{ .node = node },
    });

    // switch (ri.semantics) {
    //     .val => return b.add(.load, .{ .operand = ptr, .node = node }),
    //     .ref => return ptr,
    //     .ty, .any => unreachable,
    // }
}

fn opMember(b: *Block, scope: **Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const field_val = b.tree.data(node).member;
    const field_token = b.tree.mainToken(node) + 1;
    const field_name = b.tree.tokenString(field_token);
    const id = try fg.pool.getOrPutString(field_name);

    const operand = try expr(b, scope, .{ .semantics = ri.semantics, .ctx = .elem }, field_val);
    switch (ri.semantics) {
        .val => return b.add(.{
            .data = .{ .field_val = .{ .base = operand, .field = id } },
            .loc = .{ .node = node },
        }),
        .ptr, .ref => return b.add(.{
            .data = .{ .field_ref = .{ .base = operand, .field = id } },
            .loc = .{ .node = node },
        }),
        .none, .ty => unreachable,
    }
}

// this function can modify the scope passed in if it encounters a declaration
fn statement(b: *Block, s: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;

    return try switch (b.tree.data(node)) {
        .const_decl => {
            const ret = try constDecl(b, s, node);
            const ident = b.tree.tokenString(b.tree.mainToken(node) + 1);
            const id = try fg.pool.getOrPutString(ident);
            const val_scope = try fg.arena.create(Scope.LocalVal);
            val_scope.* = Scope.LocalVal.init(s.*, id, ret);
            s.* = &val_scope.base;

            _ = try b.add(.{
                .data = .{ .dbg_var_val = .{
                    .name = id,
                    .val = ret,
                } },
                .loc = .{ .node = node },
            });
            return ret;
        },
        .const_decl_attr => {
            const ret = try constDeclAttr(b, s, node);
            const ident = b.tree.tokenString(b.tree.mainToken(node) + 1);
            const id = try fg.pool.getOrPutString(ident);
            const val_scope = try fg.arena.create(Scope.LocalVal);
            val_scope.* = Scope.LocalVal.init(s.*, id, ret);
            s.* = &val_scope.base;

            _ = try b.add(.{
                .data = .{ .dbg_var_val = .{
                    .name = id,
                    .val = ret,
                } },
                .loc = .{ .node = node },
            });
            return ret;
        },
        .var_decl => {
            const ret = try varDecl(b, s, node);
            const ident = b.tree.tokenString(b.tree.mainToken(node) + 2);
            const id = try fg.pool.getOrPutString(ident);
            const ptr_scope = try fg.arena.create(Scope.LocalPtr);
            ptr_scope.* = Scope.LocalPtr.init(s.*, id, ret);
            s.* = &ptr_scope.base;

            _ = try b.add(.{
                .data = .{ .dbg_var_ptr = .{
                    .name = id,
                    .ptr = ret,
                } },
                .loc = .{ .node = node },
            });
            return ret;
        },
        .type_decl => |ty| {
            const ret = try typeExpr(b, s, ty);
            const ident = b.tree.tokenString(b.tree.mainToken(node) + 1);
            const id = try fg.pool.getOrPutString(ident);
            const type_scope = try fg.arena.create(Scope.LocalType);
            type_scope.* = Scope.LocalType.init(s.*, id, ret);
            s.* = &type_scope.base;
            return ret;
        },
        .assign_simple => assignSimple(b, s, node),
        .assign_binary => assignBinary(b, s, node),
        .if_simple => ifSimple(b, s, node),
        .if_else => ifElse(b, s, node),
        .if_chain => ifChain(b, s, node),
        .return_val => returnStmt(b, s, node),
        .yield_val => yield(b, s, node),
        .loop_forever => loopForever(b, s, node),
        .loop_conditional => loopConditional(b, s, node),
        .loop_range => loopRange(b, s, node),
        .@"break" => loopBreak(b, s, node),
        .call => call(b, s, node),
        else => {
            std.log.err("statement: unexpected node: {}\n", .{b.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn globalStatement(fg: *FirGen, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const data = fg.tree.data(node);
    return try switch (data) {
        .const_decl => globalConst(fg, scope, node),
        .const_decl_attr => globalConstAttr(fg, scope, node),
        // .type_decl => typeDecl(fg, scope, node),
        .var_decl => globalVar(fg, scope, node),
        else => {
            std.log.err("global statement: unexpected node: {}\n", .{fg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn call(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const call_expr = b.tree.data(node).call;
    // TODO: probably use ref semantics
    const ri: ResultInfo = .{ .semantics = .ptr, .ctx = .none };
    const ptr = try identExpr(b, scope, ri, call_expr.ptr);

    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);

    const sl = b.tree.extraData(call_expr.args, Node.ExtraSlice);
    const arg_nodes = b.tree.extraSlice(sl);
    try fg.scratch.ensureUnusedCapacity(b.fg.arena, arg_nodes.len);
    for (arg_nodes, 0..) |arg_node, i| {
        const arg_inner = try valExpr(b, scope, arg_node);
        const param_index: u32 = @intCast(i);
        const param_type = try b.add(.{
            .data = .{ .param_type = .{ .function = ptr, .index = param_index } },
            .loc = .{ .node = arg_node },
        });
        const arg = try coerce(b, scope, arg_inner, param_type, arg_node);
        fg.scratch.appendAssumeCapacity(@intFromEnum(arg));
    }

    const args = fg.scratch.items[scratch_top..];
    const pl = try b.fg.addSlice(args);
    return b.add(.{
        .data = .{ .call = .{ .function = ptr, .args = pl } },
        .loc = .{ .node = node },
    });
}

fn blockInner(b: *Block, scope: **Scope, node: Node.Index) Error!void {
    const fg = b.fg;
    const data = b.tree.data(node).block;

    var s: **Scope = scope;
    const sl = b.tree.extraData(data.stmts, Node.ExtraSlice);
    const stmts = b.tree.extraSlice(sl);

    const open = b.tree.mainToken(node);
    const dbg_block_begin = try b.reserveLoc(.dbg_block_begin, .{ .token = open });
    try fg.lazy_src_insts.append(fg.arena, dbg_block_begin);

    for (stmts) |stmt| {
        _ = try statement(b, s, stmt);
    }

    const close = b.tree.locateClosingBrace(open);
    const dbg_block_end = try b.reserveLoc(.dbg_block_end, .{ .token = close });
    try fg.lazy_src_insts.append(fg.arena, dbg_block_end);
}

fn blockUnlinked(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    var inner = Block.init(b.fg, scope.*);
    defer inner.deinit();
    var s = &inner.base;

    try blockInner(&inner, &s, node);
    return b.addBlockUnlinked(&inner, node);
}

fn coerce(b: *Block, s: **Scope, val: Fir.Index, dest_ty: Fir.Index, node: Node.Index) !Fir.Index {
    _ = s;
    // generate a "type validation" marker instruction
    // this is a passthrough which takes in the above value reference
    // and the type reference and returns the value reference
    // semantic analysis will validate that the type is as it should be
    // and then remove this instruction in the mir
    return b.add(.{
        .data = .{ .coerce = .{ .src = val, .ty = dest_ty } },
        .loc = .{ .node = node },
    });
}

fn constDecl(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope (by caller) so that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const const_decl = b.tree.data(node).const_decl;
    return constDeclInner(b, s, const_decl.val, const_decl.ty, node);
}

fn constDeclAttr(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope such that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const const_decl = b.tree.data(node).const_decl_attr;
    const metadata = b.tree.extraData(const_decl.metadata, Node.DeclMetadata);
    return constDeclInner(b, s, const_decl.val, metadata.ty, node);
}

fn constDeclInner(b: *Block, s: **Scope, val: Node.Index, ty: Node.Index, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const ident_index = b.tree.mainToken(node) + 1;
    const ident_str = b.tree.tokenString(ident_index);
    // const id = try fg.pool.getOrPutString(ident_str);
    if (builtin_types.has(ident_str)) {
        try fg.errors.append(fg.gpa, .{
            .tag = .shadows_builtin_type,
            .token = ident_index,
        });
        return error.HandledUserError;
    }

    const inner = try valExpr(b, s, val);
    // _ = try b.addDebugValue(ref, id, node);

    if (ty == 0) {
        // untyped (inferred) declaration
        return inner;
    } else {
        const dest_ty = try typeExpr(b, s, ty);
        return coerce(b, s, inner, dest_ty, node);
    }
}

fn varDecl(b: *Block, s: **Scope, node: Node.Index) !Fir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.tree.data(node).var_decl;
    var val = try expr(b, s, .{ .semantics = .val, .ctx = .store }, var_decl.val);
    if (var_decl.ty != 0) {
        // type annotated declaration
        const dest_ty = try typeExpr(b, s, var_decl.ty);
        val = try coerce(b, s, val, dest_ty, node);
    }

    const push = try b.add(.{
        .data = .{ .push_mut = val },
        .loc = .{ .node = node },
    });
    return push;
}

fn globalConst(fg: *FirGen, s: **Scope, node: Node.Index) !Fir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    const const_decl = fg.tree.data(node).const_decl;

    var b = Block.init(fg, s.*);
    defer b.deinit();
    var scope = &b.base;

    const handle = try b.add(.{
        .data = .{ .global_handle = {} },
        .loc = .{ .node = node },
    });

    var rvalue = try valExpr(&b, &scope, const_decl.val);
    if (const_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&b, &scope, const_decl.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        rvalue = try coerce(&b, &scope, rvalue, type_annotation, node);
    } else {
        // generate a type inference, since global decls can't
        // postpone the problem like locals can
        const type_inference = try b.add(.{
            .data = .{ .type_of = rvalue },
            .loc = .{ .node = node },
        });
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_inference } },
            .loc = .{ .node = node },
        });
    }

    _ = try b.add(.{
        .data = .{ .global_set_init = .{ .handle = handle, .val = rvalue } },
        .loc = .{ .node = node },
    });
    _ = try b.add(.{
        .data = .{ .yield_inline = handle },
        .loc = .{ .node = node },
    });
    return Block.addBlockInlineUnlinked(fg, &b, node);
}

fn globalConstAttr(fg: *FirGen, s: **Scope, node: Node.Index) !Fir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    // this version supports attributes

    const const_decl = fg.tree.data(node).const_decl_attr;
    const metadata = fg.tree.extraData(const_decl.metadata, Node.DeclMetadata);

    var b = Block.init(fg, s.*);
    defer b.deinit();
    var scope = &b.base;

    const handle = try b.add(.{
        .data = .{ .global_handle = {} },
        .loc = .{ .node = node },
    });

    const attrs = b.tree.extra_data[metadata.attrs_start..metadata.attrs_end];

    var has_initializer = true;
    for (attrs) |attr| {
        switch (b.tree.tokenTag(b.tree.mainToken(attr))) {
            .a_export => _ = try b.add(.{
                .data = .{ .global_set_linkage_external = handle },
                .loc = .{ .node = node },
            }),
            .a_import => {
                _ = try b.add(.{
                    .data = .{ .global_set_linkage_external = handle },
                    .loc = .{ .node = node },
                });
                has_initializer = false;
            },
            else => unreachable,
        }
    }

    var rvalue: ?Fir.Index = null;
    if (has_initializer) {
        std.debug.assert(const_decl.val != 0);
        rvalue = try valExpr(&b, &scope, const_decl.val);
    } else {
        std.debug.assert(const_decl.val == 0);
        std.debug.assert(metadata.ty != 0); // this should be a handled user error
    }

    if (metadata.ty != 0) {
        const type_annotation = try typeExpr(&b, &scope, metadata.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        if (rvalue) |val| {
            rvalue = try coerce(&b, &scope, val, type_annotation, node);
        }
    } else {
        // generate a type inference, since global decls can't
        // postpone the problem like locals can
        const type_inference = try b.add(.{
            .data = .{ .type_of = rvalue.? },
            .loc = .{ .node = node },
        });
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_inference } },
            .loc = .{ .node = node },
        });
    }

    if (rvalue) |val| {
        _ = try b.add(.{
            .data = .{ .global_set_init = .{ .handle = handle, .val = val } },
            .loc = .{ .node = node },
        });
    }

    _ = try b.add(.{
        .data = .{ .yield_inline = handle },
        .loc = .{ .node = node },
    });
    return Block.addBlockInlineUnlinked(fg, &b, node);
}

fn globalVar(fg: *FirGen, s: **Scope, node: Node.Index) !Fir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = fg.tree.data(node).var_decl;

    var b = Block.init(fg, s.*);
    defer b.deinit();
    var scope = &b.base;

    const handle = try b.add(.{
        .data = .{ .global_handle = {} },
        .loc = .{ .node = node },
    });
    _ = try b.add(.{
        .data = .{ .global_set_mutable = handle },
        .loc = .{ .node = node },
    });

    var rvalue = try valExpr(&b, &scope, var_decl.val);
    if (var_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&b, &scope, var_decl.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        rvalue = try coerce(&b, &scope, rvalue, type_annotation, node);
        _ = try b.add(.{
            .data = .{ .global_set_init = .{ .handle = handle, .val = rvalue } },
            .loc = .{ .node = node },
        });
    }

    _ = try b.add(.{
        .data = .{ .yield_inline = handle },
        .loc = .{ .node = node },
    });
    return Block.addBlockInlineUnlinked(fg, &b, node);
}

fn assignSimple(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const assign = b.tree.data(node).assign_simple;

    const val = try valExpr(b, scope, assign.val);
    const ptr = try ptrExpr(b, scope, assign.ptr);
    return b.add(.{
        .data = .{ .store = .{ .ptr = ptr, .val = val } },
        .loc = .{ .node = node },
    });
}

fn assignBinary(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const assign = b.tree.data(node).assign_binary;
    const op = b.tree.mainToken(node);

    const ptr = try ptrExpr(b, scope, assign.ptr);
    const base = try valExpr(b, scope, assign.ptr);
    const val = try valExpr(b, scope, assign.val);
    const bin = try binaryInner(b, node, op, base, val);

    return b.add(.{
        .data = .{ .store = .{ .ptr = ptr, .val = bin } },
        .loc = .{ .node = node },
    });
}

fn ifSimple(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const if_simple = b.tree.data(node).if_simple;

    const condition = try valExpr(b, scope, if_simple.condition);
    const exec_true = try blockUnlinked(b, scope, if_simple.exec_true);
    return b.add(.{
        .data = .{ .branch_single = .{ .cond = condition, .exec_true = exec_true } },
        .loc = .{ .node = node },
    });
}

fn ifElse(b: *Block, scope: **Scope, node: Node.Index) Error!Fir.Index {
    const if_else = b.tree.data(node).if_else;

    const condition = try valExpr(b, scope, if_else.condition);
    const exec = b.tree.extraData(if_else.exec, Node.IfElse);
    const exec_true = try blockUnlinked(b, scope, exec.exec_true);
    const exec_false = try blockUnlinked(b, scope, exec.exec_false);
    return b.addBranchDouble(condition, exec_true, exec_false, node);
}

fn ifChain(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const if_chain = b.tree.data(node).if_chain;
    const chain = b.tree.extraData(if_chain.chain, Node.IfChain);

    const condition = try valExpr(b, scope, if_chain.condition);
    const exec_true = try blockUnlinked(b, scope, chain.exec_true);
    const next = block: {
        var block_scope = Block.init(b.fg, scope.*);
        defer block_scope.deinit();
        var s = scope.*;

        _ = switch (b.tree.data(chain.next)) {
            .if_simple => try ifSimple(&block_scope, &s, chain.next),
            .if_else => try ifElse(&block_scope, &s, chain.next),
            else => unreachable,
        };
        break :block try b.addBlockUnlinked(&block_scope, node);
    };

    return b.addBranchDouble(condition, exec_true, next, node);
}

fn returnStmt(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const return_val = b.tree.data(node).return_val;
    const return_type = try b.add(.{
        .data = .{ .return_type = {} },
        .loc = .{ .node = node },
    });
    const operand = if (return_val == 0) try b.add(.{
        .data = .{ .none = {} },
        .loc = .{ .node = node },
    }) else op: {
        const ref = try valExpr(b, scope, return_val);
        break :op try coerce(b, scope, ref, return_type, node);
    };

    return b.add(.{
        .data = .{ .return_node = operand },
        .loc = .{ .node = node },
    });
}

fn yield(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const yield_val = b.tree.data(node).yield_val;
    const operand = try valExpr(b, scope, yield_val);

    return b.add(.{
        .data = .{ .yield_node = operand },
        .loc = .{ .node = node },
    });
}

fn wrapExpression(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    var inner = Block.init(b.fg, scope.*);
    defer inner.deinit();
    var s = &inner.base;

    const condition = try valExpr(&inner, &s, node);
    _ = try inner.add(.{
        .data = .{ .yield_implicit = condition },
        .loc = .{ .token = undefined },
    });
    return b.addBlockUnlinked(&inner, node);
}

fn wrapInst(b: *Block, scope: **Scope, inst: Fir.Index, node: Node.Index) !Fir.Index {
    var inner = Block.init(b.fg, scope.*);
    defer inner.deinit();

    try inner.linkInst(inst);
    _ = try inner.add(.{
        .data = .{ .yield_implicit = inst },
        .loc = .{ .token = undefined },
    });
    return b.addBlockUnlinked(&inner, node);
}

fn loopForever(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const loop_forever = b.tree.data(node).loop_forever;

    const body = try blockUnlinked(b, scope, loop_forever.body);
    return b.add(.{
        .data = .{ .loop_forever = .{ .body = body } },
        .loc = .{ .node = node },
    });
}

fn loopConditional(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const loop_conditional = b.tree.data(node).loop_conditional;

    const condition = try wrapExpression(b, scope, loop_conditional.condition);
    const body = try blockUnlinked(b, scope, loop_conditional.body);
    return b.addLoopWhile(condition, body, node);
}

fn loopRange(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    const loop_range = b.tree.data(node).loop_range;
    const signature = b.tree.extraData(loop_range.signature, Node.RangeSignature);

    var s = scope.*;
    _ = try statement(b, &s, signature.binding);
    const condition = try wrapExpression(b, &s, signature.condition);

    var inner = Block.init(b.fg, s);
    defer inner.deinit();
    s = &inner.base;

    try blockInner(&inner, &s, loop_range.body);
    _ = try statement(&inner, &s, signature.afterthought);
    const body = try b.addBlockUnlinked(&inner, node);
    return b.addLoopWhile(condition, body, node);
}

fn loopBreak(b: *Block, scope: **Scope, node: Node.Index) !Fir.Index {
    _ = scope;
    return b.add(.{
        .data = .{ .@"break" = {} },
        .loc = .{ .node = node },
    });
}

fn globalIdentId(fg: *FirGen, stmt: u32) !InternPool.StringIndex {
    const ident = switch (fg.tree.data(stmt)) {
        .const_decl, .const_decl_attr, .type_decl => fg.tree.tokenString(fg.tree.mainToken(stmt) + 1),
        .var_decl => fg.tree.tokenString(fg.tree.mainToken(stmt) + 2),
        else => unreachable,
    };

    return fg.pool.getOrPutString(ident);
}

fn instructionReturns(fg: *FirGen, inst: Fir.Index) bool {
    var i = @intFromEnum(inst);
    // skip epilogue
    while (i > 0) : (i -= 1) {
        switch (fg.insts.get(i)) {
            .dbg_block_end => {},
            else => break,
        }
    }

    switch (fg.insts.get(i)) {
        .block => |block| {
            const slice = fg.extraData(Fir.Inst.ExtraSlice, block.insts);
            const insts = fg.extraSlice(slice);
            if (insts.len == 0) return false;
            return instructionReturns(fg, @enumFromInt(insts[insts.len - 1]));
        },
        .branch_single => return false,
        .branch_double => |branch_double| {
            const data = fg.extraData(Fir.Inst.BranchDouble, branch_double.pl);
            const lret = instructionReturns(fg, data.exec_true);
            const rret = instructionReturns(fg, data.exec_false);
            return lret and rret;
        },
        .return_implicit, .return_node => return true,
        else => return false,
    }
}

fn blockReturns(b: *Block) bool {
    const inst = b.insts.items[b.insts.items.len - 1];
    return instructionReturns(b.fg, inst);
}
