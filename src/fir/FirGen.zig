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
    };

    // post order format guarantees that the module node will be the last
    const module_node: u32 = @intCast(tree.nodes.len - 1);
    const module_index = try fg.module(module_node);

    return .{
        .tree = tree,
        .insts = fg.insts.toOwnedSlice(),
        .locs = try fg.locs.toOwnedSlice(gpa),
        .extra = try fg.extra.toOwnedSlice(gpa),
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

    const Semantics = enum {
        val,
        ref,
        ty,
        any,
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
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @bitCast(fg.extra.items[index + i]),
        }
    }
    return result;
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

fn module(fg: *FirGen, node: Node.Index) !Fir.Index {
    const data = fg.tree.data(node).module;

    var module_scope: Scope.Module = .{};
    var namespace = Scope.Namespace.init(&module_scope.base);

    // first pass through statements - "forward declare" all identifiers
    // in the toplevel namespace so that forward-referencing and recursive
    // function calls resolve
    // note that since we link to the node id, we can resolve identifiers
    // that haven't been irgen'ed yet
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    var stmts = fg.tree.extra_data[data.stmts_start..data.stmts_end];
    try fg.scratch.ensureUnusedCapacity(fg.arena, stmts.len);

    for (stmts) |stmt| {
        const id = try fg.globalIdentId(stmt);
        try namespace.decls.put(fg.arena, id, {});
    }

    // second pass - generate the value/expression and update the resolution map
    // so we can link node ids to instructions
    for (stmts) |stmt| {
        const id = try fg.globalIdentId(stmt);
        const inst = try globalStatement(fg, &namespace.base, stmt);
        const global = try fg.add(.{
            .data = .{ .global = .{ .name = id, .block = inst } },
            .loc = .{ .node = stmt },
        });
        fg.scratch.appendAssumeCapacity(@intFromEnum(global));
        // const id = try globalIdentId(fg, stmt);
        // switch (fg.tree.data(stmt)) {
        //     .const_decl, .const_decl_attr, .var_decl => {
        //         _ = id;
        //         _ = inst;
        //         // try fg.untyped_decls.put(fg.gpa, id, inst);
        //     },
        //     else => {
        //         std.log.err("module: unexpected node: {}\n", .{fg.tree.data(node)});
        //         return GenError.NotImplemented;
        //     },
        // }
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
    return b.add(.{
        .data = .{ .int = int },
        .loc = .{ .node = node },
    });
}

fn floatLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const float_token = b.tree.mainToken(node);
    const float_str = b.tree.tokenString(float_token);
    const float = try parseFloat(float_str);
    return b.add(.{
        .data = .{ .float = float },
        .loc = .{ .node = node },
    });
}

fn boolLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const bool_token = b.tree.mainToken(node);
    const value: u32 = switch (b.tree.tokenTag(bool_token)) {
        .k_true => 1,
        .k_false => 0,
        else => unreachable,
    };
    return b.add(.{
        .data = .{ .int = value },
        .loc = .{ .node = node },
    });
}

fn charLiteral(b: *Block, node: Node.Index) !Fir.Index {
    const char_token = b.tree.mainToken(node);
    const char_str = b.tree.tokenString(char_token);
    return b.add(.{
        .data = .{ .int = char_str[1] },
        .loc = .{ .node = node },
    });
}

// fn stringLiteral(b: *Block, node: Node.Index) !Fir.Index {
//     const hg = b.hg;
//     const string_token = hg.tree.mainToken(node);
//     const string_text = hg.tree.tokenString(string_token);
//     const string_literal = string_text[1 .. string_text.len - 1];
//     const id = try hg.interner.intern(string_literal);
//
//     const len: u32 = @intCast(string_literal.len);
//     const buffer_type: Type = .{ .basic = .{ .kind = Type.Kind.string, .width = 0 } };
//     const buffer_value = try hg.pool.internValue(.{ .string = id });
//
//     // underlying string buffer, gets emitted to .rodata as a global
//     _ = try b.add(.constant, .{
//         .ty = try b.add(.ty, .{ .ty = buffer_type }),
//         .val = buffer_value,
//         .node = node,
//     });
//
//     const len_value = try hg.pool.internValue(.{ .integer = len });
//     const slice = try Value.createSlice(hg.pool.arena, buffer_value, len_value);
//     const slice_value = try hg.pool.internValue(slice);
//     const string_type = try Type.Slice.init(hg.gpa, Type.Common.u8_type);
//
//     return b.add(.constant, .{
//         .ty = try b.add(.ty, .{ .ty = string_type }),
//         .val = slice_value,
//         .node = node,
//     });
//
//     // const inner_slice = try hg.gpa.create(Value.Slice);
//     // inner_slice.* = .{ .ptr = string, .len = string_literal.len };
//     // const slice = Value{ .extended = &inner_slice.base };
//     // // comptime slice to that string
//     // return try b.add(.constant, .{
//     //     .ty = try b.add(.ty, .{ .ty = string_type }),
//     //     .val = try b.addValue(slice),
//     //     .node = node,
//     // });
// }

fn identExpr(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const ident_token = b.tree.mainToken(node);
    const ident_str = b.tree.tokenString(ident_token);

    // check identifier against builtin types
    if (builtin_types.has(ident_str)) {
        switch (ri.semantics) {
            .ty => return b.add(.{
                .data = .{ .builtin_type = {} },
                .loc = .{ .node = node },
            }),
            else => {
                // try fg.errors.append(fg.gpa, .{
                //     .tag = .invalid_identifier,
                //     .token = ident_token,
                // });
                return error.HandledUserError;
            },
        }
    }

    const id = try fg.pool.getOrPutString(ident_str);
    const ident_scope = (try scope.resolveIdent(id)) orelse {
        // try fg.errors.append(fg.gpa, .{
        //     .tag = .invalid_identifier,
        //     .token = ident_token,
        // });
        return error.HandledUserError;
    };

    switch (ident_scope.tag) {
        // for constants, the ref points to the instruction that returned its value
        // so we need to check that this is an rvalue context
        // we can't use consts in an lvalue context, aka on the left side of an assignment
        .local_val => {
            switch (ri.semantics) {
                .val, .any => return ident_scope.cast(Scope.LocalVal).?.inst,
                .ref => {
                    // TODO: this could be a nicer constant assignment error
                    // especially since we want to upgrade these to const ptrs
                    try fg.errors.append(fg.gpa, .{
                        .tag = .invalid_lvalue,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
                .ty => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .invalid_type,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        .local_ptr => {
            const local_ptr = ident_scope.cast(Scope.LocalPtr).?;
            switch (ri.semantics) {
                // but for mutable variables, the ref points to the address
                // in memory so we have to generate a load instruction to
                // get the value at that address
                .val => return b.add(.{
                    .data = .{ .load = .{ .ptr = local_ptr.ptr } },
                    .loc = .{ .node = node },
                }),
                .ref, .any => return local_ptr.ptr,
                .ty => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .invalid_type,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        .local_type => {
            switch (ri.semantics) {
                .ty, .any => return ident_scope.cast(Scope.LocalType).?.inst,
                .val, .ref => {
                    try fg.errors.append(fg.gpa, .{
                        .tag = .invalid_lvalue,
                        .token = ident_token,
                    });
                    return error.InvalidLvalue;
                },
            }
        },
        .namespace => {
            // TODO: semantics for this
            return b.add(.{
                .data = .{ .load_global = .{ .name = id } },
                .loc = .{ .node = node },
            });
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
        => |t| {
            return b.add(.{
                .data = @unionInit(Inst.Data, @tagName(t), .{ .l = l, .r = r }),
                .loc = .{ .node = node },
            });
        },
        else => unreachable,
    }
}

fn binary(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const operator_token = b.tree.mainToken(node);
    const tag = b.tree.tokenTag(operator_token);
    _ = tag;
    // TODO: binary short circuiting
    const binary_expr = b.tree.data(node).binary;
    const left = try valExpr(b, scope, binary_expr.left);
    const right = try valExpr(b, scope, binary_expr.right);
    return binaryInner(b, node, operator_token, left, right);
}

fn unary(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
    const unary_expr = b.tree.data(node).unary;
    const operator_token = b.tree.mainToken(node);

    switch (b.tree.tokenTag(operator_token)) {
        // calculate a reference to the pointer and return it
        .ampersand => return refExpr(b, scope, unary_expr),
        .asterisk => {
            const ptr = try valExpr(b, scope, unary_expr);
            switch (ri.semantics) {
                .val => return b.add(.{
                    .data = .{ .load = .{ .ptr = ptr } },
                    .loc = .{ .node = node },
                }),
                .ref, .any => return ptr,
                .ty => unreachable,
            }
        },
        // nop: +x is just x
        // TODO: consider removing this syntax entirely
        .plus => return expr(b, scope, ri, unary_expr),
        .minus => {
            const operand = try valExpr(b, scope, unary_expr);
            return b.add(.{
                .data = .{ .neg = operand },
                .loc = .{ .node = node },
            });
        },
        .bang => {
            // lowers to a bool comparison to 0
            // just coerce this to a u1 so we know we're operating on a bool
            const inner = try valExpr(b, scope, unary_expr);
            const bool_type = try b.add(.{
                .data = .{ .bool_type = {} },
                .loc = .{ .node = node },
            });
            const operand = try coerce(b, scope, inner, bool_type, node);
            const other = try b.add(.{
                .data = .{ .int = 0 },
                .loc = .{ .node = node },
            });
            return b.add(.{
                .data = .{ .cmp_eq = .{ .l = operand, .r = other } },
                .loc = .{ .node = node },
            });
        },
        .tilde => {
            const operand = try valExpr(b, scope, unary_expr);
            return b.add(.{
                .data = .{ .bitwise_inv = operand },
                .loc = .{ .node = node },
            });
        },
        else => {
            std.log.err("unary: unexpected token: {}\n", .{b.tree.tokenTag(operator_token)});
            return error.UnexpectedToken;
        },
    }
}

fn fnDecl(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const fn_decl = b.tree.data(node).fn_decl;
    const signature = b.tree.extraData(fn_decl.signature, Node.FnSignature);

    // used to store parameters temporarily
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const params = b.tree.extra_data[signature.params_start..signature.params_end];
    try fg.scratch.ensureUnusedCapacity(fg.arena, params.len);

    var s: *Scope = scope;
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
        param_scope.* = Scope.LocalVal.init(s, param_id, ref);
        s = &param_scope.base;
    }

    var body_scope = Block.init(b.fg, s);
    defer body_scope.deinit();

    const fir_params = fg.scratch.items[scratch_top..];
    const scratch_top_new = fg.scratch.items.len;
    try fg.scratch.ensureUnusedCapacity(fg.arena, fir_params.len);
    for (fir_params) |param| {
        try body_scope.linkInst(@enumFromInt(param));
    }

    const return_type = try typeExpr(b, scope, signature.return_ty);
    const pl = try fg.addSlice(fg.scratch.items[scratch_top_new..]);
    const sig = try b.add(.{
        .data = .{ .function_type = .{ .params = pl, .@"return" = return_type } },
        .loc = .{ .node = node },
    });

    const body = try block(&body_scope, &body_scope.base, fn_decl.body, true);
    return b.add(.{
        .data = .{ .function = .{ .signature = sig, .body = body } },
        .loc = .{ .node = node },
    });
}

fn arrayInit(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const array_init = b.tree.data(node).array_init;

    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    const elements = b.tree.extra_data[array_init.elements_start..array_init.elements_end];
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

fn expr(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) !Fir.Index {
    return switch (ri.semantics) {
        .val => switch (b.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .char_literal => charLiteral(b, node),
            // .string_literal => stringLiteral(b, node),
            .ident => identExpr(b, scope, ri, node),
            .call => try call(b, scope, node),
            .binary => try binary(b, scope, node),
            .unary => try unary(b, scope, ri, node),
            .fn_decl => try fnDecl(b, scope, node),
            .array_init => try arrayInit(b, scope, node),
            .index => try indexAccess(b, scope, ri, node),
            // .field => try fieldAccess(b, scope, ri, node),
            // .get_slice => try getSlice(b, scope, node),
            .paren => try expr(b, scope, ri, b.tree.data(node).paren),
            .block => {
                var block_scope = Block.init(b.fg, scope);
                defer block_scope.deinit();
                const bl = try block(&block_scope, &block_scope.base, node, true);
                try b.linkInst(bl);
                return bl;
            },
            .if_else => try ifElse(b, scope, node),
            else => {
                std.log.err("expr (val semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .ref => switch (b.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .ident => identExpr(b, scope, ri, node),
            .call => try call(b, scope, node),
            .binary => try binary(b, scope, node),
            .unary => try unary(b, scope, ri, node),
            .index => try indexAccess(b, scope, ri, node),
            // .field => try fieldAccess(b, scope, ri, node),
            .paren => try expr(b, scope, ri, b.tree.data(node).paren),
            else => {
                std.log.err("expr (ref semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .ty => switch (b.tree.data(node)) {
            .ident => identExpr(b, scope, ri, node),
            .unary => try unary(b, scope, ri, node),
            .pointer => try pointerType(b, scope, node),
            .many_pointer => try manyPointerType(b, scope, node),
            .array => try arrayType(b, scope, node),
            .function => try fnType(b, scope, node),
            else => {
                std.log.err("expr (type semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
        .any => switch (b.tree.data(node)) {
            .integer_literal => integerLiteral(b, node),
            .float_literal => floatLiteral(b, node),
            .bool_literal => boolLiteral(b, node),
            .ident => identExpr(b, scope, ri, node),
            .call => try call(b, scope, node),
            .binary => try binary(b, scope, node),
            .unary => try unary(b, scope, ri, node),
            // .index => try indexAccess(b, scope, ri, node),
            // .field => try fieldAccess(b, scope, ri, node),
            else => {
                std.log.err("expr (any semantics): unexpected node: {}\n", .{b.tree.data(node)});
                return GenError.NotImplemented;
            },
        },
    };
}

inline fn valExpr(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .val };
    return expr(b, scope, ri, node);
}

inline fn refExpr(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .ref };
    return expr(b, scope, ri, node);
}

inline fn typeExpr(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .ty };
    return expr(b, scope, ri, node);
}

inline fn anyExpr(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const ri: ResultInfo = .{ .semantics = .any };
    return expr(b, scope, ri, node);
}

fn pointerType(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const pointee = b.tree.data(node).pointer;
    const inner = try typeExpr(b, scope, pointee);
    return b.add(.{
        .data = .{ .pointer_type = .{ .pointee = inner } },
        .loc = .{ .node = node },
    });
}

fn manyPointerType(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const pointee = b.tree.data(node).many_pointer;
    const inner = try typeExpr(b, scope, pointee);
    return b.add(.{
        .data = .{ .many_pointer_type = .{ .pointee = inner } },
        .loc = .{ .node = node },
    });
}

fn sliceType(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const element_type = b.tree.data(node).slice;
    const inner = try typeExpr(b, scope, element_type);
    return b.add(.{
        .data = .{ .slice_type = .{ .element = inner } },
        .loc = .{ .node = node },
    });
}

// TODO: merge with fnDecl
fn fnType(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const fn_type = b.tree.data(node).function;
    const signature = b.tree.extraData(fn_type, Node.FnSignature);

    const params = b.tree.extra_data[signature.params_start..signature.params_end];
    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);
    try fg.scratch.ensureUnusedCapacity(fg.arena, params.len);
    for (params) |param| {
        const data = b.tree.data(param).param;
        const param_type = try typeExpr(b, scope, data);
        fg.scratch.appendAssumeCapacity(@intFromEnum(param_type));
    }

    const param_types = fg.scratch.items[scratch_top..];
    const return_type = try typeExpr(b, scope, signature.return_ty);
    const pl = try fg.addSlice(param_types);
    return b.add(.{
        .data = .{ .function_type = .{ .params = pl, .@"return" = return_type } },
        .loc = .{ .node = node },
    });
}

fn arrayType(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const array_type = b.tree.data(node).array;
    const element_type = try typeExpr(b, scope, array_type.element_type);
    const count = try valExpr(b, scope, array_type.count_expr);
    // TODO: make sure count is actually a comptime_int
    return b.add(.{
        .data = .{ .array_type = .{ .element = element_type, .count = count } },
        .loc = .{ .node = node },
    });
}

fn indexAccess(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
    const index_access = b.tree.data(node).index;

    const operand = try anyExpr(b, scope, index_access.operand);
    const index = access: {
        const inner = try valExpr(b, scope, index_access.index);
        // TODO: actual variable length usize
        // TODO: add back the coerce
        // const usize_type = try b.add(.ty, .{ .ty = Type.Common.u64_type });
        // break :access try b.add(.coerce, .{
        //     .ty = usize_type,
        //     .val = inner,
        //     .node = index_access.index,
        // });
        break :access inner;
    };
    switch (ri.semantics) {
        .val => return b.add(.{
            .data = .{ .index_val = .{ .base = operand, .index = index } },
            .loc = .{ .node = node },
        }),
        .ref => return b.add(.{
            .data = .{ .index_ref = .{ .base = operand, .index = index } },
            .loc = .{ .node = node },
        }),
        .any, .ty => unreachable,
    }
}

// fn getSlice(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
//     const fg = b.fg;
//     const get_slice = b.tree.data(node).get_slice;
//     const payload = b.tree.extraData(get_slice.range, Node.GetSlice);
//     const array = try anyExpr(b, scope, get_slice.operand);
//     const start = try valExpr(b, scope, payload.start);
//     const end = try valExpr(b, scope, payload.end);
//     // TODO: actual variable length usize
//     const usize_type = try b.add(.ty, .{ .ty = Type.Common.u64_type });
//     const start_index = try b.add(.coerce, .{
//         .ty = usize_type,
//         .val = start,
//         .node = payload.start,
//     });
//     const end_index = try b.add(.coerce, .{
//         .ty = usize_type,
//         .val = end,
//         .node = payload.end,
//     });
//     const type_of = try b.add(.type_of, .{ .operand = array, .node = node });
//     const element_type = try b.add(.element_type, .{ .operand = type_of, .node = node });
//     const len = try b.add(.sub, .{ .lref = end_index, .rref = start_index, .node = node });
//     return try b.add(.slice_init, .{
//         .element_type = element_type,
//         .ptr = array, // TODO: pointer arithmetic
//         .len = len,
//         .node = node,
//     });
//
//     // switch (ri.semantics) {
//     //     .val => return b.add(.load, .{ .operand = ptr, .node = node }),
//     //     .ref => return ptr,
//     //     .ty, .any => unreachable,
//     // }
// }

// fn fieldAccess(b: *Block, scope: *Scope, ri: ResultInfo, node: Node.Index) Error!Fir.Index {
//     const fg = b.fg;
//     const field_val = b.tree.data(node).field;
//
//     const operand = try anyExpr(b, scope, field_val);
//     switch (ri.semantics) {
//         .val => return b.add(.field_val, .{ .operand = operand, .node = node }),
//         .ref => return b.add(.field_ref, .{ .operand = operand, .node = node }),
//         .any, .ty => unreachable,
//     }
// }

fn statement(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    return try switch (b.tree.data(node)) {
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
            std.log.err("statement: unexpected node: {}\n", .{b.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn globalStatement(fg: *FirGen, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const data = fg.tree.data(node);
    return try switch (data) {
        .const_decl => globalConst(fg, scope, node),
        .const_decl_attr => globalConstAttr(fg, scope, node),
        .var_decl => globalVar(fg, scope, node),
        else => {
            std.log.err("global statement: unexpected node: {}\n", .{fg.tree.data(node)});
            return GenError.NotImplemented;
        },
    };
}

fn call(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const fg = b.fg;
    const call_expr = b.tree.data(node).call;
    const ri: ResultInfo = .{ .semantics = .ref };
    const ptr = try identExpr(b, scope, ri, call_expr.ptr);

    const scratch_top = fg.scratch.items.len;
    defer fg.scratch.shrinkRetainingCapacity(scratch_top);

    const arg_nodes = b.tree.extra_data[call_expr.args_start..call_expr.args_end];
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

fn block(b: *Block, scope: *Scope, node: Node.Index, comptime add_unlinked: bool) Error!Fir.Index {
    const fg = b.fg;
    const data = b.tree.data(node).block;

    var s: *Scope = scope;
    const stmts = b.tree.extra_data[data.stmts_start..data.stmts_end];
    for (stmts) |stmt| {
        const ref = try statement(b, s, stmt);
        switch (b.tree.data(stmt)) {
            .const_decl, .const_decl_attr => {
                const ident = b.tree.tokenString(b.tree.mainToken(stmt) + 1);
                const id = try fg.pool.getOrPutString(ident);
                const var_scope = try fg.arena.create(Scope.LocalVal);
                var_scope.* = Scope.LocalVal.init(s, id, ref);
                s = &var_scope.base;
            },
            .var_decl => {
                const ident = b.tree.tokenString(b.tree.mainToken(stmt) + 2);
                const id = try fg.pool.getOrPutString(ident);
                const var_scope = try fg.arena.create(Scope.LocalPtr);
                var_scope.* = Scope.LocalPtr.init(s, id, ref);
                s = &var_scope.base;
            },
            else => {},
        }
    }

    // unwind and free scope objects
    while (s != scope) {
        const parent = s.parent().?;
        fg.arena.destroy(s);
        s = parent;
    }

    if (add_unlinked) {
        return b.addBlockUnlinked(b, node);
        // return b.addBlock(b, node);
    } else {
        return undefined;
    }
}

fn coerce(b: *Block, s: *Scope, val: Fir.Index, dest_ty: Fir.Index, node: Node.Index) !Fir.Index {
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

fn constDecl(b: *Block, s: *Scope, node: Node.Index) !Fir.Index {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope (by caller) so that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const fg = b.fg;
    const const_decl = b.tree.data(node).const_decl;

    const ident_index = b.tree.mainToken(node);
    const ident_str = b.tree.tokenString(ident_index + 1);
    const id = try fg.pool.getOrPutString(ident_str);
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

fn constDeclAttr(b: *Block, s: *Scope, node: Node.Index) !Fir.Index {
    // "initializes" constant variables
    // this doesn't actually create any instructions for declaring the constant
    // instead, the value to set the constant to is computed, and the resulting
    // instruction return value is stored in the scope such that future
    // code that needs to access this constant can simply look up the identifier
    // and refer to the associated value instruction
    const fg = b.fg;
    const const_decl = b.tree.data(node).const_decl_attr;
    const metadata = b.tree.extraData(const_decl.metadata, Node.DeclMetadata);

    const ident_index = b.tree.mainToken(node);
    const ident_str = b.tree.tokenString(ident_index + 1);
    const id = try fg.pool.getOrPutString(ident_str);
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

fn varDecl(b: *Block, s: *Scope, node: Node.Index) !Fir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = b.tree.data(node).var_decl;
    const val = try valExpr(b, s, var_decl.val);
    if (var_decl.ty == 0) {
        // untyped (inferred) declaration
        return b.add(.{
            .data = .{ .push = val },
            .loc = .{ .node = node },
        });
    } else {
        // type annotated declaration
        const dest_ty = try typeExpr(b, s, var_decl.ty);
        const coerced = try coerce(b, s, val, dest_ty, node);
        return b.add(.{
            .data = .{ .push = coerced },
            .loc = .{ .node = node },
        });
    }
}

fn globalConst(fg: *FirGen, s: *Scope, node: Node.Index) !Fir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    const const_decl = fg.tree.data(node).const_decl;

    var b = Block.init(fg, s);
    defer b.deinit();
    const scope = &b.base;

    const handle = try b.add(.{
        .data = .{ .global_handle = {} },
        .loc = .{ .node = node },
    });

    var rvalue = try valExpr(&b, scope, const_decl.val);
    if (const_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&b, scope, const_decl.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        rvalue = try coerce(&b, scope, rvalue, type_annotation, node);
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

fn globalConstAttr(fg: *FirGen, s: *Scope, node: Node.Index) !Fir.Index {
    // global constants are read only variable that exist at runtime
    // (they aren't inlined by the frontend, but can be by the backend)
    // they are created by initializing a inline block that contains the
    // constexpr instructions for computing the rvalue of the constant
    // this version supports attributes

    const const_decl = fg.tree.data(node).const_decl_attr;
    const metadata = fg.tree.extraData(const_decl.metadata, Node.DeclMetadata);

    var b = Block.init(fg, s);
    defer b.deinit();
    const scope = &b.base;

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
        rvalue = try valExpr(&b, scope, const_decl.val);
    } else {
        std.debug.assert(const_decl.val == 0);
        std.debug.assert(metadata.ty != 0); // this should be a handled user error
    }

    if (metadata.ty != 0) {
        const type_annotation = try typeExpr(&b, scope, metadata.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        if (rvalue) |val| {
            rvalue = try coerce(&b, scope, val, type_annotation, node);
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

fn globalVar(fg: *FirGen, s: *Scope, node: Node.Index) !Fir.Index {
    // "initializes" mutable variables
    // unlike constant declarations, mutable variables are stored in "memory"
    // so we have to create alloc instructions in addition to computing the value
    // otherwise, this function operates like constDecl
    const var_decl = fg.tree.data(node).var_decl;

    var b = Block.init(fg, s);
    defer b.deinit();
    const scope = &b.base;

    const handle = try b.add(.{
        .data = .{ .global_handle = {} },
        .loc = .{ .node = node },
    });
    _ = try b.add(.{
        .data = .{ .global_set_mutable = handle },
        .loc = .{ .node = node },
    });

    var rvalue = try valExpr(&b, scope, var_decl.val);
    if (var_decl.ty != 0) {
        // if this is typed, add a coerce and mark the global's type
        const type_annotation = try typeExpr(&b, scope, var_decl.ty);
        _ = try b.add(.{
            .data = .{ .global_set_type = .{ .handle = handle, .ty = type_annotation } },
            .loc = .{ .node = node },
        });

        rvalue = try coerce(&b, scope, rvalue, type_annotation, node);
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

fn assignSimple(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const assign = b.tree.data(node).assign_simple;

    const ptr = refExpr(b, scope, assign.ptr) catch |err| {
        if (err == error.InvalidLvalue) {
            // TODO: not really accurate?
            try fg.errors.append(fg.gpa, .{
                .tag = .const_assign,
                .token = b.tree.mainToken(node),
            });
            return error.ConstAssign;
        } else {
            return err;
        }
    };
    const val = try valExpr(b, scope, assign.val);
    return b.add(.{
        .data = .{ .store = .{ .ptr = ptr, .val = val } },
        .loc = .{ .node = node },
    });
}

fn conditionExpr(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const ref = try valExpr(b, scope, node);
    const condition_type = try b.add(.{
        .data = .{ .bool_type = {} },
        .loc = .{ .node = node },
    });
    return try coerce(b, scope, ref, condition_type, node);
}

fn assignBinary(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const assign = b.tree.data(node).assign_binary;
    const op = b.tree.mainToken(node);

    const ptr = refExpr(b, scope, assign.ptr) catch |err| {
        if (err == error.InvalidLvalue) {
            // TODO: not really accurate?
            try fg.errors.append(fg.gpa, .{
                .tag = .const_assign,
                .token = b.tree.mainToken(node),
            });
            return error.ConstAssign;
        } else {
            return err;
        }
    };
    const base = try valExpr(b, scope, assign.ptr);
    const val = try valExpr(b, scope, assign.val);
    const bin = try binaryInner(b, node, op, base, val);

    return b.add(.{
        .data = .{ .store = .{ .ptr = ptr, .val = bin } },
        .loc = .{ .node = node },
    });
}

fn ifSimple(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const if_simple = b.tree.data(node).if_simple;
    var block_scope = Block.init(b.fg, scope);
    defer block_scope.deinit();
    const s = &block_scope.base;

    const condition = try conditionExpr(b, scope, if_simple.condition);
    const exec = try block(&block_scope, s, if_simple.exec_true, true);
    return b.add(.{
        .data = .{ .branch_single = .{ .cond = condition, .exec_true = exec } },
        .loc = .{ .node = node },
    });
}

fn ifElse(b: *Block, scope: *Scope, node: Node.Index) Error!Fir.Index {
    const if_else = b.tree.data(node).if_else;

    const condition = try conditionExpr(b, scope, if_else.condition);
    const exec = b.tree.extraData(if_else.exec, Node.IfElse);
    const exec_true = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, exec.exec_true, true);
    };
    const exec_false = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, exec.exec_false, true);
    };
    const pl = try b.fg.addExtra(Inst.BranchDouble{
        .exec_true = exec_true,
        .exec_false = exec_false,
    });
    return b.add(.{
        .data = .{ .branch_double = .{ .cond = condition, .pl = pl } },
        .loc = .{ .node = node },
    });
}

fn ifChain(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const if_chain = b.tree.data(node).if_chain;

    const condition = try conditionExpr(b, scope, if_chain.condition);
    const chain = b.tree.extraData(if_chain.chain, Node.IfChain);
    const exec_true = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();
        break :block try block(&block_scope, &block_scope.base, chain.exec_true, true);
    };
    const next = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();
        _ = switch (b.tree.data(chain.next)) {
            .if_simple => try ifSimple(&block_scope, &block_scope.base, chain.next),
            .if_else => try ifElse(&block_scope, &block_scope.base, chain.next),
            else => unreachable,
        };

        break :block try b.addBlockUnlinked(&block_scope, node);
    };

    const pl = try b.fg.addExtra(Inst.BranchDouble{
        .exec_true = exec_true,
        .exec_false = next,
    });
    return b.add(.{
        .data = .{ .branch_double = .{ .cond = condition, .pl = pl } },
        .loc = .{ .node = node },
    });
}

fn returnStmt(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
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

fn yield(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const yield_val = b.tree.data(node).yield_val;
    const operand = try valExpr(b, scope, yield_val);

    return b.add(.{
        .data = .{ .yield_node = operand },
        .loc = .{ .node = node },
    });
}

fn loopForever(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const loop_forever = b.tree.data(node).loop_forever;
    var loop_scope = Block.init(b.fg, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_forever.body, true);

    const condition = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();

        // TODO: should be implicit not node
        const condition_inner = try b.add(.{
            .data = .{ .int = 1 },
            .loc = .{ .node = node },
        });
        const condition_type = try b.add(.{
            .data = .{ .bool_type = {} },
            .loc = .{ .node = node },
        });
        const condition = try coerce(b, scope, condition_inner, condition_type, node);
        _ = try block_scope.add(.{
            .data = .{ .yield_implicit = condition },
            .loc = .{ .token = undefined }, // TODO: token
        });

        break :block try b.addBlockUnlinked(&block_scope, node);
    };

    return b.add(.{
        .data = .{ .loop = .{ .cond = condition, .body = body } },
        .loc = .{ .node = node },
    });
}

fn loopConditional(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const loop_conditional = b.tree.data(node).loop_conditional;

    const condition = block: {
        var block_scope = Block.init(b.fg, scope);
        defer block_scope.deinit();
        const s = &block_scope.base;

        const condition = try conditionExpr(&block_scope, s, loop_conditional.condition);
        _ = try block_scope.add(.{
            .data = .{ .yield_implicit = condition },
            .loc = .{ .token = undefined },
        });
        break :block try b.addBlockUnlinked(&block_scope, node);
    };

    var loop_scope = Block.init(b.fg, scope);
    defer loop_scope.deinit();
    const body = try block(&loop_scope, &loop_scope.base, loop_conditional.body, true);

    return b.add(.{
        .data = .{ .loop = .{ .cond = condition, .body = body } },
        .loc = .{ .node = node },
    });
}

fn loopRange(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    const fg = b.fg;
    const loop_range = b.tree.data(node).loop_range;
    const signature = b.tree.extraData(loop_range.signature, Node.RangeSignature);

    const ref = try statement(b, scope, signature.binding);
    var s: *Scope = var_scope: {
        const ident = b.tree.tokenString(b.tree.mainToken(signature.binding) + 2);
        const id = try fg.pool.getOrPutString(ident);
        var var_scope = Scope.LocalPtr.init(scope, id, ref);
        break :var_scope &var_scope.base;
    };

    const condition = block: {
        var block_scope = Block.init(b.fg, s);
        defer block_scope.deinit();
        const bs = &block_scope.base;

        const condition_inner = try valExpr(&block_scope, bs, signature.condition);
        const condition_type = try b.add(.{
            .data = .{ .bool_type = {} },
            .loc = .{ .node = node },
        });
        const condition = try coerce(&block_scope, bs, condition_inner, condition_type, node);
        _ = try block_scope.add(.{
            .data = .{ .yield_implicit = condition },
            .loc = .{ .token = undefined }, // TODO
        });
        break :block try b.addBlockUnlinked(&block_scope, node);
    };

    // we have a block (loop outer body) that contains the afterthought
    // and then an inner nested block that contains the user loop body
    const body = body: {
        var outer_scope = Block.init(b.fg, s);
        defer outer_scope.deinit();

        var inner_scope = Block.init(b.fg, &outer_scope.base);
        defer inner_scope.deinit();

        _ = try block(&inner_scope, &inner_scope.base, loop_range.body, false);
        _ = try outer_scope.addBlock(&inner_scope, node);

        _ = try statement(&outer_scope, &outer_scope.base, signature.afterthought);
        // if (try statement(&outer_scope, &outer_scope.base, signature.afterthought)) |_| {
        // return error.AfterthoughtDecl;
        // }

        break :body try b.addBlockUnlinked(&outer_scope, node);
    };

    return b.add(.{
        .data = .{ .loop = .{ .cond = condition, .body = body } },
        .loc = .{ .node = node },
    });
}

fn loopBreak(b: *Block, scope: *Scope, node: Node.Index) !Fir.Index {
    _ = scope;
    return b.add(.{
        .data = .{ .@"break" = {} },
        .loc = .{ .node = node },
    });
}

fn globalIdentId(fg: *FirGen, stmt: u32) !InternPool.StringIndex {
    const ident = switch (fg.tree.data(stmt)) {
        .const_decl, .const_decl_attr => fg.tree.tokenString(fg.tree.mainToken(stmt) + 1),
        .var_decl => fg.tree.tokenString(fg.tree.mainToken(stmt) + 2),
        else => unreachable,
    };

    return fg.pool.getOrPutString(ident);
}
