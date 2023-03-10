const std = @import("std");
const Interner = @import("interner.zig").Interner;
const Ast = @import("Ast.zig");
const TokenIndex = Ast.TokenIndex;
const Type = @import("typing.zig").Type;

const Node = Ast.Node;
pub const Error = error { InvalidRef };
pub const Hir = @This();

insts: std.MultiArrayList(Inst).Slice,
extra_data: []const u32,
interner: Interner,
resolution_map: std.AutoHashMapUnmanaged(Node.Index, Hir.Ref),

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        int,
        float,
        call,
        add,
        sub,
        mul,
        div,
        mod,
        eq,
        neq,
        leq,
        geq,
        lt,
        gt,
        lsl,
        lsr,
        asl,
        asr,

        validate_ty,
        load_inline,
        alloc,
        load,
        store,

        fn_decl,
        param,

        block,
        jump,
        branch_single,
        branch_double,
        loop,
        loop_break,
        // returns control flow to the function's callee
        // includes an operand as the return value
        // includes the instruction
        ret_implicit,
        ret_node,
        yield_implicit,
        yield_node,

        module,
    };

    pub const Data = union {
        // integer constant
        int: u64,
        // float constant
        float: f64,
        // binary operation
        bin: struct {
            l: Ref,
            r: Ref,
        },
        node: NodeIndex,
        // used for unary operations (single operand) linking to the source node
        un_node: struct {
            // reference to the node creating this instruction
            node: NodeIndex,
            // unary operand reference
            operand: Ref,
        },
        // used for unary operations (single operand) linking to the source token
        un_tok: struct {
            // reference to the token creating this instruction
            tok: TokenIndex,
            // unary operand reference
            operand: Ref,
        },
        pl_node: struct {
            // reference to the node creating this instruction
            node: NodeIndex,
            // index into extra where payload is stored
            pl: ExtraIndex,
        },
        pl_tok: struct {
            // reference to the token creating this instruction
            tok: TokenIndex,
            // index into extra where payload is stored
            pl: ExtraIndex,
        },
    };

    pub const Extra = union {
        index: u32,
        ref: Ref,
    };

    pub fn indexToRef(index: Hir.Index) Hir.Ref {
        const ref_len = @intCast(u32, @typeInfo(Hir.Ref).Enum.fields.len);
        return @intToEnum(Hir.Ref, ref_len + index);
    }

    pub fn refToIndex(ref: Hir.Ref) ?Hir.Index {
        const ref_len = @intCast(u32, @typeInfo(Hir.Ref).Enum.fields.len);
        const index = @enumToInt(ref);
        return if (index >= ref_len) index - ref_len else null;
    }

    pub const Call = struct {
        addr: Ref,
        args_len: u32,
    };

    pub const Binary = struct {
        lref: Ref,
        rref: Ref,
    };

    pub const FnDecl = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
        return_ty: Ref,
        body: Index,
    };

    pub const Param = struct {
        name: u32,
        ty: Ref,
    };

    pub const ConstDecl = struct {
        name: u32,
        ty: Ref,
    };

    pub const Store = struct {
        addr: Index,
        val: Ref,
    };

    pub const ValidateTy = struct {
        ref: Ref,
        ty: Ref,
    };

    pub const BranchSingle = struct {
        condition: Ref,
        exec_true: Index,
    };

    pub const BranchDouble = struct {
        condition: Ref,
        exec_true: Index,
        exec_false: Index,
    };

    pub const Loop = struct {
        condition: Index,
        body: Index,
    };

    pub const Block = struct {
        len: u32,
    };

    pub const Module = struct {
        len: u32,
    };
};

pub const Index = u32;
pub const NodeIndex = u32;
pub const ExtraIndex = u32;
pub const Ref = enum(u32) {
    u8_ty,
    u16_ty,
    u32_ty,
    u64_ty,
    i8_ty,
    i16_ty,
    i32_ty,
    i64_ty,
    f32_ty,
    f64_ty,
    bool_ty,
    void_ty,

    zero_val,
    one_val,
    btrue_val,
    bfalse_val,

    void_val,
    _,

    pub fn toType(ref: Hir.Ref) Type {
        return switch (ref) {
            .i8_ty => Type.initTag(.i8),
            .u8_ty => Type.initTag(.u8),
            .i16_ty => Type.initTag(.i16),
            .u16_ty => Type.initTag(.u16),
            .i32_ty => Type.initTag(.i32),
            .u32_ty => Type.initTag(.u32),
            .i64_ty => Type.initTag(.i64),
            .u64_ty => Type.initTag(.u64),
            .f32_ty => Type.initTag(.f32),
            .f64_ty => Type.initTag(.f64),
            .bool_ty => Type.initTag(.u1),
            .void_ty => Type.initTag(.void),
            .zero_val, .one_val,
            .btrue_val, .bfalse_val,
            .void_val => unreachable,
            _ => unreachable,
        };
    }
};

pub fn extraData(hir: *const Hir, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields) |field, i| {
        if (field.field_type == ExtraIndex) {
            @field(result, field.name) = hir.extra_data[index + i];
        } else if (field.field_type == Hir.Ref) {
            @field(result, field.name) = @intToEnum(Hir.Ref, hir.extra_data[index + i]);
        } else {
            unreachable;
        }
    }
    return result;
}
