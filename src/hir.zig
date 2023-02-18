const std = @import("std");
const TokenIndex = @import("ast.zig").TokenIndex;
const Interner = @import("interner.zig").Interner;
const Node = @import("ast.zig").Node;
const Type = @import("typing.zig").Type;

pub const Error = error { InvalidRef };

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

        validate_ty,
        load_inline,
        alloc,
        load,
        store,

        fn_decl,

        block,
        jump,
        branch_single,
        branch_double,
        // returns control flow to the function's callee
        // includes an operand as the return value
        // includes the instruction
        ret_implicit,
        ret_node,

        toplevel,
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

    pub const Index = u32;
    pub const NodeIndex = u32;
    pub const ExtraIndex = u32;

    pub const Extra = union {
        index: u32,
        ref: Ref,
    };

    pub fn indexToRef(index: Inst.Index) Inst.Ref {
        const ref_len = @intCast(u32, @typeInfo(Inst.Ref).Enum.fields.len);
        return @intToEnum(Inst.Ref, ref_len + index);
    }

    pub fn refToIndex(ref: Inst.Ref) ?Inst.Index {
        const ref_len = @intCast(u32, @typeInfo(Inst.Ref).Enum.fields.len);
        const index = @enumToInt(ref);
        return if (index >= ref_len) index - ref_len else null;
    }

    pub fn refToType(ref: Inst.Ref) ?Type {
        return switch (ref) {
            .u1_ty => Type.initTag(.u1),
            .u8_ty => Type.initTag(.u8),
            .i8_ty => Type.initTag(.i8),
            .u16_ty => Type.initTag(.u16),
            .i16_ty => Type.initTag(.i16),
            .u32_ty => Type.initTag(.u32),
            .i32_ty => Type.initTag(.i32),
            .u64_ty => Type.initTag(.u64),
            .i64_ty => Type.initTag(.i64),
            .bool_ty => Type.initTag(.u8),
            else => null,
        };
    }

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
    };

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
    // pub const Arg = struct {
    //     val: Ref,
    // };

    pub const BranchSingle = struct {
        condition: Ref,
        exec_true: Index,
    };

    pub const BranchDouble = struct {
        condition: Ref,
        exec_true: Index,
        exec_false: Index,
    };

    pub const Block = struct {
        len: u32,
        // insts_start: ExtraIndex,
        // insts_end: ExtraIndex,
    };

    pub const Toplevel = struct {
        len: u32,
        // insts_start: ExtraIndex,
        // insts_end: ExtraIndex,
    };
};

pub const Hir = struct {
    insts: std.MultiArrayList(Inst).Slice,
    extra_data: []const u32,
    interner: Interner,
    resolution_map: std.AutoHashMap(Node.Index, Inst.Ref),

    pub fn extraData(hir: *const Hir, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            if (field.field_type == Inst.ExtraIndex) {
                @field(result, field.name) = hir.extra_data[index + i];
            } else if (field.field_type == Inst.Ref) {
                @field(result, field.name) = @intToEnum(Inst.Ref, hir.extra_data[index + i]);
            } else {
                unreachable;
            }
        }
        return result;
    }
};
