const std = @import("std");
const HirGen = @import("hirgen.zig").HirGen;
const TokenIndex = @import("ast.zig").TokenIndex;

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
        eq,
        neq,

        fn_decl,

        block,
        branch,
        // returns control flow to the function's callee
        // includes an operand as the return value
        // includes the instruction
        ret_implicit,
        ret_node,
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
    // call: Inst.Ref,
    // fn_decl: Inst.Ref,
    // if_stmt: Inst.Ref,
    // block: Inst.Ref,
    // return_void: void,
    // return_val: Inst.Ref,

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

    pub fn refToIndex(ref: Inst.Ref) !Inst.Index {
        const ref_len = @intCast(u64, @typeInfo(Inst.Ref).Enum.fields.len);
        const index = @enumToInt(ref);
        return if (index >= ref_len) index - ref_len else Error.InvalidRef;
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

        izero_val,
        ione_val,
        fzero_val,
        fone_val,

        void_val,
        _,
    };

    // pub const Extra = union {
    //     ref: Ref,
    //     extra: ExtraIndex,
    //     str: u64,
    // }; 

    pub const Call = struct {
        addr: Ref,
        args_start: ExtraIndex,
        args_end: ExtraIndex,
    };

    pub const Binary = struct {
        lref: Ref,
        rref: Ref,
    };

    pub const FnDecl = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
        body: Ref,
    };

    pub const Param = struct {
        name: u64,
        ty: Ref,
    };

    pub const ConstDecl = struct {
        name: u64,
        ty: Ref,
    };
    // pub const Arg = struct {
    //     val: Ref,
    // };

    pub const Branch = struct {
        condition: Ref,
        body: Ref,
    };

    pub const Block = struct {
        insts_start: ExtraIndex,
        insts_end: ExtraIndex,
    };

    pub const Toplevel = struct {
        insts_start: ExtraIndex,
        insts_end: ExtraIndex,
    };
};

pub const Hir = struct {
    inst: std.ArrayList(Inst).Slice,
    extra_data: []Inst.Extra,

    pub fn extraData(hir: *Hir, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            if (field.type == Inst.ExtraIndex) {
                @field(result, field.name) = try HirGen.refToIndex(hir.extra_data[index + i]);
            } else if (field.type == Inst.Ref) {
                @field(result, field.name) = hir.extra_data[index + i];
            } else {
                unreachable;
            }
        }
        return result;
    }
};
