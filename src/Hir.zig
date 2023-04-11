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
        // load an integer literal (immediate)
        // data.int = int value
        int,
        // load a float literal (immediate)
        // data.float = float value
        float,

        // binary arithmetic operations
        // data.pl_node.pl = Inst.Binary
        add,
        sub,
        mul,
        div,
        mod,

        // binary comparison operations
        // data.pl_node.pl = Inst.Binary
        cmp_eq,
        cmp_ne,
        cmp_gt,
        cmp_ge,
        cmp_lt,
        cmp_le,

        // TODO
        lsl,
        lsr,
        asl,
        asr,

        // coerces (expands to constant, cast, or noop in mir)
        // data.pl_node.pl = Inst.Coerce
        coerce,
        // loads a module decl that is forward declared
        // that is, ref is not available during generation but will
        // be by the time the entire hir is generated and mir is running
        // data.pl_node.pl = decl
        load_inline,
        // pushes a value onto the stack and returns the memory address
        // data.un_node.operand = ref to push
        alloc_push,
        // loads data from a memory address and returns a ref to the value
        // data.un_node.operand = memory address (ref to alloc_push or decl)
        load,
        // stores data to a memory address
        // data.pl_node.pl = Inst.Store
        store,

        // TODO
        fn_decl,
        param,
        // calls a function at an address (ref) and a list of arguments
        // data.pl_node.pl = Inst.Call
        call,
        
        decl_const,
        decl_mut,
        decl_export,

        // scope block - body of function, loop, branch, etc
        // data.pl_node.pl = Inst.Block
        block,
        // same as block, but doesn't generate a block in mir
        // used only for grouping of related hir instructions
        // ends in yield_inline emitting the value calculated in the block
        // data.pl_node.pl = Inst.Block
        block_inline,

        // conditional execution that jumps below if false (if statement)
        // data.pl_node.pl = Inst.BranchSingle
        branch_single,
        // conditional execution that branches both ways (if/else)
        // data.pl_node.pl = Inst.BranchDouble
        branch_double,
        // loops an execution block as long as a condition is true
        // data.pl_node.pl = Inst.Loop
        loop,
        // breaks out of a loop
        loop_break,

        // returns control flow to the function's callee (not explicitly stated in src code)
        // includes an operand as the return value
        // data.un_tok.tok = token that caused the *implicit* return to be generated
        // data.un_tok.operand = ref to data to return
        ret_implicit,
        // same as ret_implicit, but generated explicitly from src code (return statement)
        // data.un_node.node = return node
        // data.un_node.operand = ref to data to return
        ret_node,

        // jumps out a block, emitting a value to be used outside the block expression
        // data.un_tok.tok = token that caused the implicit yield to be generated
        // data.un_tok.operand = ref to data to emit
        yield_implicit,
        // same as yield_implicit, but generated explicitly from src code (yield statement)
        // data.un_node.node = yield node
        // data.un_node.operand = ref to data to emit
        yield_node,
        // same as yield_implicit, but for inline blocks
        yield_inline,

        // TODO
        dbg_value,
        dbg_declare,
        dbg_assign,

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
        return_type: Ref,
        body: Index,
        hash_lower: u32,
        hash_upper: u32,
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

    pub const Coerce = struct {
        val: Ref,
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

    pub const DebugValue = struct {
        name: u32,
        value: Ref,
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
};

pub fn extraData(hir: *const Hir, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields) |field, i| {
        @field(result, field.name) = switch (field.field_type) {
            u32 => hir.extra_data[index + i],
            Ref => @intToEnum(Ref, hir.extra_data[index + i]),
            else => unreachable,
        };
    }
    return result;
}
