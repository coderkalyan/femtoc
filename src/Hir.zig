const std = @import("std");
const Interner = @import("interner.zig").Interner;
const Ast = @import("Ast.zig");
const TokenIndex = Ast.TokenIndex;
const Value = @import("value.zig").Value;
const Type = @import("typing.zig").Type;
const error_handler = @import("error_handler.zig");
const Allocator = std.mem.Allocator;

const Node = Ast.Node;
pub const Error = error{InvalidRef};
pub const Hir = @This();

insts: std.MultiArrayList(Inst).Slice,
module_index: Index,
block_slices: [][]Hir.Index,
extra_data: []const u32,
values: []Value,
types: []Type,
interner: Interner,
untyped_decls: std.AutoHashMapUnmanaged(u32, Hir.Index),
errors: []error_handler.SourceError,
instmap: []Ref,

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
        // type declaration
        ty,

        // binary arithmetic operations
        // data.pl_node.pl = Inst.Binary
        add,
        sub,
        mul,
        div,
        mod,

        // binary comparison operations
        // data.pl_node.pl = Inst.Binary
        //
        // first set is high level (untyped) comparisons
        cmp_eq,
        cmp_ne,
        cmp_gt,
        cmp_ge,
        cmp_lt,
        cmp_le,
        // and typed concrete comparisons
        icmp_eq,
        icmp_ne,
        icmp_ugt,
        icmp_uge,
        icmp_ult,
        icmp_ule,
        icmp_sgt,
        icmp_sge,
        icmp_slt,
        icmp_sle,
        fcmp_gt,
        fcmp_ge,
        fcmp_lt,
        fcmp_le,

        // TODO
        lsl,
        lsr,
        asl,
        asr,

        // unary operators
        neg,
        log_not,
        bit_not,

        // coerces (expands to constant, cast, or noop in mir)
        // data.pl_node.pl = Inst.Coerce
        coerce,
        constant,
        zext,
        sext,
        fpext,

        // loads a module decl that is forward declared
        // that is, ref is not available during generation but will
        // be by the time the entire hir is generated and mir is running
        // data.pl_node.pl = decl
        // load_inline,
        load_global,
        // pushes a value onto the stack and returns the memory address
        // data.un_node.operand = ref to push
        push,
        // TODO
        alloca,
        global_mut,
        link_extern,
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
        // TODO: these might need node references
        // type
        ty: Type,
        ty_pl: struct {
            ty: Ref,
            // payload index in another array (like extra or values)
            pl: u32,
        },
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
        const ref_len: u32 = @intCast(@typeInfo(Hir.Ref).Enum.fields.len);
        return @enumFromInt(ref_len + index);
    }

    pub fn refToIndex(ref: Hir.Ref) ?Hir.Index {
        const ref_len: u32 = @intCast(@typeInfo(Hir.Ref).Enum.fields.len);
        const index = @intFromEnum(ref);
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

    pub const Extend = struct {
        val: Ref,
        ty: Ref,
    };

    pub const Constant = struct {
        val: u32,
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
        head: u32,
    };

    pub const Module = struct {
        len: u32,
    };

    pub const DebugValue = struct {
        name: u32,
        value: Ref,
    };

    pub const Function = struct {
        params: []Param,
        return_type: Ref,
        body: Index,
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
    comptime_uint,
    comptime_sint,
    f32_ty,
    f64_ty,
    comptime_float,
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
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            u32 => hir.extra_data[index + i],
            Ref => @enumFromInt(hir.extra_data[index + i]),
            else => unreachable,
        };
    }
    return result;
}

pub inline fn resolveRef(hir: *const Hir, ref: Ref) Ref {
    return if (Hir.Inst.refToIndex(ref)) |index| hir.instmap[index] else ref;
}

pub fn resolveType(hir: *const Hir, ref: Ref) Type {
    if (Inst.refToIndex(ref)) |index| {
        const data = hir.insts.items(.data)[index];
        return switch (hir.insts.items(.tag)[index]) {
            .ty => data.ty,
            .constant => ty: {
                const constant_data = hir.extraData(data.pl_node.pl, Hir.Inst.Constant);
                break :ty hir.resolveType(constant_data.ty);
            },
            .add, .sub, .mul, .div, .mod => ty: {
                // during generation, we make sure both operands match, so we can
                // just use the left one
                const bin_data = hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
                break :ty hir.resolveType(bin_data.lref);
            },
            .neg, .log_not, .bit_not => hir.resolveType(data.un_node.operand),
            .icmp_eq, .icmp_ne => Type.initLiteral(.uint, 1),
            .icmp_ugt, .icmp_uge, .icmp_ult, .icmp_ule => Type.initLiteral(.uint, 1),
            .icmp_sgt, .icmp_sge, .icmp_slt, .icmp_sle => Type.initLiteral(.uint, 1),
            .fcmp_gt, .fcmp_ge, .fcmp_lt, .fcmp_le => Type.initLiteral(.uint, 1),
            .push => hir.resolveType(data.un_node.operand),
            .alloca, .global_mut, .link_extern => hir.resolveType(data.un_node.operand),
            .store => unreachable, // shouldn't be referenced
            .load => hir.resolveType(Inst.indexToRef(data.pl_node.pl)),
            .param => ty: {
                const param_data = hir.extraData(data.pl_node.pl, Hir.Inst.Param);
                break :ty hir.resolveType(param_data.ty);
            },
            .call => ty: {
                const call_data = hir.extraData(data.pl_node.pl, Hir.Inst.Call);
                // std.debug.print("addr: {}\n", .{Inst.refToIndex(call_data.addr).?});
                const call_ty = hir.resolveType(call_data.addr);
                const fn_type = call_ty.extended.cast(Type.Function).?;
                // std.debug.print("fn: {}\n", .{@as(u64, @bitCast(call_ty))});
                break :ty fn_type.return_type;
            },
            .zext, .sext, .fpext => ty: {
                const ext_data = hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
                break :ty hir.resolveType(ext_data.lref);
            },
            .block, .block_inline => ty: {
                const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);
                const insts = hir.block_slices[block_data.head];
                break :ty hir.resolveType(Inst.indexToRef(insts[insts.len - 1]));
            },
            .branch_single, .branch_double, .loop => Type.initVoid(), // TODO: follow into the block
            .yield_node, .yield_implicit, .yield_inline => hir.resolveType(data.un_node.operand),
            .dbg_value, .dbg_declare, .dbg_assign => unreachable, // should not be referenced
            .ret_node, .ret_implicit => unreachable, // always end a function, can't be referenced
            .lsl, .lsr, .asl, .asr => unreachable, // TODO
            .load_global => ty: {
                const pl = hir.insts.items(.data)[index].pl_node.pl;
                const inst = hir.untyped_decls.get(pl).?;
                break :ty hir.resolveType(Inst.indexToRef(inst));
            },
            // untyped instructions should be replaced before they're referred to
            .int, .float => unreachable,
            .cmp_eq, .cmp_ne, .cmp_gt, .cmp_ge, .cmp_lt, .cmp_le => unreachable,
            .coerce, .loop_break, .fn_decl, .module => unreachable,
        };
    } else {
        return switch (ref) {
            .bool_ty => Type.initLiteral(.uint, 1),
            .u8_ty => Type.initLiteral(.uint, 8),
            .u16_ty => Type.initLiteral(.uint, 16),
            .u32_ty => Type.initLiteral(.uint, 32),
            .u64_ty => Type.initLiteral(.uint, 64),
            .i8_ty => Type.initLiteral(.sint, 8),
            .i16_ty => Type.initLiteral(.sint, 16),
            .i32_ty => Type.initLiteral(.sint, 32),
            .i64_ty => Type.initLiteral(.sint, 64),
            .f32_ty => Type.initLiteral(.float, 32),
            .f64_ty => Type.initLiteral(.float, 64),
            .btrue_val, .bfalse_val => Type.initLiteral(.uint, 1),
            .void_ty, .void_val => Type.initVoid(),
            .zero_val, .one_val => Type.initLiteralImplicitWidth(.comptime_uint),
            .comptime_uint => Type.initLiteralImplicitWidth(.comptime_uint),
            .comptime_sint => Type.initLiteralImplicitWidth(.comptime_sint),
            .comptime_float => Type.initLiteralImplicitWidth(.comptime_float),
            _ => unreachable,
        };
    }
}

pub fn refToInt(hir: *const Hir, ref: Ref) u64 {
    switch (ref) {
        .zero_val => return 0,
        .one_val => return 1,
        else => {
            const index = Hir.Inst.refToIndex(ref).?;
            const pl = hir.insts.items(.data)[index].pl_node.pl;
            const data = hir.extraData(pl, Hir.Inst.Constant);
            const val = hir.values[data.val];
            switch (val.kind()) {
                .zero => return 0,
                .one => return 1,
                .u32 => {
                    const payload = val.payload.cast(Value.Payload.U32).?;
                    const ty = hir.resolveType(data.ty);
                    if (ty.intSign()) {
                        // interpret payload as i32, sign extend it to i64,
                        // then reinterpret as u64 to return
                        // TODO: probably more readable to implement and use alu.sext
                        return @bitCast(@as(i64, @intCast(@as(i32, @bitCast(payload.int)))));
                    } else {
                        return @intCast(payload.int);
                    }
                },
                .u64 => {
                    const payload = val.payload.cast(Value.Payload.U64).?;
                    return payload.int;
                },
                else => unreachable,
            }
        },
    }
}

pub fn refToFloat(hir: *const Hir, ref: Ref) f64 {
    const index = Hir.Inst.refToIndex(ref).?;
    const pl = hir.insts.items(.data)[index].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const val = hir.values[data.val];
    const float = val.payload.cast(Value.Payload.F64).?;
    return float.float;
}
