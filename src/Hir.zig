const std = @import("std");
const Interner = @import("interner.zig").Interner;
const Ast = @import("Ast.zig");
const TokenIndex = Ast.TokenIndex;
const Value = @import("value.zig").Value;
const Type = @import("hir/type.zig").Type;
const error_handler = @import("error_handler.zig");
const Allocator = std.mem.Allocator;

const Node = Ast.Node;
pub const Error = error{InvalidRef};
pub const Hir = @This();

tree: *const Ast,
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
        // data.int = immediate value
        int,
        // load a float literal (immediate)
        // data.float = immediate value
        float,
        // load a void literal (immediate)
        // no data
        none,
        // declare a new type to be used
        // data.ty = type (stored directly)
        // TODO: why do we need this instead of types[]?
        ty,
        pointer_ty,
        // declare a new typed immediate constant
        // data.pl_node.pl = Inst.Constant
        constant,

        // binary arithmetic operations
        // data.pl_node.pl = Inst.Binary
        add,
        sub,
        mul,
        div,
        mod,

        // binary comparison operations
        // data.pl_node.pl = Inst.Binary
        // these comparisons are untyped and therefore unchecked
        // emitted directly when we see a comparison operator in
        // the ast.
        cmp_eq,
        cmp_ne,
        cmp_gt,
        cmp_ge,
        cmp_lt,
        cmp_le,
        // these are typed comparison, after checking that operands match
        // and can be used
        // there are separate ones for signed and unsigned integer comparison
        // as well as float comparison
        // we don't currently have float (in)equality since its not a good thing to do
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
        // data.un_node.operand = operand to use
        neg,
        // TODO: phase this out in codegen (turn it into a cmp)
        log_not,
        bit_not,

        // user requested implicit type coercion
        // i.e. type annotation
        // data.pl_node.pl = Inst.Coerce
        coerce,

        // lossless extend data to larger sized type
        // data.pl_node.pl = Inst.Extend
        zext,
        sext,
        fpext,

        // loads a module decl that is forward declared
        // that is, ref is not available during generation but will
        // be by the time the entire hir is generated and mir is running
        // data.pl_node.pl = decl
        // load_inline,
        // TODO
        load_global,

        // pushes a value onto the stack and returns the memory address
        // generated when a mutable variable is encountered in ast
        // this is later split into an alloca and a store
        // data.un_node.operand = ref to push
        push,

        // allocates a stack slot and returns the memory address
        // nothing is written to the slot, there must be a store
        // before any loads to this slot to avoid reading undefined memory
        // data.un_node.operand = ref to type of slot
        alloca,
        // loads data from a memory address and returns a ref to the value
        // data.un_node.operand = memory address (stack slot from push or alloca)
        load,
        // stores data to a memory address (stack slot from push or alloca)
        // data.pl_node.pl = Inst.Store
        store,

        // marks a value in a global variable block_inline as mutable
        // and returns the "modified" instruction
        // data.un_node.operand = variable just marked mutable
        global_mut,
        // marks a value in a global variable block_inline as externally linked
        // and returns the "modified" instruction
        // data.un_node.operand = variable just marked externally linked
        link_extern,

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

        // returns control flow to the callee, generated explicitly from src code (return statement)
        // includes an operand as the return value
        // data.un_node.node = return node
        // data.un_node.operand = data to return
        ret_node,
        // same as ret_node, but not explicitly stated in src code (implicit return)
        // includes an operand as the return value
        // data.un_tok.tok = token that caused the *implicit* return to be generated (i.e. brace)
        // data.un_tok.operand = data to return
        ret_implicit,

        // jumps out of a block, emitting a value to be used outside the block expression
        // data.un_tok.tok = token that caused the implicit yield to be generated
        // data.un_tok.operand = ref to data to emit
        yield_implicit,
        // same as yield_implicit, but generated explicitly from src code (yield statement)
        // data.un_node.node = yield node
        // data.un_node.operand = ref to data to emit
        yield_node,
        // same as yield_implicit, but for inline blocks
        // data.un_tok.tok = token that caused the implicit yield to be generated
        // data.un_tok.operand = ref to data to emit
        yield_inline,

        // TODO
        dbg_value,
        dbg_declare,
        dbg_assign,

        // top level instruction storing everything in a module (file) generated
        // data.pl_node.pl = Inst.Module
        module,
    };

    pub const Data = union {
        // int immediate
        int: u64,

        // float immediate
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
        token: TokenIndex,
        un_node_new: struct {
            node: NodeIndex,
            operand: Index,
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
};

pub const Index = u32;
pub const NodeIndex = u32;
pub const ExtraIndex = u32;
pub const Ref = enum(u32) {
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

// recursively resolves the type of a hir
// most instructions don't store their type, this is reserved
// for certain types like constants (that are typed values)
// but in most other cases, the type can be trivially determined
// either by the type of instruction (comparison => u1),
// signature (call is the return type of the address),
// or by recursively resolving the operand type (arithmetic)
pub fn resolveType(hir: *const Hir, gpa: Allocator, ref: Ref) !Type {
    if (Inst.refToIndex(ref)) |index| {
        const data = hir.insts.items(.data)[index];
        return switch (hir.insts.items(.tag)[index]) {
            // type instructions just store a type
            .ty => data.ty,
            // constants are typed values, they reference a type
            .constant => ty: {
                const constant_data = hir.extraData(data.pl_node.pl, Hir.Inst.Constant);
                break :ty hir.resolveType(gpa, constant_data.ty);
            },
            .none => Type.Common.void_type,
            // unary operand instructions - recursively resolve
            .neg,
            .log_not,
            .bit_not,
            .global_mut,
            .link_extern,
            .yield_node,
            .yield_implicit,
            .yield_inline,
            .ret_node,
            .ret_implicit,
            => hir.resolveType(gpa, data.un_node.operand),
            .push => {
                const pointee = try hir.resolveType(gpa, data.un_node.operand);
                const inner = try gpa.create(Type.Pointer);
                inner.* = .{ .pointee = pointee };
                return .{ .extended = &inner.base };
            },
            .alloca => {
                const pointee = try hir.resolveType(gpa, Hir.Inst.indexToRef(data.un_node_new.operand));
                const inner = try gpa.create(Type.Pointer);
                inner.* = .{ .pointee = pointee };
                return .{ .extended = &inner.base };
            },
            // binary operand instructions - recursively resolve
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .lsl,
            .asl,
            .lsr,
            .asr,
            => ty: {
                // during generation, we make sure both operands match, so we can
                // just use the left one
                const bin_data = hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
                break :ty hir.resolveType(gpa, bin_data.lref);
            },
            // comparison instructions - returns a u1
            .icmp_eq,
            .icmp_ne,
            .icmp_ugt,
            .icmp_uge,
            .icmp_ult,
            .icmp_ule,
            .icmp_sgt,
            .icmp_sge,
            .icmp_slt,
            .icmp_sle,
            .fcmp_gt,
            .fcmp_ge,
            .fcmp_lt,
            .fcmp_le,
            => Type.Common.u1_type,
            // resolve the type of the alloca/push being loaded
            .load => ty: {
                const pointer = try hir.resolveType(gpa, Inst.indexToRef(data.pl_node.pl));
                const inner = pointer.extended.cast(Type.Pointer).?;
                break :ty inner.pointee;
            },
            // parameters reference their type
            .param => ty: {
                const param_data = hir.extraData(data.pl_node.pl, Hir.Inst.Param);
                break :ty hir.resolveType(gpa, param_data.ty);
            },
            // resolve the return type of the address being called
            .call => ty: {
                const call_data = hir.extraData(data.pl_node.pl, Hir.Inst.Call);
                const call_ty = try hir.resolveType(gpa, call_data.addr);
                const fn_type = call_ty.extended.cast(Type.Function).?;
                break :ty fn_type.return_type;
            },
            // resolve the type being extended to
            .zext,
            .sext,
            .fpext,
            => ty: {
                const ext_data = hir.extraData(data.pl_node.pl, Hir.Inst.Extend);
                break :ty hir.resolveType(gpa, ext_data.ty);
            },
            // resolve the last instruction in the block
            .block,
            .block_inline,
            => ty: {
                const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);
                const insts = hir.block_slices[block_data.head];
                break :ty hir.resolveType(gpa, Inst.indexToRef(insts[insts.len - 1]));
            },
            .branch_single,
            .branch_double,
            .loop,
            .loop_break,
            => unreachable, // TODO: follow into the block
            .load_global => ty: {
                const pl = hir.insts.items(.data)[index].pl_node.pl;
                const inst = hir.untyped_decls.get(pl).?;
                break :ty hir.resolveType(gpa, Inst.indexToRef(inst));
            },
            // should not be referenced (don't return anything meaningful)
            .dbg_value,
            .dbg_declare,
            .dbg_assign,
            .store,
            => unreachable,
            // untyped instructions should be replaced before they're referred to
            .int,
            .float,
            .cmp_eq,
            .cmp_ne,
            .cmp_gt,
            .cmp_ge,
            .cmp_lt,
            .cmp_le,
            .coerce,
            .fn_decl,
            .module,
            .pointer_ty,
            => unreachable,
        };
    } else {
        return switch (ref) {
            _ => unreachable,
        };
    }
}

pub fn refToInt(hir: *const Hir, ref: Ref) u64 {
    const index = Hir.Inst.refToIndex(ref).?;
    return hir.instToInt(index);
}

pub fn instToInt(hir: *const Hir, index: Index) u64 {
    const pl = hir.insts.items(.data)[index].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const val = hir.values[data.val];
    switch (val.kind()) {
        .zero => return 0,
        .one => return 1,
        .u32 => {
            const payload = val.extended.cast(Value.U32).?;
            const ty = hir.resolveType(undefined, data.ty) catch unreachable;
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
            const payload = val.extended.cast(Value.U64).?;
            return payload.int;
        },
        else => unreachable,
    }
}

pub fn refToFloat(hir: *const Hir, ref: Ref) f64 {
    const index = Hir.Inst.refToIndex(ref).?;
    const pl = hir.insts.items(.data)[index].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const val = hir.values[data.val];
    const float = val.extended.cast(Value.F64).?;
    return float.float;
}
