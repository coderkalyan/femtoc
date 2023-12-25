const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Air = @This();

insts: std.MultiArrayList(Inst).Slice,
extra: []const u32,
pool: *InternPool,

// index into the instructions array (pointer to an instruction)
pub const Index = enum(u32) { _ };
// index into the extra data array (pointer to the start of an extra data segment)
pub const ExtraIndex = enum(u32) { _ };

pub const Inst = union(enum) {
    // typed value with comptime-known value
    constant: InternPool.Index,
    // value expressions
    // binary expressions take in two operands, all of these behave the same
    // they work on any type, type checking is done during semantic analysis
    // addition +
    add: struct {
        l: Index,
        r: Index,
    },
    // subtraction -
    sub: struct {
        l: Index,
        r: Index,
    },
    // multiplication *
    mul: struct {
        l: Index,
        r: Index,
    },
    // division /
    div: struct {
        l: Index,
        r: Index,
    },
    // modulo %
    mod: struct {
        l: Index,
        r: Index,
    },
    // bitwise or |
    bitwise_or: struct {
        l: Index,
        r: Index,
    },
    // bitwise and &
    bitwise_and: struct {
        l: Index,
        r: Index,
    },
    // bitwise xor ^
    bitwise_xor: struct {
        l: Index,
        r: Index,
    },
    // integer compare equal ==
    icmp_eq: struct {
        l: Index,
        r: Index,
    },
    // integer compare not equal !=
    icmp_ne: struct {
        l: Index,
        r: Index,
    },
    // integer compare unsigned greater than >
    icmp_ugt: struct {
        l: Index,
        r: Index,
    },
    // integer compare greater unsigned than equal >=
    icmp_uge: struct {
        l: Index,
        r: Index,
    },
    // integer compare unsigned less than <
    icmp_ult: struct {
        l: Index,
        r: Index,
    },
    // integer compare unsigned less than equal <=
    icmp_ule: struct {
        l: Index,
        r: Index,
    },
    // integer compare signed greater than >
    icmp_sgt: struct {
        l: Index,
        r: Index,
    },
    // integer compare signed greater than equal >=
    icmp_sge: struct {
        l: Index,
        r: Index,
    },
    // integer compare signed less than <
    icmp_slt: struct {
        l: Index,
        r: Index,
    },
    // integer compare signed less than equal <=
    icmp_sle: struct {
        l: Index,
        r: Index,
    },
    // float compare greater than >
    fcmp_gt: struct {
        l: Index,
        r: Index,
    },
    // float compare greater than equal >=
    fcmp_ge: struct {
        l: Index,
        r: Index,
    },
    // float compare less than <
    fcmp_lt: struct {
        l: Index,
        r: Index,
    },
    // float compare less than equal <=
    fcmp_le: struct {
        l: Index,
        r: Index,
    },
    // logical shift left <<
    lsl: struct {
        l: Index,
        r: Index,
    },
    // logical shift right >>
    lsr: struct {
        l: Index,
        r: Index,
    },
    // arithmetic shift left <<
    asl: struct {
        l: Index,
        r: Index,
    },
    // arithmetic shift right >>
    asr: struct {
        l: Index,
        r: Index,
    },
    // unary expressions
    // negate -
    neg: Index,
    // bitwise invert ~
    bitwise_inv: Index,
    // performs a function call
    call: struct {
        // function to call
        function: Index,
        // list of arguments
        args: ExtraIndex,
    },
    // zero extends an operand to a destination type
    zext: struct {
        operand: Index,
        ty: InternPool.Index,
    },
    // sign extends an operand to a destination type
    sext: struct {
        operand: Index,
        ty: InternPool.Index,
    },
    // floating point extends an operand to a destination type
    fpext: struct {
        operand: Index,
        ty: InternPool.Index,
    },

    // indexes into an array, slice, or many pointer (passed by
    // reference) and returns a reference to the element
    // to get the reference to a value-semantics aggregate, first
    // emit an index_val to extract an element, and then promote
    // the element to an (immutable) stack object
    index_ref: struct {
        base: Index,
        index: Index,
    },
    // indexes into an array, slice, or many pointer (passed by
    // value) and returns the value of the element
    // to get the value through a reference to the aggregate, it is
    // better to use index_ref followed by a load
    index_val: struct {
        base: Index,
        index: Index,
    },

    // allocates a stack slot and returns a pointer to it
    alloc: struct {
        // type of the object being stored in the stack slot
        slot_type: InternPool.Index,
        // type of the pointer yielded by the alloc
        // which is always slot_type*
        pointer_type: InternPool.Index,
    },
    // loads data from a memory address and returns a ref to the value
    load: struct {
        ptr: Index,
    },
    // stores data to a memory address
    store: struct {
        // where to store
        ptr: Index,
        // what to store
        val: Index,
    },

    // conditional execution that jumps below if false (if statement)
    branch_single: struct {
        // condition to branch on
        cond: Index,
        // block to execute if true
        exec_true: Index,
    },
    // conditional execution that branches both ways (if/else)
    branch_double: struct {
        // condition to branch on
        cond: Index,
        // extra payload that stores the blocks to branch to
        pl: ExtraIndex,
    },
    // loops an execution block as long as a condition is true
    loop: struct {
        // condition block to evaluate before each loop iteration
        cond: Index,
        // body to execute on each loop iteration
        body: Index,
    },
    // returns control flow to the caller, possibly with a return value
    @"return": Index,
    // exits a block, emitting a value as the "result" of the block
    yield: Index,
    // breaks out of a loop
    @"break",
    // continues to the next iteration of a loop
    @"continue",

    // block of instructions
    block: struct {
        insts: ExtraIndex,
    },
    // marks a parameter in a function body, which is referred to
    param: struct {
        name: InternPool.StringIndex,
        ty: Index,
    },

    load_decl: InternPool.Index,

    // represents a "slice" to an "array" stored as a span of elements in extra data
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const BranchDouble = struct {
        exec_true: Index,
        exec_false: Index,
    };
};

pub const Ref = enum(u32) {
    _,

    pub fn toIndex(ref: Ref) ?Index {
        const raw: u32 = @intFromEnum(ref);
        if (raw >> 31 != 0) {
            return @enumFromInt(@as(u31, @truncate(raw)));
        } else {
            return null;
        }
    }

    pub fn toInterned(ref: Ref) ?InternPool.Index {
        const raw: u32 = @intFromEnum(ref);
        if (raw >> 31 == 0) {
            return @enumFromInt(@as(u31, @truncate(raw)));
        } else {
            return null;
        }
    }
};

pub fn extraData(air: *const Air, comptime T: type, index: Air.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(air.extra[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(air: *const Air, slice: Inst.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return air.extra[start..end];
}

// pub fn typeOf(air: *const Air, pool: *const InternPool, index: Index) error{OutOfMemory}!InternPool.Index {
//     const i = @intFromEnum(index);
//     const data = air.insts.items(.data)[i];
//     switch (air.insts.items(.tags)[i]) {
//         .constant => return data.ty,
//     }
// }
