const std = @import("std");
const InternPool = @import("../InternPool.zig");

const Air = @This();

insts: std.MultiArrayList(Inst).Slice,
extra: []const u32,
pool: *InternPool,
toplevel: Index,

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
    add: BinaryOp,
    // subtraction -
    sub: BinaryOp,
    // multiplication *
    mul: BinaryOp,
    // division /
    div: BinaryOp,
    // modulo %
    mod: BinaryOp,
    // bitwise or |
    bitwise_or: BinaryOp,
    // bitwise and &
    bitwise_and: BinaryOp,
    // bitwise xor ^
    bitwise_xor: BinaryOp,
    // integer compare equal ==
    icmp_eq: BinaryOp,
    // integer compare not equal !=
    icmp_ne: BinaryOp,
    // integer compare unsigned greater than >
    icmp_ugt: BinaryOp,
    // integer compare greater unsigned than equal >=
    icmp_uge: BinaryOp,
    // integer compare unsigned less than <
    icmp_ult: BinaryOp,
    // integer compare unsigned less than equal <=
    icmp_ule: BinaryOp,
    // integer compare signed greater than >
    icmp_sgt: BinaryOp,
    // integer compare signed greater than equal >=
    icmp_sge: BinaryOp,
    // integer compare signed less than <
    icmp_slt: BinaryOp,
    // integer compare signed less than equal <=
    icmp_sle: BinaryOp,
    // float compare greater than >
    fcmp_gt: BinaryOp,
    // float compare greater than equal >=
    fcmp_ge: BinaryOp,
    // float compare less than <
    fcmp_lt: BinaryOp,
    // float compare less than equal <=
    fcmp_le: BinaryOp,
    // logical shift left <<
    lsl: BinaryOp,
    // logical shift right >>
    lsr: BinaryOp,
    // arithmetic shift left <<
    asl: BinaryOp,
    // arithmetic shift right >>
    asr: BinaryOp,
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
    // promotes a "ref" (internal stack reference type) to a pointer
    reftoptr: struct {
        operand: Index,
        ty: InternPool.Index,
    },
    // demotes a pointer to a ref
    ptrtoref: struct {
        operand: Index,
        ty: InternPool.Index,
    },
    // initializes a slice with a ptr and len
    slice_init: struct {
        pl: ExtraIndex,
    },
    // initializes an array with a list of elements
    array_init: struct {
        ty: InternPool.Index,
        elements: ExtraIndex,
    },
    // initializes an array with a list of fields (in order)
    struct_init: struct {
        ty: InternPool.Index,
        fields: ExtraIndex,
    },

    // indexes into an array, slice, or many pointer (passed by
    // reference) and returns a reference to the element
    // to get the reference to a value-semantics aggregate, first
    // emit an index_val to extract an element, and then promote
    // the element to an (immutable) stack object
    index_ref: struct {
        pl: ExtraIndex,
    },
    // indexes into an array, slice, or many pointer (passed by
    // value) and returns the value of the element
    // to get the value through a reference to the aggregate, it is
    // better to use index_ref followed by a load
    index_val: struct {
        base: Index,
        index: Index,
    },
    // extracts the ptr field from a slice (passed by value)
    // and returns it
    slice_ptr_val: struct {
        base: Index,
        ty: InternPool.Index,
    },
    // extracts the len field from a slice (passed by value)
    // and returns it
    slice_len_val: struct {
        base: Index,
        ty: InternPool.Index,
    },
    // extracts a field by name from a struct (passed by value)
    // and returns it
    field_val: struct {
        base: Index,
        name: InternPool.StringIndex,
    },

    // allocates a stack slot and returns a const pointer to it
    alloc: struct {
        // type of the object being stored in the stack slot
        slot_type: InternPool.Index,
        // type of the pointer yielded by the alloc
        // which is always slot_type *
        pointer_type: InternPool.Index,
    },
    // allocates a stack slot and returns a mut pointer to it
    alloc_mut: struct {
        // type of the object being stored in the stack slot
        slot_type: InternPool.Index,
        // type of the pointer yielded by the alloc
        // which is always slot_type *mut
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
    loop_while: struct {
        // condition block to evaluate before each loop iteration
        cond: Index,
        // body to execute on each loop iteration
        body: Index,
    },
    // loops an execution block forever (until broken out of)
    loop_forever: struct {
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
        ty: InternPool.Index,
    },

    // TODO: it would be better to store a decl index
    load_decl: struct {
        ip_index: InternPool.Index,
        ty: InternPool.Index,
    },

    // represents a "slice" to an "array" stored as a span of elements in extra data
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    // not actually stored as extra data, but used to avoid duplicating switching logic
    pub const BinaryOp = struct {
        l: Index,
        r: Index,
    };

    pub const BranchDouble = struct {
        exec_true: Index,
        exec_false: Index,
    };

    pub const IndexRef = struct {
        base: Index,
        index: Index,
        ty: InternPool.Index,
    };

    pub const SliceInit = struct {
        ptr: Index,
        len: Index,
        ty: InternPool.Index,
    };

    pub const Tag = std.meta.Tag(Inst);
};

pub const Decl = struct {
    // name: ?InternPool.StringIndex,
    name: NamingStrategy,
    ty: InternPool.Index,
    initializer: ?InternPool.Index,
    mutable: bool,
    linkage: Linkage,

    pub const Linkage = enum {
        internal,
        external,
    };

    pub const NamingStrategy = union(enum) {
        named: InternPool.StringIndex,
        unnamed,
    };
};

pub const FunctionBody = struct {
    air: InternPool.Index,
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

pub fn typeOf(air: *const Air, index: Index) InternPool.Index {
    const pool = air.pool;
    const i = @intFromEnum(index);
    switch (air.insts.get(i)) {
        .constant => |constant| {
            const tv = pool.indexToKey(constant).tv;
            return tv.ty;
        },
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
        .lsl,
        .lsr,
        .asl,
        .asr,
        => |bin| return air.typeOf(bin.l),
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
        => return InternPool.Index.u1_type,
        .neg,
        .bitwise_inv,
        => |un| return air.typeOf(un),
        .call => |call| {
            const ty = air.typeOf(call.function);
            const ptr_type = pool.indexToType(ty).pointer;
            const function_type = pool.indexToType(ptr_type.pointee).function;
            return function_type.@"return";
        },
        inline .zext, .sext, .fpext, .reftoptr, .ptrtoref => |cast| return cast.ty,
        .slice_init => |slice_init| {
            const data = air.extraData(Air.Inst.SliceInit, slice_init.pl);
            return data.ty;
        },
        .array_init => |array_init| return array_init.ty,
        .struct_init => |struct_init| return struct_init.ty,
        .index_ref => |index_ref| {
            const data = air.extraData(Air.Inst.IndexRef, index_ref.pl);
            return data.ty;
        },
        .index_val => |index_val| {
            const ty = air.typeOf(index_val.base);
            const base_type = pool.indexToKey(ty).ty;
            switch (base_type) {
                .array => |array| return array.element,
                .many_pointer => |many_pointer| return many_pointer.pointee,
                .slice => |slice| return slice.element,
                else => unreachable,
            }
        },
        .slice_ptr_val => |slice_ptr_val| return slice_ptr_val.ty,
        .slice_len_val => |slice_len_val| return slice_len_val.ty,
        .field_val => |field_val| {
            // TODO: this is pretty expensive, maybe we want to cache the type
            // in the instruction
            const struct_ip_index = air.typeOf(field_val.base);
            const struct_type = pool.indexToType(struct_ip_index).@"struct";
            const field_types = pool.extra.items[struct_type.fields.start..struct_type.fields.end];

            const field_map_index = pool.indexToKey(struct_type.names).field_map;
            const field_map = pool.fields.at(@intFromEnum(field_map_index));
            const slot_index = field_map.get(field_val.name).?;
            return @enumFromInt(field_types[slot_index]);
        },
        inline .alloc, .alloc_mut => |alloc| return alloc.pointer_type,
        .load => |load| {
            const ty = air.typeOf(load.ptr);
            const pointer_type = pool.indexToKey(ty).ty;
            switch (pointer_type) {
                .ref => |ref| return ref.pointee,
                .pointer => |pointer| return pointer.pointee,
                .many_pointer => |many_pointer| return many_pointer.pointee,
                else => unreachable,
            }
        },
        .store => unreachable, // should not be referenced
        .branch_single => unreachable, // should not be referenced
        .branch_double => unreachable, // TODO
        .loop_forever,
        .loop_while,
        .@"return",
        .yield,
        .@"break",
        .@"continue",
        => unreachable, // should not be referenced
        .block => unreachable, // TODO
        .param => |param| return param.ty,
        .load_decl => |load_decl| return load_decl.ty,
    }
}
