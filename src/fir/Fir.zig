const std = @import("std");
const Ast = @import("../Ast.zig");
const InternPool = @import("../InternPool.zig");
const error_handler = @import("../error_handler.zig");

const Fir = @This();
const NodeIndex = Ast.Node.Index;
const TokenIndex = Ast.TokenIndex;

tree: *const Ast,
// we store Inst.payload separtely from Inst.loc, because loc can be freed after
// semantic analysis, it packs better, and its easier to have a MultiArrayList
insts: std.MultiArrayList(Inst.Data).Slice,
locs: std.ArrayListUnmanaged(Inst.Loc).Slice,
extra: []const u32,
errors: []error_handler.SourceError,
module_index: Index,
pool: *InternPool,

// index into the instructions array (pointer to an instruction)
pub const Index = enum(u32) { _ };
// index into the extra data array (pointer to the start of an extra data segment)
pub const ExtraIndex = enum(u32) { _ };

pub const Inst = struct {
    data: Data,
    loc: Loc,

    pub const Data = union(enum) {
        // literals
        // integer literal (immediate)
        int: u64,
        // float literal (immediate)
        float: f64,
        // boolean literal (immediate)
        bool: bool,
        // "none" void literal (immediate)
        none,
        // string literal
        string: InternPool.StringIndex,
        // array literal (list of items)
        array: ExtraIndex,
        // struct literal
        @"struct": struct {
            ty: Index,
            fields: ExtraIndex,
        },

        // type expressions
        // builtin type, figure out what it is using the node location
        builtin_type,
        // build a pointer * to an underlying type
        pointer_type: struct {
            pointee: Index,
            mutable: bool,
        },
        // build a many pointer [*] to an underlying type
        many_pointer_type: struct {
            pointee: Index,
        },
        // build a array [N] to an underlying type
        array_type: struct {
            // expression for underlying type
            element: Index,
            // expression for number of elements
            count: Index,
        },
        // build a slice [] to an underlying type
        slice_type: struct {
            element: Index,
        },
        // function type given a set of parameter types and return type
        function_type: struct {
            params: ExtraIndex,
            @"return": Index,
        },
        // struct type given a set of fields
        struct_type: struct {
            fields: ExtraIndex,
        },
        // yields the type of the current function body
        return_type,
        // extracts the type of a param from a function signature by index
        param_type: struct {
            function: Index,
            index: u32,
        },
        // extracts the type of an element from an array, slice, pointer, or many pointer
        element_type: struct {
            parent: Index,
        },
        // yields the type of the operand
        type_of: Index,

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
        // compare equal ==
        cmp_eq: BinaryOp,
        // compare not equal !=
        cmp_ne: BinaryOp,
        // compare greater than >
        cmp_gt: BinaryOp,
        // compare greater than equal >=
        cmp_ge: BinaryOp,
        // compare less than <
        cmp_lt: BinaryOp,
        // compare less than equal <=
        cmp_le: BinaryOp,
        // shift left <<
        sl: BinaryOp,
        // shift right >>
        sr: BinaryOp,
        // logical xor `xor`
        logical_xor: BinaryOp,

        // unary expressions
        // negate -
        neg: Index,
        // logical negation !
        logical_not: Index,
        // bitwise negation ~
        bitwise_not: Index,

        // coerce operand to destination type
        coerce: struct {
            src: Index,
            ty: Index,
        },
        // converts a stack ref to an operable pointer
        reftoptr: Index,
        // converts a pointer to a stack ref
        ptrtoref: Index,

        // performs a function call
        call: struct {
            // function to call
            function: Index,
            // list of arguments
            args: ExtraIndex,
        },
        // indexes into an array, slice, or many pointer, yielding
        // a pointer to the element (not the value), independent of
        // whether the array is passed by value or reference
        index_ref: struct {
            base: Index,
            index: Index,
        },
        // indexes into an array, slice, or many pointer, yielding
        // the value of the element, independent of
        // whether the array is passed by value or reference
        index_val: struct {
            base: Index,
            index: Index,
        },
        // constructs a slice from an array or slice
        slice: struct {
            pl: ExtraIndex,
        },
        // access a field by name from a struct, array, or slice, yielding
        // a pointer to the field (not the value), independent of
        // whether the array is passed by value or reference
        field_ref: struct {
            base: Index,
            field: InternPool.StringIndex,
        },
        // access a field by name from a struct, array, or slice, yielding
        // the value of the field, independent of
        // whether the array is passed by value or reference
        field_val: struct {
            base: Index,
            field: InternPool.StringIndex,
        },

        // loads a module decl that is forward declared
        // that is, ref is not available during generation but
        // will be by the time the entire fir is generated
        load_global: struct {
            name: InternPool.StringIndex,
        },

        // pushes a value onto the stack and returns the memory address
        // generated when a const variable requires a reference
        // this is later split into an alloca and a store
        push: Index,
        // same as above, but the stack reference will be mutable
        // loads data from a memory address and returns a ref to the value
        // generated when a mutable variable is encountered in ast
        push_mut: Index,
        load: struct {
            ptr: Index,
        },
        // same as above, but can be folded into a second instruction that
        // consumes it in a specific way
        load_lazy: struct {
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
        // loops an execution block forever (until break)
        loop_forever: struct {
            // body to execute on each loop iteration
            body: Index,
        },

        // returns control flow to the caller (explicitly generated from `return` statement)
        // includes a return value
        return_node: Index,
        // same as return_node, but generated implicitly without a return statement
        return_implicit: Index,
        // exits a block, emitting a value as the "result" of the block
        // generated by a `yield` statement
        yield_node: Index,
        // same as yield_node, but generated implicitly
        yield_implicit: Index,
        // same as yield_implicit, but for inline blocks
        yield_inline: Index,
        // breaks out of a loop (`break` statement)
        @"break",
        // continues to the next execution of a loop (`continue` statement)
        @"continue",

        // block of instructions
        block: struct {
            insts: ExtraIndex,
        },
        // same as block, but for code that will get comptime inlined
        // currently only used for toplevel declarations
        block_inline: struct {
            insts: ExtraIndex,
        },

        // function body
        function: struct {
            // function signature type (contains param types and return type)
            signature: Index,
            // function body block
            body: Index,
        },
        // marks a parameter in a function body, which is referred to
        param: struct {
            name: InternPool.StringIndex,
            ty: Index,
        },

        // global declaration markings
        // initialize a global
        global_handle,
        // marks a global as mutable
        global_set_mutable: Index,
        // sets an initial value to a global
        global_set_init: struct {
            // the global to initialize
            handle: Index,
            // value to initialize it with
            val: Index,
        },
        // sets the type of a global
        global_set_type: struct {
            // the global to modify
            handle: Index,
            // type
            ty: Index,
        },
        global_set_linkage_external: Index,
        // names a global
        global: struct {
            name: InternPool.StringIndex,
            block: Index,
        },

        // debug information
        dbg_block_begin,
        dbg_block_end,
        dbg_var_val: struct {
            name: InternPool.StringIndex,
            val: Index,
        },
        dbg_var_ptr: struct {
            name: InternPool.StringIndex,
            ptr: Index,
        },

        // toplevel instruction
        module: struct {
            insts: ExtraIndex,
        },

        pub const BinaryOp = struct {
            l: Index,
            r: Index,
        };
    };

    pub const Tag = std.meta.Tag(Data);

    pub const Loc = union {
        node: NodeIndex,
        token: TokenIndex,
    };

    // pub const SrcLoc = struct {
    //     line: u32,
    //     col: u32,
    // };

    // represents a "slice" to an "array" stored as a span of elements in extra data
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const BranchDouble = struct {
        exec_true: Index,
        exec_false: Index,
    };

    pub const Slice = struct {
        base: Index,
        start: Index,
        end: Index,
    };

    pub const StructField = struct {
        name: InternPool.StringIndex,
        ty: Index,
    };

    pub const StructFieldInitializer = struct {
        name: InternPool.StringIndex,
        val: Index,
    };
};

// constructs a Fir.Inst by consolidating data insts and locs arrays
pub fn get(fir: *const Fir, index: Fir.Index) Fir.Inst {
    const i: u32 = @intFromEnum(index);
    std.debug.assert(i < fir.insts.len);
    return .{
        .data = fir.insts.get(i),
        .loc = fir.locs[i],
    };
}

pub fn tag(fir: *const Fir, index: Fir.Index) std.meta.Tag(Inst.Data) {
    return fir.insts.items(.tags)[@intFromEnum(index)];
}

pub fn extraData(fir: *const Fir, comptime T: type, index: Fir.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    const base: u32 = @intFromEnum(index);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @enumFromInt(fir.extra[base + i]),
        }
    }
    return result;
}

pub fn extraSlice(fir: *const Fir, slice: Inst.ExtraSlice) []const u32 {
    const start: u32 = @intFromEnum(slice.start);
    const end: u32 = @intFromEnum(slice.end);
    return fir.extra[start..end];
}
