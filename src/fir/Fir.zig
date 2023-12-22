const std = @import("std");
const Ast = @import("../Ast.zig");
const Interner = @import("../interner.zig").Interner;

const Fir = @This();
const NodeIndex = Ast.Node.Index;
const TokenIndex = Ast.TokenIndex;

tree: *const Ast,
// we store Inst.payload separtely from Inst.loc, because loc can be freed after
// semantic analysis, it packs better, and its easier to have a MultiArrayList
insts: std.MultiArrayList(Inst.Data).Slice,
locs: std.ArrayListUnmanaged(Inst.Loc).Slice,
extra: []const u32,
module_index: Index,
interner: Interner,

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
        // "none" void literal (immediate)
        none,

        // type expressions
        // builtin type, figure out what it is using the node location
        builtin_type,
        // boolean type, used to coerce conditions
        // TODO: try to get rid of this
        bool_type,
        // build a pointer * to an underlying type
        pointer_type: struct {
            pointee: Index,
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
        // compare equal ==
        cmp_eq: struct {
            l: Index,
            r: Index,
        },
        // compare not equal !=
        cmp_ne: struct {
            l: Index,
            r: Index,
        },
        // compare greater than >
        cmp_gt: struct {
            l: Index,
            r: Index,
        },
        // compare greater than equal >=
        cmp_ge: struct {
            l: Index,
            r: Index,
        },
        // compare less than <
        cmp_lt: struct {
            l: Index,
            r: Index,
        },
        // compare less than equal <=
        cmp_le: struct {
            l: Index,
            r: Index,
        },
        // shift left <<
        sl: struct {
            l: Index,
            r: Index,
        },
        // shift right >>
        sr: struct {
            l: Index,
            r: Index,
        },

        // unary expressions
        // negate -
        neg: Index,
        // bitwise invert ~
        bitwise_inv: Index,

        // coerce operand to destination type
        coerce: struct {
            src: Index,
            ty: Index,
        },

        // performs a function call
        call: struct {
            // function to call
            function: Index,
            // list of arguments
            args: ExtraIndex,
        },

        // loads a module decl that is forward declared
        // that is, ref is not available during generation but
        // will be by the time the entire fir is generated
        load_global: struct {
            // TODO: change this to a intern pool string index
            name: u32,
        },

        // pushes a value onto the stack and returns the memory address
        // generated when a mutable variable is encountered in ast
        // this is later split into an alloca and a store
        push: Index,
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
            name: u32,
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
            name: u32,
            block: Index,
        },

        // toplevel instruction
        module: struct {
            insts: ExtraIndex,
        },
    };

    pub const Loc = union {
        node: NodeIndex,
        token: TokenIndex,
    };

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
    return fir.insts.items(.tag)[@intFromEnum(index)];
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
