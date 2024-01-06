const std = @import("std");
const lex = @import("lex.zig");
const error_handler = @import("error_handler.zig");

const Lexer = lex.Lexer;
const Token = lex.Token;
pub const Ast = @This();

pub const TokenIndex = u32;
pub const ByteOffset = u32;

// represents the entire, immutable, AST of a source file, once parsed.
// in-progess mutable parsing data is stored in the `Parser` struct in parser.zig
// the AST owns the source, token list, node list, and node extra_data list
source: [:0]const u8,
tokens: TokenList.Slice,
nodes: std.MultiArrayList(Node).Slice,
extra_data: []Node.Index,
errors: []const error_handler.SourceError,

// we store an array of token tags and start locations
// to reference during parsing. AST nodes don't store tokens
// or strings themselves, but rather the index to the relevant
// token(s) in this array. note that tokens themselves don't own
// any strings, but reference character offsets in the global
// source array.
pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: ByteOffset,
    // even though the lexer generates end offsets, storing them
    // is a low return on investment because we don't need them
    // for most tokens (only identifiers, strings, and literals)
    // therefore, we instead re-lex a single token to get the
    // end offset/length as necessary
});

// represents a node in the femto abstract syntax tree.
// due to the u32::max cap on source file size, we can
// also cap the AST at u32::max nodes. hence, nodes are
// inserted into a flat, contigious arraylist. node pointers
// use u32 indices instead of pointer types. this reduces
// memory footprint and improves cache locality
//
// the AST node is a binary tree because the vast majority
// of nodes only need two children (or fewer) and adding more
// would be a waste. nodes that need to reference more children
// use the extra_data array explained below in the `Ast` struct
pub const Node = struct {
    // index to the "main" token representing this node, if applicable.
    // examples include 'fn' for function declarations, `let` for
    // const or var declarations, and operator tokens for unary and
    // binary operations. strings such as identifiers can be extracted
    // from this information using a fixed offset (i.e. +1 for const decls)
    main_token: TokenIndex,

    // up to two u32 child references
    data: Data,

    // each union member can hold up to
    // two u32 child references. these are either
    // `Index` types to index into the nodes array
    // or `ExtraIndex` to index into the extra_data array
    // zig doesn't have distinct integers, so this isn't
    // strictly type safe, but its done for readability
    //
    // extra data indices represent only the "start" of the
    // unpacked extra data struct in the extra_data array
    pub const Data = union(enum) {
        // used for the null node at the beginning
        placeholder,

        // a single "named" thing like a variable, field, or type name
        ident,
        // parentheses around any exprssion
        paren: Index,

        // types
        // pointer type * to an underlying type
        pointer_type: Index,
        mut_pointer_type: Index,
        // pointer type [*] to an underlying type
        many_pointer_type: Index,
        // slice type [] to an underlying type
        slice_type: Index,
        // array type [n] to an underlying type
        array_type: struct {
            element_type: Index,
            count_expr: Index,
        },
        // function type fn (param1: type, ...) type
        function_type: ExtraIndex,
        // struct type
        struct_type: struct {
            fields: ExtraIndex,
        },

        // function declaration 'fn (params...) ret {body}'
        // main_token = n/a
        // proto = FnSignature {} 'fn (params...) ret'
        // body = body block node
        fn_decl: struct {
            signature: ExtraIndex,
            body: Index,
        },
        // function parameter (in the declaration/prototype)
        // as opposed to *arguments* at the call site
        // 'argc: u32'
        // main_token = name
        param: Index,
        // struct field
        field: Index,

        // literals
        // main_token = literal string
        integer_literal: u64,
        // main_token = unparsed literal string
        float_literal,
        // main_token = literal string (true or false)
        bool_literal,
        // main_token = literal string
        char_literal,
        string_literal,
        struct_literal: struct {
            struct_type: Node.Index,
            fields: ExtraIndex,
        },
        field_initializer: Node.Index,
        array_literal: struct {
            elements: ExtraIndex,
        },

        // complex expressions
        // binary expression 'a [+-*/...] b'
        // main_token = operator token
        // left = left side expression node
        // right = right side expression node
        binary: struct {
            left: Index,
            right: Index,
        },
        // unary prefix expression '[+-!~]a'
        unary: Index,

        // unary postfix expressions
        // function call 'foo(1, 2, 3)'
        // main_token = function name
        // args_start = start of argument array
        // args_end = end of argument array
        // TODO: this is very bad, increases union size
        // move to extra data
        call: struct {
            ptr: Index,
            args: ExtraIndex,
        },
        // accesses an array/slice/many pointer element by index
        subscript: struct {
            operand: Index,
            index: Index,
        },
        slice: struct {
            operand: Index,
            range: ExtraIndex,
        },
        // accesses a field by name (identifier)
        // main_token = '.'
        member: Index,

        // declarations
        // constant declaration 'let x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        const_decl: struct {
            ty: Index,
            val: Index,
        },
        // var declaration 'let mut x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        var_decl: struct {
            ty: Index,
            val: Index,
        },
        // type alias 'type Index = u32'
        // main_token = 'type'
        type_decl: Index,
        // type alias 'distinct type Index = u32'
        // main_token = 'distinct'
        distinct_type_decl: Index,

        // statements
        // block '{...}'
        // main_token = '{'
        block: struct {
            stmts: ExtraIndex,
        },

        // attribute without arguments
        attr_simple,
        // attribute with arguments
        // range = argument indices
        attr_args: ExtraIndex,

        // constant declaration with attribute(s) '@export let x[: ty] = ...';
        const_decl_attr: struct {
            metadata: ExtraIndex,
            val: Index,
        },

        // variable assignment 'foo = "bar"'
        // main_token = variable name
        // val = value node
        assign_simple: struct {
            ptr: Index,
            val: Index,
        },

        // variable assignment with operator 'foo += 1'
        // main_token = variable name
        assign_binary: struct {
            ptr: Index,
            val: Index,
        },

        // empty return 'return'
        // main_token = 'return'
        // return_void: void,
        // return value 'return 5'
        // main_token = 'return'
        return_val: Index,
        yield_val: Index,

        // simple if statement 'if cond {body}'
        // main_token = 'if'
        // condition = conditional expression node
        // exec_true = block node to execute on true
        if_simple: struct {
            condition: Index,
            exec_true: Index,
        },

        // if else statement 'if cond {} else {}'
        // main_token = 'if'
        // condition = conditional expression node
        // exec = extra index to body blocks
        if_else: struct {
            condition: Index,
            exec: ExtraIndex,
        },

        // chained if-else if statement 'if cond {} else if cond {}'
        // main_token = 'if'
        // condition = conditional expression node
        // chain = extra index to chain information
        if_chain: struct {
            condition: Index,
            chain: ExtraIndex,
        },

        // forever loop 'for {}'
        // main_token = 'for'
        // body = body block node
        loop_forever: struct {
            body: Index,
        },

        // loop while condition satisfied 'for cond {}'
        // main_token = 'for'
        // condition = conditional expression node
        // body = body block node
        loop_conditional: struct {
            condition: Index,
            body: Index,
        },

        // traditional range loop while condition satisfied
        // with binding and afterthought 'for let mut i = 0; i < 10; i += 1 {}'
        // main_token = 'for'
        // signature = extra index to loop range signature
        loop_range: struct {
            signature: ExtraIndex,
            body: Index,
        },

        // "break" statement to exit a loop early
        @"break",

        // body = body block node
        module: struct {
            stmts: ExtraIndex,
        },
    };

    pub const Index = u32; // index into nodes array
    pub const ExtraIndex = u32; // index into extra_data array

    // represents a contigious range of nodes (subarray)
    pub const Range = struct {
        start: Index,
        end: Index,
    };

    // functionally identical to above, differentiated for clarity
    pub const ExtraSlice = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    // extra data content

    // function signature, excluding return type
    pub const FnSignature = struct {
        params: ExtraIndex,
        return_ty: Index,
    };

    // if else execution information
    // exec_true = block node to execute if condition is met
    // exec_false = block node to execute if condition is not met
    pub const IfElse = struct {
        exec_true: Index,
        exec_false: Index,
    };

    // chained if else if execution information
    // exec_true = block node to execute if condition is met
    // next = next conditional node in the chain (if, if else, or another if chain)
    pub const IfChain = struct {
        exec_true: Index,
        next: Index,
    };

    // range-based for loop signature
    // binding = variable declaration at beginning of loop
    // condition = boolean expression to check loop continuation
    // afterthought = statement executed at the end of each loop iteration
    pub const RangeSignature = struct {
        binding: Index,
        condition: Index,
        afterthought: Index,
    };

    pub const DeclMetadata = struct {
        ty: Index,
        attrs_start: ExtraIndex,
        attrs_end: ExtraIndex,
    };

    pub const GetSlice = struct {
        start: Index,
        end: Index,
    };
};

pub fn extraData(self: *const Ast, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        comptime std.debug.assert(field.type == Node.Index);
        @field(result, field.name) = self.extra_data[index + i];
    }
    return result;
}

pub fn extraSlice(tree: *const Ast, sl: Ast.Node.ExtraSlice) []const u32 {
    const start: u32 = @intCast(sl.start);
    const end: u32 = @intCast(sl.end);
    return tree.extra_data[start..end];
}

pub fn tokenString(tree: *const Ast, index: TokenIndex) []const u8 {
    const tokens = tree.tokens;
    const token_start = tokens.items(.start)[index];
    var lexer = Lexer.init_index(tree.source, token_start);
    const token = lexer.next();

    return tree.source[token.loc.start..token.loc.end];
}

pub fn tokenTag(tree: *const Ast, index: TokenIndex) Token.Tag {
    return tree.tokens.items(.tag)[index];
}

pub fn mainToken(tree: *const Ast, node: Node.Index) TokenIndex {
    return tree.nodes.items(.main_token)[node];
}

pub fn data(tree: *const Ast, node: Node.Index) Node.Data {
    return tree.nodes.items(.data)[node];
}
