const std = @import("std");
const lex = @import("lex.zig");
const Token = lex.Token;

pub const Error = error { InvalidNode };
pub const TokenIndex = u32;
pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: ByteOffset,
});

pub const Node = struct {
    main_token: TokenIndex,
    data: Data,

    pub const Tag = enum(u32) {
        named_ty,
        fn_decl,
        fn_proto,
        param,

        integer_literal,
        float_literal,
        binary_expr,
        var_expr,
        call_expr,

        ty_decl,

        block,
        const_decl,
        var_decl,
        return_void,
        return_val,
        if_stmt,

        // use,
        // struct_proto,
    };

    pub const Data = union(Tag) {
        // types
        named_ty: void,
        fn_decl: struct {
            proto: Index,
            body: Index,
        },
        fn_proto: struct {
            params: ExtraIndex,
            return_ty: Index,
        },
        param: struct {
            ty: Index,
        },

        // expressions
        integer_literal: void,
        float_literal: void,
        binary_expr: struct {
            left: Index,
            right: Index,
        },
        var_expr: void,
        call_expr: struct {
            args_start: Index,
            args_end: Index,
        },

        // declarations
        ty_decl: struct {
            ty: Index,
        },

        // statements
        block: struct {
            stmts_start: ExtraIndex,
            stmts_end: ExtraIndex,
        },
        const_decl: struct {
            ty: Index,
            val: Index,
        },
        var_decl: struct {
            ty: Index,
            val: Index,
        },
        return_void: void,
        return_val: struct {
            val: Index,
        },
        if_stmt: struct {
            condition: Index,
            body: Index,
        },
    };

    pub const Index = u32;
    pub const ExtraIndex = u32;

    pub const Range = struct {
        start: Index,
        end: Index,
    };

    // functionally identical to above, differentiated for clarity
    pub const ExtraRange = struct {
        start: ExtraIndex,
        end: ExtraIndex,
    };

    pub const FnProto = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
    };

    // pub const GlobalDecl = struct {
    //     mutable: bool,
    //     ident: []u8,
    //     type_node: Index,
    //     value_node: Index,
    // };
};

pub const Ast = struct {
    source: [:0]const u8,
    tokens: TokenList.Slice,
    nodes: std.MultiArrayList(Node).Slice,
    extra_data: []Node.Index,
    //errors: []const Error,

    pub fn extraData(self: *const Ast, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            comptime std.debug.assert(field.field_type == Node.Index);
            @field(result, field.name) = self.extra_data[index + i];
        }
        return result;
    }

    pub fn parseGlobalDecl(self: *Ast, index: usize) !Node.GlobalDecl {
        if (self.nodes(.tag)[index] != .global_decl) return Error.InvalidNode;

        const main_token = self.nodes(.main_token)[index];
        const data = self.nodes(.data)[index];
        const mutable = self.tokens[main_token + 1] == .k_mut;
        return .{
            .mutable = mutable,
            .ident = if (mutable) self.tokens[main_token + 2] else self.tokens[main_token + 1],
            .type_node = data.l,
            .value_node = data.r,
        };
    }
};
