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
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Data = struct {
        l: Index,
        r: Index,
    };

    pub const Index = u32;

    pub const Tag = enum {
        use,
        type_decl,
        const_decl,
        struct_proto,
        fn_proto,
        fn_decl,
        ret_stmt,
        block,
        add,
        sub,
        mul,
        div,
        int_lit,
        float_lit,
        ident_expr,
        call_expr,
        bin_expr,
        if_stmt,
    };

    pub const Range = struct {
        start: Index,
        end: Index,
    };

    pub const FnProto = struct {
        params_start: Index,
        params_end: Index,
    };

    // pub const GlobalDecl = struct {
    //     mutable: bool,
    //     ident: []u8,
    //     type_node: Index,
    //     value_node: Index,
    // };
    //
    // pub const FnProto = struct {
    //     params_start: Index,
    //     params_end: Index,
    //     return_type: Index,
    // };
};

pub const Ast = struct {
    source: [:0]const u8,
    tokens: TokenList.Slice,
    nodes: std.MultiArrayList(Node).Slice,
    extra_data: []Node.Index,
    //errors: []const Error,

    pub fn extraData(self: *Ast, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            comptime std.debug.assert(field.type == Node.Index);
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
