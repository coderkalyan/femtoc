const std = @import("std");
const Interner = @import("interner.zig").Interner;
const Ast = @import("Ast.zig");
const TokenIndex = Ast.TokenIndex;
const Value = @import("value.zig").Value;
const Type = @import("hir/type.zig").Type;
const error_handler = @import("error_handler.zig");
const Allocator = std.mem.Allocator;
const render = @import("render.zig");
const InternPool = @import("InternPool.zig");

const Node = Ast.Node;
const ValueHandle = InternPool.ValueHandle;
pub const Error = error{InvalidRef};
pub const Hir = @This();

tree: *const Ast,
insts: std.MultiArrayList(Inst).Slice,
annot: []Inst.Annotation,
module_index: Index,
block_slices: [][]Hir.Index,
extra_data: []const u32,
pool: InternPool.Ptr,
interner: Interner,
untyped_decls: std.AutoHashMapUnmanaged(u32, Hir.Index),
errors: []error_handler.SourceError,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        // literals
        // load an integer literal (immediate)
        // data.int = immediate value
        int,
        // load a float literal (immediate)
        // data.float = immediate value
        float,
        // load a void literal (immediate)
        // data.placeholder = no data
        none,
        // initialize an array from a set of elements
        // data.pl_node.pl = Inst.ArrayInitializer
        // load an array literal [a, b, c...] from a
        // list of element instructions
        // this is an untyped instruction
        array,

        array_init,
        slice_init,
        // declare a new type to be used
        // data.ty = type (stored directly)
        // TODO: why do we need this instead of types[]?
        ty,
        // create a new type thats a pointer to the given type
        // data.operand = type to take a pointer to
        pointer_type,
        function_type,
        array_type,
        many_pointer_type,
        slice_type,
        // fetches the return type of the current function body
        // undefined for toplevel block_inlines
        ret_type,
        param_type,
        element_type,
        // fetches the type of a instruction
        // data.operand = instruction value to get the type of
        type_of,
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
        bitwise_or,
        bitwise_and,
        bitwise_xor,

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

        sl,
        sr,
        lsl,
        lsr,
        asl,
        asr,

        // unary operators
        // data.un_node.operand = operand to use
        neg,
        // TODO: phase this out in codegen (turn it into a cmp)
        bit_not,
        index_ref,
        index_val,
        field_ref,
        field_val,
        slice_ptr_ref,
        slice_len_ref,
        slice_ptr_val,
        slice_len_val,

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
        // data.un_node.operand = value to push
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
        // glo
        global_handle,
        global_set_mutable,
        global_set_init,
        global_set_type,
        // marks a value in a global variable block_inline as externally linked
        // and returns the "modified" instruction
        // data.un_node.operand = variable just marked externally linked
        global_set_linkage_external,

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
        // dbg_value,
        // dbg_declare,
        // dbg_assign,

        // top level instruction storing everything in a module (file) generated
        // data.pl_node.pl = Inst.Module
        module,
    };

    pub const Data = union {
        // int immediate
        int: u64,
        // float immediate
        float: f64,
        // no data stored for this instruction
        placeholder: void,
        // type
        ty: Type,
        // just a node reference, no other data
        node: NodeIndex,
        // just a token reference, no other data
        token: TokenIndex,
        // used for unary operations (single operand) linking to the source node
        un_node: struct {
            // reference to the node creating this instruction
            node: NodeIndex,
            // unary operand reference
            operand: Index,
        },
        // used for unary operations (single operand) linking to the source token
        un_tok: struct {
            // reference to the token creating this instruction
            tok: TokenIndex,
            // unary operand reference
            operand: Index,
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
        ty_op: struct {
            ty: Index,
            operand: Index,
        },
    };

    pub const Call = struct {
        ptr: Index,
        args_start: ExtraIndex,
        args_end: ExtraIndex,
    };

    pub const Binary = struct {
        lref: Index,
        rref: Index,
    };

    pub const FnDecl = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
        return_type: Index,
        body: Index,
        hash_lower: u32,
        hash_upper: u32,
    };

    pub const Param = struct {
        name: u32,
        ty: Index,
    };

    pub const Store = struct {
        ptr: Index,
        val: Index,
    };

    pub const Coerce = struct {
        ty: Index,
        val: Index,
    };

    pub const Extend = struct {
        ty: Index,
        val: Index,
    };

    pub const Constant = struct {
        val: ValueHandle,
        ty: Index,
    };

    pub const BranchSingle = struct {
        condition: Index,
        exec_true: Index,
    };

    pub const BranchDouble = struct {
        condition: Index,
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

    pub const ParamType = struct {
        ptr: Index,
        param_index: u32,
    };

    pub const GlobalOperand = struct {
        handle: Index,
        operand: Index,
    };

    pub const FunctionType = struct {
        return_type: Index,
        params_start: ExtraIndex,
        params_end: ExtraIndex,
    };

    pub const ArrayInitializer = struct {
        elements_start: ExtraIndex,
        elements_end: ExtraIndex,
    };

    pub const ArrayType = struct {
        element_type: Index,
        count: Index,
    };

    pub const SliceInit = struct {
        element_type: Index,
        ptr: Index,
        len: Index,
    };

    pub const ArrayAccess = struct {
        array: Index,
        index: Index,
    };
    // pub const DebugValue = struct {
    //     name: u32,
    //     value: Ref,
    // };

    pub const Annotation = union {
        node: u32,
        token: u32,
    };
};

pub const Index = u32;
pub const NodeIndex = u32;
pub const ExtraIndex = u32;

pub fn activeDataField(comptime tag: Inst.Tag) std.meta.FieldEnum(Inst.Data) {
    return switch (tag) {
        .int => .int,
        .float => .float,
        .none,
        .loop_break,
        => .placeholder,
        .ty => .ty,
        .global_handle, .ret_type => .node,
        .constant,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
        .cmp_eq,
        .cmp_ne,
        .cmp_gt,
        .cmp_ge,
        .cmp_lt,
        .cmp_le,
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
        .sl,
        .sr,
        .lsl,
        .lsr,
        .asl,
        .asr,
        .coerce,
        .zext,
        .sext,
        .fpext,
        .store,
        .fn_decl,
        .param,
        .call,
        .block,
        .block_inline,
        .branch_single,
        .branch_double,
        .loop,
        .module,
        .param_type,
        .global_set_init,
        .global_set_type,
        .function_type,
        .array_init,
        .slice_init,
        .array,
        .array_type,
        .index_ref,
        .index_val,
        => .pl_node,
        .neg,
        .bit_not,
        .load,
        .global_set_mutable,
        .global_set_linkage_external,
        .ret_node,
        .yield_node,
        .yield_inline,
        .load_global,
        .pointer_type,
        .many_pointer_type,
        .slice_type,
        .type_of,
        .field_ref,
        .field_val,
        .slice_ptr_ref,
        .slice_len_ref,
        .slice_ptr_val,
        .slice_len_val,
        .element_type,
        => .un_node,
        .ret_implicit,
        .yield_implicit,
        => .un_tok,
        .alloca, .push => .ty_op,
    };
}

pub fn payloadType(comptime tag: Inst.Tag) type {
    return switch (tag) {
        .constant => Inst.Constant,
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
        .cmp_eq,
        .cmp_ne,
        .cmp_gt,
        .cmp_ge,
        .cmp_lt,
        .cmp_le,
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
        .sl,
        .sr,
        .lsl,
        .lsr,
        .asl,
        .asr,
        => Inst.Binary,
        .coerce => Inst.Coerce,
        .zext,
        .sext,
        .fpext,
        => Inst.Extend,
        .store => Inst.Store,
        .fn_decl => Inst.FnDecl,
        .param => Inst.Param,
        .call => Inst.Call,
        .block,
        .block_inline,
        => Inst.Block,
        .branch_single => Inst.BranchSingle,
        .branch_double => Inst.BranchDouble,
        .loop => Inst.Loop,
        .module => Inst.Module,
        .param_type => Inst.ParamType,
        .global_set_init,
        .global_set_type,
        => Inst.GlobalOperand,
        .function_type => Inst.FunctionType,
        .array_init, .array => Inst.ArrayInitializer,
        .slice_init => Inst.SliceInit,
        .array_type => Inst.ArrayType,
        .index_ref, .index_val => Inst.ArrayAccess,
        else => {
            @compileLog(tag);
            unreachable;
        },
    };
}

pub fn extraData(hir: *const Hir, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields, 0..) |field, i| {
        @field(result, field.name) = switch (field.type) {
            u32 => hir.extra_data[index + i],
            else => unreachable,
        };
    }
    return result;
}

pub fn InstData(comptime tag: Inst.Tag) type {
    const active_field = comptime activeDataField(tag);
    switch (active_field) {
        .placeholder => return void,
        .int,
        .float,
        .ty,
        .node,
        .token,
        => {
            // construct an anonymous struct with just a single
            // field, with the same name as this tag
            const field_type = std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
            const anon_fields = &.{.{
                .name = @tagName(active_field),
                .type = field_type,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(field_type),
            }};
            return @Type(std.builtin.Type{ .Struct = .{
                .layout = .Auto,
                .fields = anon_fields,
                .decls = &.{},
                .is_tuple = false,
            } });
        },
        .un_node,
        .un_tok,
        => {
            // no extra data associated with this, so just return the
            // data type of the active field
            // no need to generate a type since it already exists
            return std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
        },
        .ty_op => {
            // no extra data associated with this, so just return the
            // data type of the active field
            // no need to generate a type since it already exists
            return std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
        },
        .pl_node => {
            // construct a type which has all fields from the associated
            // payload struct, plus the node reference from the pl_node
            const payload_type = payloadType(tag);
            const payload_fields = std.meta.fields(payload_type);
            var anon_fields = [1]std.builtin.Type.StructField{undefined} ** (payload_fields.len + 2);
            anon_fields[0] = .{
                .name = "node",
                .type = NodeIndex,
                .default_value = null,
                .is_comptime = false,
                .alignment = @alignOf(NodeIndex),
            };
            anon_fields[1] = .{
                .name = "pl",
                .type = u32,
                .default_value = &@as(u32, 0),
                .is_comptime = false,
                .alignment = @alignOf(u32),
            };
            for (payload_fields, 2..) |field, i| {
                anon_fields[i] = field;
            }
            return @Type(std.builtin.Type{ .Struct = .{
                .layout = .Auto,
                .fields = &anon_fields,
                .decls = &.{},
                .is_tuple = false,
            } });
        },
        .pl_tok => unreachable,
    }
}

pub fn get(hir: *const Hir, index: Index, comptime tag: Inst.Tag) InstData(tag) {
    const data = hir.insts.items(.data)[index];
    const active_field = comptime activeDataField(tag);
    switch (active_field) {
        .placeholder => return {},
        .int,
        .float,
        .ty,
        .node,
        .token,
        => {
            // construct an anonymous struct with just a single
            // field, with the same name as this tag
            var result: InstData(tag) = undefined;
            const field_name = @tagName(active_field);
            @field(result, field_name) = @field(data, field_name);
            return result;
        },
        .un_node,
        .un_tok,
        => {
            var result: InstData(tag) = undefined;
            const data_type = std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
            const source_data = @field(data, @tagName(active_field));
            const fields = std.meta.fields(data_type);
            inline for (fields) |field| {
                @field(result, field.name) = @field(source_data, field.name);
            }
            return result;
        },
        .ty_op => {
            var result: InstData(tag) = undefined;
            const data_type = std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
            const source_data = @field(data, @tagName(active_field));
            const fields = std.meta.fields(data_type);
            inline for (fields) |field| {
                @field(result, field.name) = @field(source_data, field.name);
            }
            // result.node = hir.annot[index].node;
            return result;
        },
        .pl_node => {
            var result: InstData(tag) = undefined;
            result.pl = data.pl_node.pl;
            result.node = data.pl_node.node;
            const payload_type = payloadType(tag);
            const payload = hir.extraData(data.pl_node.pl, payload_type);
            const fields = std.meta.fields(payload_type);
            inline for (fields) |field| {
                @field(result, field.name) = @field(payload, field.name);
            }
            return result;
        },
        .pl_tok => unreachable,
    }
}

// recursively resolves the type of a hir
// most instructions don't store their type, this is reserved
// for certain types like constants (that are typed values)
// but in most other cases, the type can be trivially determined
// either by the type of instruction (comparison => u1),
// signature (call is the return type of the address),
// or by recursively resolving the operand type (arithmetic)
pub fn resolveType(hir: *const Hir, gpa: Allocator, index: Index) error{OutOfMemory}!Type {
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
        .bit_not,
        .ret_node,
        .yield_node,
        .yield_inline,
        .type_of,
        => hir.resolveType(gpa, data.un_node.operand),
        .yield_implicit,
        .ret_implicit,
        => hir.resolveType(gpa, data.un_tok.operand),
        .push,
        .alloca,
        => {
            const pointee = try hir.resolveType(gpa, data.un_node.operand);
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
        .bitwise_or,
        .bitwise_and,
        .bitwise_xor,
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
            const pointer = try hir.resolveType(gpa, data.un_node.operand);
            switch (pointer.kind()) {
                .pointer => {
                    const inner = pointer.extended.cast(Type.Pointer).?;
                    break :ty inner.pointee;
                },
                .many_pointer => {
                    const inner = pointer.extended.cast(Type.ManyPointer).?;
                    break :ty inner.pointee;
                },
                .array => {
                    const inner = pointer.extended.cast(Type.Array).?;
                    break :ty inner.element;
                },
                else => |kind| {
                    std.log.err("tried to load invalid type {}\n", .{kind});
                    unreachable;
                },
            }
        },
        // parameters reference their type
        .param => ty: {
            const param_data = hir.extraData(data.pl_node.pl, Hir.Inst.Param);
            break :ty hir.resolveType(gpa, param_data.ty);
        },
        // resolve the return type of the address being called
        .call => ty: {
            const call_data = hir.extraData(data.pl_node.pl, Hir.Inst.Call);
            const call_ty = try hir.resolveType(gpa, call_data.ptr);
            const fn_type = call_ty.extended.cast(Type.Function).?;
            break :ty fn_type.return_type;
        },
        .index_ref => ty: {
            const array_access = hir.get(index, .index_ref);
            const ty = try hir.resolveType(gpa, array_access.array);
            const pointer_type = ty.extended.cast(Type.Pointer).?;
            const array_type = pointer_type.pointee.extended.cast(Type.Array).?;

            const inner = try gpa.create(Type.Pointer);
            inner.* = .{ .pointee = array_type.element };
            break :ty .{ .extended = &inner.base };
        },
        .index_val => {
            const array_access = hir.get(index, .index_val);
            const ty = try hir.resolveType(gpa, array_access.array);
            const pointer_type = ty.extended.cast(Type.Pointer).?;
            const array_type = pointer_type.pointee.extended.cast(Type.Array).?;
            return array_type.element;
        },
        .slice_ptr_ref => {
            const slice_ptr = hir.get(index, .slice_ptr_ref);
            const ty = try hir.resolveType(gpa, slice_ptr.operand);
            const pointer_type = ty.extended.cast(Type.Pointer).?;
            const slice_type = pointer_type.pointee.extended.cast(Type.Slice).?;
            const ptr = try Type.ManyPointer.init(gpa, slice_type.element);
            return Type.Pointer.init(gpa, ptr);
        },
        .slice_len_ref => return Type.Pointer.init(gpa, Type.Common.u64_type),
        .slice_ptr_val => {
            const slice_ptr = hir.get(index, .slice_ptr_val);
            const ty = try hir.resolveType(gpa, slice_ptr.operand);
            const pointer_type = ty.extended.cast(Type.Pointer).?;
            const slice_type = pointer_type.pointee.extended.cast(Type.Slice).?;
            return Type.ManyPointer.init(gpa, slice_type.element);
        },
        .slice_len_val => Type.Common.u64_type,
        // resolve the type being extended to
        .zext,
        .sext,
        .fpext,
        => ty: {
            const ext_data = hir.extraData(data.pl_node.pl, Hir.Inst.Extend);
            break :ty hir.resolveType(gpa, ext_data.ty);
        },
        // resolve the last instruction in the block
        .block => ty: {
            const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);
            const insts = hir.block_slices[block_data.head];
            break :ty hir.resolveType(gpa, insts[insts.len - 1]);
        },
        // TODO: this won't be generic to local inline_blocks if those
        // exist in the future
        .block_inline => ty: {
            const block_data = hir.extraData(data.pl_node.pl, Hir.Inst.Block);
            const insts = hir.block_slices[block_data.head];
            const yield = hir.get(insts[insts.len - 1], .yield_inline);
            const handle = yield.operand;

            // find a global_set_type that references this handle
            for (insts) |block_inst| {
                if (hir.insts.items(.tag)[block_inst] == .global_set_type) {
                    const set_type = hir.get(block_inst, .global_set_type);
                    if (set_type.handle == handle) {
                        break :ty hir.resolveType(gpa, set_type.operand);
                    }
                }
            }

            // uh oh, bad IR
            unreachable;
        },
        .array_init => ty: {
            const array_init = hir.extraData(data.pl_node.pl, Hir.Inst.ArrayInitializer);
            const len = array_init.elements_end - array_init.elements_start;
            const first_element = hir.extra_data[array_init.elements_start];
            const element_type = try hir.resolveType(gpa, first_element);

            const inner = try gpa.create(Type.Array);
            inner.* = .{ .count = @intCast(len), .element = element_type };
            break :ty .{ .extended = &inner.base };
        },
        .array => ty: {
            const array_init = hir.extraData(data.pl_node.pl, Hir.Inst.ArrayInitializer);
            const len = array_init.elements_end - array_init.elements_start;

            const inner = try gpa.create(Type.ComptimeArray);
            inner.* = .{ .count = @intCast(len) };
            break :ty .{ .extended = &inner.base };
        },
        .slice_init => ty: {
            const slice_init = hir.extraData(data.pl_node.pl, Hir.Inst.SliceInit);
            break :ty hir.resolveType(gpa, slice_init.element_type);
        },
        .branch_double => ty: {
            const branch_double = hir.extraData(data.pl_node.pl, Hir.Inst.BranchDouble);
            break :ty hir.resolveType(gpa, branch_double.exec_true);
        },
        .branch_single,
        .loop,
        .loop_break,
        => unreachable, // TODO: follow into the block
        .load_global => ty: {
            const pl = hir.insts.items(.data)[index].un_node.operand;
            const inst = hir.untyped_decls.get(pl).?;
            break :ty hir.resolveType(gpa, inst);
        },
        // should not be referenced (don't return anything meaningful)
        // .dbg_value,
        // .dbg_declare,
        // .dbg_assign,
        .store,
        .global_set_mutable,
        .global_set_init,
        .global_set_type,
        .global_handle,
        .global_set_linkage_external,
        => |tag| {
            std.log.err("hir: encountered illegal instruction {} while resolving type\n", .{tag});
            unreachable;
        },
        // untyped instructions should be replaced before they're referred to
        .int,
        .float,
        .cmp_eq,
        .cmp_ne,
        .cmp_gt,
        .cmp_ge,
        .cmp_lt,
        .cmp_le,
        .sl,
        .sr,
        .coerce,
        .fn_decl,
        .module,
        .pointer_type,
        .param_type,
        .function_type,
        .array_type,
        .many_pointer_type,
        .slice_type,
        .ret_type,
        .field_ref,
        .field_val,
        .element_type,
        => |tag| {
            std.log.err("hir: encountered illegal instruction {} while resolving type\n", .{tag});
            const out = std.io.getStdOut();
            var buffered_out = std.io.bufferedWriter(out.writer());
            var writer = buffered_out.writer();
            var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, gpa, hir);
            hir_renderer.render() catch unreachable;
            buffered_out.flush() catch unreachable;
            unreachable;
        },
    };
}

pub fn instToInt(hir: *const Hir, index: Index) u64 {
    const pl = hir.insts.items(.data)[index].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const val = hir.pool.getValue(data.val);
    return val.integer;
}

pub fn instToFloat(hir: *const Hir, inst: Hir.Index) f64 {
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    return @bitCast(hir.pool.getValue(data.val).float);
}
