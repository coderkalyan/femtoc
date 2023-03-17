const std = @import("std");
const Type = @import("typing.zig").Type;
const NodeIndex = @import("Ast.zig").Node.Index;
const Interner = @import("interner.zig").Interner;

// const Interner = @import("interner.zig").Interner;
const Mir = @This();
pub const Error = error { NotImplemented };
pub const UserError = struct {
    node: NodeIndex,
    tag: Tag,

    pub const Tag = enum {
        uint_overflow,
        uint_sign_cast_overflow,
        sint_underflow,
        sint_flip_overflow,
        divisor_zero,
        coerce_unsigned_signed,
        coerce_overflow,
        coerce_underflow,
    };
};

insts: std.MultiArrayList(Inst).Slice,
extra: []const u32,
values: []const Value,
interner: *const Interner,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        module,
        function,
        block,

        constant, // data contains type

        add,
        sub,
        mul,
        div,
        mod,

        cmp_eq,
        cmp_ne,
        cmp_uge,
        cmp_ule,
        cmp_ugt,
        cmp_ult,
        cmp_sge,
        cmp_sle,
        cmp_sgt,
        cmp_slt,
        cmp_fge,
        cmp_fle,
        cmp_fgt,
        cmp_flt,

        alloc,
        load,
        store,
        param,

        call,
        branch_single,
        branch_double,
        ret,
        yield,
        loop,
    };

    pub const Data = union {
        ty_pl: struct {
            ty: Type,
            // payload index in another array (like extra or values)
            pl: u32,
        },
        op_pl: struct {
            op: Ref,
            pl: u32,
        },
        ty_op: struct {
            ty: Type,
            op: Ref,
        },
        un_op: Ref,
        bin_op: struct {
            lref: Ref,
            rref: Ref,
        },
        ty: Type,
        pl: u32,
    };

    pub fn typeOf(mir: *Mir, ref: Ref) Type {
        const index = Mir.refToIndex(ref);
        const data = mir.inst.items(.data)[index];
        
        return switch (mir.inst.items(.tag)[index]) {
            .constant => data.ty_pl.ty,
            .add, .sub, .mul, .div => mir.typeOf(data.bin_op.lref),
        };
    }

    pub const Call = struct {
        args_len: u32,
    };

    pub const CondBr = struct {
        exec_true: u32,
        exec_false: u32,
    };

    pub const Loop = struct {
        condition: u32,
        body: u32,
    };

    pub const Block = struct {
        insts_len: u32,
    };
};

pub const Index = u32;
pub const Ref = enum(u32) {
    zero_val,
    one_val,
    void_val,

    u8_ty,
    u16_ty,
    u32_ty,
    u64_ty,
    i1_ty,
    i8_ty,
    i16_ty,
    i32_ty,
    i64_ty,
    f32_ty,
    f64_ty,
    void_ty,

    _,
};

pub fn indexToRef(index: Index) Ref {
    const ref_len = @intCast(u32, @typeInfo(Ref).Enum.fields.len);
    return @intToEnum(Ref, ref_len + index);
}

pub fn refToIndex(ref: Ref) ?Index {
    const ref_len = @intCast(u32, @typeInfo(Ref).Enum.fields.len);
    const index = @enumToInt(ref);
    return if (index >= ref_len) index - ref_len else null;
}

pub fn extraData(mir: *const Mir, index: usize, comptime T: type) T {
    const fields = std.meta.fields(T);
    var result: T = undefined;
    inline for (fields) |field, i| {
        if (field.field_type == u32) {
            @field(result, field.name) = mir.extra[index + i];
        } else if (field.field_type == Ref) {
            @field(result, field.name) = @intToEnum(Ref, mir.extra[index + i]);
        }
    }
    return result;
}

pub const Function = struct {
    len: u32,
};

pub const Module = struct {
    len: u32,
};

pub const Value = union {
    int: u64,
    float: f64,
    payload: *Payload,

    pub const Payload = struct {
        tag: Tag,

        pub const Tag = enum {
            reference, // dummy not fleshed out yet
            function,
        };
    };
};

pub const TypedValue = struct {
    ty: Type,
    val: Value,

    pub fn coerces(tv: *TypedValue, ty: Type) bool {
        // TODO: payload types
        switch (tv.ty.tag) {
            .comptime_int => {
                _ = ty;
                return true;
            },
            else => return false,
        }
    }
};
