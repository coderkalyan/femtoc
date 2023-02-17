const std = @import("std");
const Type = @import("typing.zig").Type;

// const Interner = @import("interner.zig").Interner;
const Mir = @This();
pub const Error = error { NotImplemented };

insts: std.MultiArrayList(Inst).Slice,
extra: []const u32,
values: []const Value,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        constant, // data contains type

        add,
        sub,
        mul,
        div,
        mod,

        eq,
        neq,
        geq,
        leq,
        gt,
        lt,

        alloc,
        load,
        store,
        param,

        call,
        br,
        condbr,
        ret,
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
        un_op: struct {
            op: Ref,
        },
        bin_op: struct {
            lref: Ref,
            rref: Ref,
        },
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
};

pub const Index = u32;
pub const Ref = enum(u32) {
    izero_val,
    ione_val,
    void_val,

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

pub const Value = union {
    int: u64,
    float: f64,
    payload: *Payload,

    pub const Payload = struct {
        tag: Tag,

        pub const Tag = enum {
            reference, // dummy not fleshed out yet
        };
    };
};

pub const TypedValue = struct {
    ty: Type,
    val: Value,
};
