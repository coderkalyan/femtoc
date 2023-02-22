const std = @import("std");
const Type = @import("typing.zig").Type;

// const Interner = @import("interner.zig").Interner;
const Mir = @This();
pub const Error = error { NotImplemented };

insts: std.MultiArrayList(Inst).Slice,
extra: []const u32,
values: []const Value,
blocks: []const u32,
entry: u32,

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

    // pub const Block = struct {
    //     insts_len: u32,
    // };
};

pub const Index = u32;
pub const Ref = enum(u32) {
    zero_val,
    one_val,
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
