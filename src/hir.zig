const std = @import("std");

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        int,
        float,
        arg,
        ret,
        decl_var,
        add,
        sub,
        mul,
        div,
        call,
        param,
        branch,
    };

    pub const Data = union {
        operand: ValRef,
        int: u64,
        float: f64,
        val_ref: ValRef,
        arg_index: u64, // no more than u8 in practice but kept for consistency
        // ty_ref: TyRef,
        extra_index: u64,
    };

    pub const Index = u64;

    pub const ValRef = enum(u64) {
        zero,
        one,
        _,
    };

    pub const TyRef = enum(u64) {
        u8,
        u16,
        u32,
        u64,
        i8,
        i16,
        i32,
        i64,
        f32,
        f64,
        bool,
        infer,
    };

    pub const Ref = union {
        val: ValRef,
        ty: TyRef,
        str: u64,
    };

    pub const Binary = struct {
        lref: ValRef,
        rref: ValRef,
    };

    pub const Call = struct {
        addr: ValRef,
        args_start: ValRef,
        args_end: ValRef,
    };

    pub const Arg = struct {
        val: ValRef,
    };

    pub const Branch = struct {
        cond: ValRef,
        addr: ValRef,
    };

    pub const Param = struct {
        name: u64,
        ty: TyRef,
    };
};

pub const Hir = struct {
    inst: std.MultiArrayList(Inst).Slice,
    extra_data: []Inst.Ref,

    pub fn extraData(hir: *Hir, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            if (field.type == Inst.ValRef) {
                @field(result, field.name) = hir.extra_data[index + i].val;
            } else if (field.type == Inst.TyRef) {
                @field(result, field.name) = hir.extra_data[index + i].ty;
            } else if (field.type == u64) {
                @field(result, field.name) = hir.extra_data[index + i].str;
            } else {
                unreachable;
            }
        }
        return result;
    }
};
