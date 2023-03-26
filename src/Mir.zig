const std = @import("std");
const Type = @import("typing.zig").Type;
const NodeIndex = @import("Ast.zig").Node.Index;
const Interner = @import("interner.zig").Interner;
const Value = @import("value.zig").Value;
const Compilation = @import("Compilation.zig");

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
comp: *Compilation,

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        block,

        constant, // data contains type
        ty,

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
        load_decl,

        call,
        branch_single,
        branch_double,
        ret,
        yield,
        loop,

        dbg_value,
        dbg_declare,
        dbg_assign,

        zext,
        sext,
        fpext,

        global_val,
        global_ptr,
    };

    pub const Data = union {
        ty_pl: struct {
            ty: Ref,
            // payload index in another array (like extra or values)
            pl: u32,
        },
        op_pl: struct {
            op: Ref,
            pl: u32,
        },
        ty_op: struct {
            ty: Ref,
            op: Ref,
        },
        un_op: Ref,
        bin_op: struct {
            lref: Ref,
            rref: Ref,
        },
        ty: Type,
        pl: u32,
        bin_pl: struct {
            l: u32,
            r: u32,
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

    pub const Function = struct {
        // name: u32,
        mir_index: u32,
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
    u1_ty,
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

pub fn refToType(mir: *const Mir, ref: Ref) Type {
    if (refToIndex(ref)) |index| {
        std.debug.assert(mir.insts.items(.tag)[index] == .ty);
        return mir.insts.items(.data)[index].ty;
    } else {
        return switch (ref) {
            .u1_ty => Type.initInt(1, false),
            .i8_ty => Type.initInt(8, true),
            .u8_ty => Type.initInt(8, false),
            .i16_ty => Type.initInt(16, true),
            .u16_ty => Type.initInt(16, false),
            .i32_ty => Type.initInt(32, true),
            .u32_ty => Type.initInt(32, false),
            .i64_ty => Type.initInt(64, true),
            .u64_ty => Type.initInt(64, false),
            .f32_ty => Type.initFloat(32),
            .f64_ty => Type.initFloat(64),
            .void_ty => Type.initVoid(),
            .zero_val, .one_val,
            .void_val => unreachable,
            _ => unreachable,
        };
    }
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

pub const Module = struct {
    len: u32,
};

pub fn resolveTy(mir: *const Mir, ref: Mir.Ref) Type {
    if (Mir.refToIndex(ref)) |index| {
        const data = mir.insts.items(.data)[index];
        return switch (mir.insts.items(.tag)[index]) {
            .constant => mir.refToType(data.ty_pl.ty),
            .add, .sub, .mul, .div, .mod => mir.resolveTy(data.bin_op.lref),
            .cmp_eq, .cmp_ne,
            .cmp_uge, .cmp_ule, .cmp_ugt, .cmp_ult,
            .cmp_sge, .cmp_sle, .cmp_sgt, .cmp_slt,
            .cmp_fge, .cmp_fle, .cmp_fgt, .cmp_flt => Type.initInt(1, false),
            .alloc => data.ty,
            .load => mir.resolveTy(data.un_op),
            .store => mir.resolveTy(data.un_op),
            .param => mir.refToType(data.ty_pl.ty),
            .load_decl => mir.comp.declPtr(data.pl).ty,
            .call => ty: {
                const ty = mir.resolveTy(data.op_pl.op);
                const fn_type = ty.extended.cast(Type.Function).?;
                break :ty fn_type.return_type;
            },
            .zext, .sext, .fpext => mir.refToType(data.ty_op.ty),
            else => {
                std.debug.print("{}\n", .{mir.insts.items(.tag)[index]});
                unreachable;
            },
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => Type.initComptimeInt(false),
            .void_val => Type.initVoid(),
            else => unreachable,
            // else => error.NotImplemented,
        };
    }
}

// TODO: merge with plToInt
pub fn refToInt(mir: *const Mir, ref: Mir.Ref) u64 {
    switch (ref) {
        .zero_val => return 0,
        .one_val => return 1,
        else => {
            const index = Mir.refToIndex(ref).?;
            const data = mir.insts.items(.data)[index].ty_pl;
            const val = mir.values[data.pl];
            switch (val.kind()) {
                .zero => return 0,
                .one => return 1,
                .u32 => {
                    const payload = val.payload.cast(Value.Payload.U32).?;
                    const ty = mir.resolveTy(data.ty);
                    if (ty.intSign()) {
                        // interpret payload as i32, sign extend it to i64,
                        // then reinterpret as u64 to return
                        // TODO: probably more readable to implement and use alu.sext
                        return @bitCast(u64, @intCast(i64, @bitCast(i32, payload.int)));
                    } else {
                        return @intCast(u64, payload.int);
                    }
                },
                .u64 => {
                    const payload = val.payload.cast(Value.Payload.U64).?;
                    return payload.int;
                },
                else => unreachable,
            }
        },
    }
}

pub fn valToInt(mir: *const Mir, val_pl: u32) u64 {
    const val = mir.values[val_pl];
    switch (val.kind()) {
        .zero => return 0,
        .one => return 1,
        .u32 => {
            const payload = val.payload.cast(Value.Payload.U32).?;
            return @intCast(u64, payload.int);
        },
        .u64 => {
            const payload = val.payload.cast(Value.Payload.U64).?;
            return payload.int;
        },
        else => unreachable,
    }
}

pub fn refToFloat(mir: *const Mir, ref: Mir.Ref) f64 {
    const index = Mir.refToIndex(ref).?;
    const pl = mir.insts.items(.data)[index].ty_pl.pl;
    return mir.valToFloat(pl);
}


pub fn valToFloat(mir: *const Mir, val_pl: u32) f64 {
    const val = mir.values[val_pl];
    const f = val.payload.cast(Value.Payload.F64).?;
    return f.float;
}
