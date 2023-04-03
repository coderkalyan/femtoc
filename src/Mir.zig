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
        loop_break,
        loop_continue,

        dbg_value,
        dbg_declare,
        dbg_assign,

        zext,
        sext,
        fpext,
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

    u8,
    u16,
    u32,
    u64,
    u1,
    i8,
    i16,
    i32,
    i64,
    comptime_uint,
    comptime_sint,
    f32,
    f64,
    comptime_float,
    void,

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
        @field(result, field.name) = switch (field.field_type) {
            u32 => mir.extra[index + i],
            Ref => @intToEnum(Ref, mir.extra[index + i]),
            else => unreachable,
        };
    }
    return result;
}

pub fn resolveType(mir: *const Mir, ref: Mir.Ref) Type {
    if (Mir.refToIndex(ref)) |index| {
        const data = mir.insts.items(.data)[index];
        return switch (mir.insts.items(.tag)[index]) {
            .ty => data.ty,
            .constant => mir.resolveType(data.ty_pl.ty),
            .add,
            .sub,
            .mul,
            .div,
            .mod => mir.resolveType(data.bin_op.lref),
            .cmp_eq,
            .cmp_ne,
            .cmp_uge,
            .cmp_ule,
            .cmp_ugt,
            .cmp_ult,
            .cmp_sge,
            .cmp_sle,
            .cmp_sgt,
            .cmp_slt,
            .cmp_fge,
            .cmp_fle,
            .cmp_fgt,
            .cmp_flt => Type.initInt(1, false),
            .alloc,
            .load,
            .store => mir.resolveType(data.un_op),
            .param => mir.resolveType(data.ty_pl.ty),
            .load_decl => mir.comp.declPtr(data.pl).ty,
            .call => ty: {
                const ty = mir.resolveType(data.op_pl.op);
                const fn_type = ty.extended.cast(Type.Function).?;
                break :ty fn_type.return_type;
            },
            .zext,
            .sext,
            .fpext => mir.resolveType(data.ty_op.ty),
            .block,
            .branch_single,
            .branch_double,
            .loop => Type.initVoid(), // TODO
            .yield => mir.resolveType(data.un_op),
            .dbg_value,
            .dbg_declare,
            .dbg_assign => unreachable, // should not be referenced
            .ret,
            .loop_break,
            .loop_continue => unreachable, // always end a block, can't be referenced
        };
    } else {
        return switch (ref) {
            .u1 => Type.initInt(1, false),
            .i8 => Type.initInt(8, true),
            .u8 => Type.initInt(8, false),
            .i16 => Type.initInt(16, true),
            .u16 => Type.initInt(16, false),
            .i32 => Type.initInt(32, true),
            .u32 => Type.initInt(32, false),
            .i64 => Type.initInt(64, true),
            .u64 => Type.initInt(64, false),
            .f32 => Type.initFloat(32),
            .f64 => Type.initFloat(64),
            .void,
            .void_val => Type.initVoid(),
            .zero_val,
            .one_val,
            .comptime_uint => Type.initComptimeInt(false),
            .comptime_sint => Type.initComptimeInt(true),
            .comptime_float => Type.initComptimeFloat(),
            _ => unreachable,
        };
    }
}

pub fn resolveValue(mir: *const Mir, index: u32) Value {
    switch (mir.insts.items(.tag)[index]) {
        .constant => return mir.values[mir.insts.items(.data)[index].ty_pl.pl],
        else => unreachable, // not yet implemented, maybe unecessary?
    }
}

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
                    const ty = mir.resolveType(data.ty);
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

pub fn refToFloat(mir: *const Mir, ref: Mir.Ref) f64 {
    const index = Mir.refToIndex(ref).?;
    const pl = mir.insts.items(.data)[index].ty_pl.pl;
    const val = mir.values[pl];
    const float = val.payload.cast(Value.Payload.F64).?;
    return float.float;
}
