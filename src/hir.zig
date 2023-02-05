const std = @import("std");
const HirGen = @import("hirgen.zig").HirGen;

pub const Error = error { InvalidRef };
pub const Tag = enum(u8) {
    int,
    float,
    call,
    add,
    sub,
    mul,
    div,
    eq,
    neq,

    fn_decl,

    block,
    if_stmt,
    // TODO: unify-ish
    return_void,
    return_val,
};

pub const Inst = union(Tag) {
    int: u64,
    float: f64,
    call: Inst.Ref,
    add: Inst.Ref,
    sub: Inst.Ref,
    mul: Inst.Ref,
    div: Inst.Ref,
    eq: Inst.Ref,
    neq: Inst.Ref,
    fn_decl: Inst.Ref,
    if_stmt: Inst.Ref,
    block: Inst.Ref,
    return_void: void,
    return_val: Inst.Ref,

    pub const Index = u64;
    pub const ExtraIndex = u64;

    pub fn indexToRef(index: Inst.Index) Inst.Ref {
        const ref_len = @intCast(u32, @typeInfo(Inst.Ref).Enum.fields.len);
        return @intToEnum(Inst.Ref, ref_len + index);
    }

    pub fn refToIndex(ref: Inst.Ref) !Inst.Index {
        const ref_len = @intCast(u64, @typeInfo(Inst.Ref).Enum.fields.len);
        const index = @enumToInt(ref);
        return if (index >= ref_len) index - ref_len else Error.InvalidRef;
    }

    pub const Ref = enum(u64) {
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

        zero,
        one,
        _,
    };

    // pub const Extra = union {
    //     ref: Ref,
    //     extra: ExtraIndex,
    //     str: u64,
    // }; 

    pub const Call = struct {
        addr: Ref,
        args_start: ExtraIndex,
        args_end: ExtraIndex,
    };

    pub const Binary = struct {
        lref: Ref,
        rref: Ref,
    };

    pub const FnDecl = struct {
        params_start: ExtraIndex,
        params_end: ExtraIndex,
        body: Ref,
    };

    pub const Param = struct {
        name: u64,
        ty: Ref,
    };

    pub const ConstDecl = struct {
        name: u64,
        ty: Ref,
    };
    // pub const Arg = struct {
    //     val: Ref,
    // };

    pub const If = struct {
        condition: Ref,
        body: Ref,
    };

    pub const Block = struct {
        insts_start: ExtraIndex,
        insts_end: ExtraIndex,
    };
};

pub const Hir = struct {
    inst: std.ArrayList(Inst).Slice,
    extra_data: []Inst.Ref,

    pub fn extraData(hir: *Hir, index: usize, comptime T: type) T {
        const fields = std.meta.fields(T);
        var result: T = undefined;
        inline for (fields) |field, i| {
            if (field.type == Inst.ExtraIndex) {
                @field(result, field.name) = try HirGen.refToIndex(hir.extra_data[index + i]);
            } else if (field.type == Inst.Ref) {
                @field(result, field.name) = hir.extra_data[index + i];
            } else {
                unreachable;
            }
        }
        return result;
    }
};
