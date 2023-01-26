const std = @import("std");

pub const Inst = struct {
    tag: Tag,
    data: Data,

    pub const Tag = enum(u8) {
        constant,
        arg,
        ret,
        decl_var,
        add,
        sub,
        mul,
        div,
    };

    pub const Data = union {
        int: u64,
        ref: ValRef,
        un_inst: u32,
        extra_index: u32,
    };

    pub const Index = u32;
};

pub const ValRef = struct {

};

pub const TyRef = struct {
    
};

pub const Hir = struct {
    inst: []Inst,
    extra_data: []u32,
};
