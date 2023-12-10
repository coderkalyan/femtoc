const std = @import("std");
const Type = @import("type.zig").Type;
const Hir = @import("../Hir.zig");
const BlockEditor = @import("BlockEditor.zig");

const indexToRef = Hir.Inst.indexToRef;
const refToIndex = Hir.Inst.refToIndex;

pub const Error = error{
    InvalidCoercion,
    Truncated,
};

pub fn coerce(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    // if the source has the dest type, just return the existing ref
    const src_ty = try b.hg.resolveType(src);
    if (src_ty.eql(dest_ty)) return src;

    return switch (dest_ty.kind()) {
        .uint => uint(b, src, dest_ty),
        .sint => sint(b, src, dest_ty),
        .float => float(b, src, dest_ty),
        else => error.NotImplemented,
    };
}

// coerce anything to a fixed-size unsigned int
fn uint(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = try hg.resolveType(src);
    // std.debug.print("{} ({any}) {}\n", .{ src, refToIndex(src), src_ty.basic });

    switch (src_ty.kind()) {
        // can coerce to a fixed uint if the comptime value fits in bounds
        .comptime_uint => {
            const val: u64 = hg.refToInt(src);
            if (val > dest_ty.maxInt()) {
                return error.Truncated;
            }
            return indexToRef(try b.addIntConstant(dest_ty, val, undefined)); // TODO: node
        },
        // comptime sints are always < 0, so can never fit in a uint
        .comptime_sint => return error.Truncated,
        // can coerce only if the destination uint is larger (same size would be caught earlier)
        .uint => {
            if (dest_ty.basic.width < src_ty.basic.width) {
                return error.Truncated;
            }
            return indexToRef(try b.addZext(src, try b.typeToRef(dest_ty), undefined)); // TODO: node
        },
        // could overflow/underflow, so not allowed
        .sint => {
            // try hg.errors.append(hg.gpa, .{
            //     .tag = .coerce_sint_to_uint,
            //     .token =
            // });
            return error.Truncated;
        },
        // lossy
        .float => return error.Truncated, // TODO: more specific error
        else => return error.InvalidCoercion,
    }
}

// coerce anything to a fixed-size signed int
fn sint(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = try hg.resolveType(src);

    switch (src_ty.kind()) {
        // can coerce to a fixed sint if the comptime value fits in bounds
        .comptime_uint => {
            const val: u64 = hg.refToInt(src);
            if (val > dest_ty.maxInt()) {
                return error.Truncated;
            }
            return indexToRef(try b.addIntConstant(dest_ty, val, undefined)); // TODO: node
        },
        // can coerce to a fixed sint if the comptime value fits in bounds
        .comptime_sint => {
            const val: i64 = @bitCast(hg.refToInt(src));
            if (val < dest_ty.minInt() or val > dest_ty.maxInt()) {
                return error.Truncated;
            }
            return indexToRef(try b.addIntConstant(dest_ty, @bitCast(val), undefined)); // TODO: node
        },
        // could overflow/underflow, so not allowed
        .uint => return error.Truncated,
        // can coerce only if the destination sint is larger (same size would be caught earlier)
        .sint => {
            if (dest_ty.basic.width < src_ty.basic.width) {
                return error.Truncated;
            }
            return indexToRef(try b.addSext(src, try b.typeToRef(dest_ty), undefined)); // TODO: node
        },
        // lossy
        .float => return error.Truncated, // TODO: more specific error
        else => return error.InvalidCoercion,
    }
}

// coerce anything to a fixed-size float
fn float(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = try hg.resolveType(src);

    switch (src_ty.kind()) {
        // can coerce to a fixed float if the comptime value fits in bounds
        // TODO: we don't check that right now
        .comptime_float => {
            const val: f64 = hg.refToFloat(src);
            return indexToRef(try b.addFloatConstant(dest_ty, val, undefined)); // TODO: node
        },
        // lossy and ambiguous, so not allowed
        .comptime_uint, .comptime_sint, .uint, .sint => return error.Truncated,
        // can coerce only if the destination float is larger (same size would be caught earlier)
        .float => {
            if (dest_ty.basic.width < src_ty.basic.width) {
                return error.Truncated;
            }
            return indexToRef(try b.addFpext(src, try b.typeToRef(dest_ty), undefined)); // TODO: node
        },
        else => return error.InvalidCoercion,
    }
}
