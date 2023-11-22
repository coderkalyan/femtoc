const std = @import("std");
const Type = @import("../typing.zig").Type;
const Hir = @import("../Hir.zig");
const BlockEditor = @import("BlockEditor.zig");

pub const Error = error{
    InvalidCoercion,
    Truncated,
};

pub fn coerce(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    return switch (dest_ty.kind()) {
        .uint, .sint => coerceToInt(b, src, dest_ty),
        .comptime_uint, .comptime_sint => coerceToComptimeInt(b, src, dest_ty),
        .float => coerceToFloat(b, src, dest_ty),
        .comptime_float => coerceToComptimeFloat(b, src, dest_ty),
        else => error.NotImplemented,
    };
}

fn coerceToInt(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = hg.resolveType(b.resolveRef(src));
    if (@as(u64, @bitCast(dest_ty)) == @as(u64, @bitCast(src_ty))) return src;

    const src_sign = src_ty.intSign();
    const dest_sign = dest_ty.intSign();
    switch (src_ty.basic.kind) {
        .comptime_uint => {
            const val: u64 = hg.refToInt(b.resolveRef(src));
            if ((val >= dest_ty.minInt()) and (val <= dest_ty.maxInt())) {
                const index = try b.addIntConstant(dest_ty, val, undefined); // TODO: node
                return Hir.Inst.indexToRef(index);
            } else return error.Truncated;
        },
        .comptime_sint => {
            const val: i64 = @bitCast(hg.refToInt(b.resolveRef(src)));
            if ((val >= dest_ty.minInt()) and (val <= dest_ty.maxInt())) {
                const index = try b.addIntConstant(dest_ty, @bitCast(val), undefined); // TODO: node
                return Hir.Inst.indexToRef(index);
            } else return error.Truncated;
        },
        .uint => {
            if (src_sign != dest_sign) return error.Truncated;
            if (dest_ty.maxInt() < src_ty.maxInt()) return error.Truncated;
            const zext_data = try hg.addExtra(Hir.Inst.Extend{
                .val = src,
                .ty = try b.typeToRef(dest_ty),
            });

            const index = try b.addInst(.{
                .tag = .zext,
                .data = .{ .pl_node = .{ .pl = zext_data, .node = undefined } }, // TODO: node
            });
            return Hir.Inst.indexToRef(index);
        },
        .sint => {
            if (src_sign != dest_sign) return error.Truncated;
            if (dest_ty.maxInt() < src_ty.maxInt()) return error.Truncated;
            if (dest_ty.minInt() > src_ty.minInt()) return error.Truncated;
            const sext_data = try hg.addExtra(Hir.Inst.Extend{
                .val = src,
                .ty = try b.typeToRef(dest_ty),
            });

            const index = try b.addInst(.{
                .tag = .sext,
                .data = .{ .pl_node = .{ .pl = sext_data, .node = undefined } }, // TODO: node
            });
            return Hir.Inst.indexToRef(index);
        },
        else => return error.InvalidCoercion,
    }
}

fn coerceToComptimeInt(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = hg.resolveType(b.resolveRef(src));
    switch (dest_ty.basic.kind) {
        .comptime_uint => switch (src_ty.kind()) {
            .comptime_uint => return src,
            .comptime_sint => {
                const val: i64 = @bitCast(hg.refToInt(b.resolveRef(src)));
                if (val < 0) return error.Truncated;
                const index = try b.addIntConstant(dest_ty, @bitCast(val), undefined); // TODO: node
                return Hir.Inst.indexToRef(index);
            },
            else => unreachable,
        },
        .comptime_sint => switch (src_ty.kind()) {
            .comptime_uint => {
                const val: u64 = hg.refToInt(b.resolveRef(src));
                if (val > std.math.maxInt(i64)) return error.Truncated;
                const index = try b.addIntConstant(dest_ty, val, undefined); // TODO: node
                return Hir.Inst.indexToRef(index);
            },
            .comptime_sint => return src,
            else => unreachable,
        },
        else => unreachable,
    }
}

// given two types in a binary operation, returns the coercion target type
// that both references should be coerced to. generally this will be one of
// the two existing types - whichever is more "well known" or "safe"
// for instance, comptime numbers cast to fixed width numbers, and
// ints cast upwards in bitwidth
// if this function returns, it does NOT necessarily mean the coercion will
// succeed (coerce() will determine that)
pub fn binaryCoerceTo(lty: Type, rty: Type) !Type {
    switch (lty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => return rty,
        .uint, .sint => switch (rty.kind()) {
            .comptime_uint, .comptime_sint, .comptime_float => return lty,
            .uint, .sint => {
                const lsize = lty.size();
                const rsize = rty.size();
                return if (rsize > lsize) rty else lty;
            },
            else => return error.NotImplemented,
        },
        .float => {
            if (lty.basic.width == 64) return lty;
            return if (rty.kind() == .comptime_float) lty else rty;
        },
        else => return error.NotImplemented,
    }
}

pub fn coerceToFloat(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    const hg = b.hg;
    const src_ty = hg.resolveType(b.resolveRef(src));
    if (@as(u64, @bitCast(src_ty)) == @as(u64, @bitCast(dest_ty))) return src;
    if (dest_ty.basic.width == 64) {
        if (src_ty.kind() == .comptime_float) {
            const val = hg.refToFloat(b.resolveRef(src));
            const index = try b.addFloatConstant(dest_ty, val, undefined); // TODO: node
            return Hir.Inst.indexToRef(index);
        } else if (src_ty.basic.width == 32) {
            const fpext_data = try hg.addExtra(Hir.Inst.Extend{
                .val = src,
                .ty = try b.typeToRef(Type.initFloat(64)),
            });

            const index = try b.addInst(.{
                .tag = .fpext,
                .data = .{ .pl_node = .{ .pl = fpext_data, .node = undefined } }, // TODO: node
            });
            return Hir.Inst.indexToRef(index);
        } else return error.InvalidCoercion;
    } else {
        if (src_ty.kind() == .comptime_float) {
            const val = hg.refToFloat(b.resolveRef(src));
            const index = try b.addFloatConstant(dest_ty, val, undefined); // TODO: node
            return Hir.Inst.indexToRef(index);
        } else return error.InvalidCoercion;
    }
}

pub fn coerceToComptimeFloat(b: *BlockEditor, src: Hir.Ref, dest_ty: Type) !Hir.Ref {
    _ = dest_ty;
    const src_ty = b.hg.resolveType(b.resolveRef(src));
    if (src_ty.kind() == .comptime_float) return src;
    return error.InvalidCoercion;
}
