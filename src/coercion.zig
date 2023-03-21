const std = @import("std");
const Analyzer = @import("Analyzer.zig");
const Type = @import("typing.zig").Type;
const Mir = @import("Mir.zig");
const Block = Analyzer.Block;

pub const Error = error {
    InvalidCoercion,
    Truncated,
};

pub fn coerce(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    return switch (dest_ty.kind()) {
        .uint, .sint => coerceToInt(analyzer, b, src, dest_ty),
        .comptime_uint, .comptime_sint => coerceToComptimeInt(analyzer, b, src, dest_ty),
        .float => coerceToFloat(analyzer, b, src, dest_ty),
        .comptime_float => coerceToComptimeFloat(analyzer, b, src, dest_ty),
        else => error.NotImplemented,
    };
}

fn coerceToInt(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    const src_ty = try analyzer.resolveTy(b, src);
    if (@bitCast(u64, dest_ty) == @bitCast(u64, src_ty)) return src;

    const src_sign = src_ty.intSign();
    const dest_sign = dest_ty.intSign();
    switch (src_ty.basic.kind) {
        .comptime_uint => {
            const val: u64 = analyzer.refToInt(src);
            if ((val >= dest_ty.minInt()) and (val <= dest_ty.maxInt())) {
                const index = try b.addIntConstant(dest_ty, val);
                return Mir.indexToRef(index);
            } else return error.Truncated;
        },
        .comptime_sint => {
            const val: i64 = @bitCast(i64, analyzer.refToInt(src));
            if ((val >= dest_ty.minInt()) and (val <= dest_ty.maxInt())) {
                const index = try b.addIntConstant(dest_ty, @bitCast(u64, val));
                return Mir.indexToRef(index);
            } else return error.Truncated;
        },
        .uint => {
            if (src_sign != dest_sign) return error.Truncated;
            if (dest_ty.maxInt() < src_ty.maxInt()) return error.Truncated;
            const index = try b.addInst(.{
                .tag = .zext,
                .data = .{ .ty_op = .{ .ty = try b.typeToRef(dest_ty), .op = src } },
            });
            return Mir.indexToRef(index);
        },
        .sint => {
            if (src_sign != dest_sign) return error.Truncated;
            if (dest_ty.maxInt() < src_ty.maxInt()) return error.Truncated;
            if (dest_ty.minInt() > src_ty.minInt()) return error.Truncated;
            const index = try b.addInst(.{
                .tag = .sext,
                .data = .{ .ty_op = .{ .ty = try b.typeToRef(dest_ty), .op = src } },
            });
            return Mir.indexToRef(index);
        },
        else => return error.InvalidCoercion,
    }
}

fn coerceToComptimeInt(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    const src_ty = try analyzer.resolveTy(b, src);
    switch (dest_ty.basic.kind) {
        .comptime_uint => switch (src_ty.kind()) {
            .comptime_uint => return src,
            .comptime_sint => {
                const val: i64 = @bitCast(i64, analyzer.refToInt(src));
                if (val < 0) return error.Truncated;
                const index = try b.addIntConstant(dest_ty, @bitCast(u64, val));
                return Mir.indexToRef(index);
            },
            else => unreachable,
        },
        .comptime_sint => switch (src_ty.kind()) {
            .comptime_uint => {
                const val: u64 = analyzer.refToInt(src);
                if (val > std.math.maxInt(i64)) return error.Truncated;
                const index = try b.addIntConstant(dest_ty, val);
                return Mir.indexToRef(index);
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

pub fn coerceToFloat(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    const src_ty = try analyzer.resolveTy(b, src);
    if (@bitCast(u64, src_ty) == @bitCast(u64, dest_ty)) return src;
    if (dest_ty.basic.width == 64) {
        if (src_ty.kind() == .comptime_float) {
            const val = analyzer.refToFloat(src);
            const index = try b.addFloatConstant(dest_ty, val);
            return Mir.indexToRef(index);
        } else if (src_ty.basic.width == 32) {
            const index = try b.addInst(.{
                .tag = .fpext,
                .data = .{ .ty_op = .{ .ty = try b.typeToRef(Type.initFloat(64)), .op = src } },
            });
            return Mir.indexToRef(index);
        } else return error.InvalidCoercion;
    } else {
        if (src_ty.kind() == .comptime_float) {
            const val = analyzer.refToFloat(src);
            const index = try b.addFloatConstant(dest_ty, val);
            return Mir.indexToRef(index);
        } else return error.InvalidCoercion;
    }
}

pub fn coerceToComptimeFloat(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    _ = dest_ty;
    const src_ty = try analyzer.resolveTy(b, src);
    if (src_ty.kind() == .comptime_float) return src;
    return error.InvalidCoercion;
}
