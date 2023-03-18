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
    return switch (dest_ty.tag) {
        .u1, .u8, .u16, .u32, .u64,
        .i8, .i16, .i32, .i64 => coerceToInt(analyzer, b, src, dest_ty),
        .comptime_uint, .comptime_sint => coerceToComptimeInt(analyzer, b, src, dest_ty),
        else => error.NotImplemented,
    };
}

fn coerceToInt(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    const src_ty = try Analyzer.resolveTy(analyzer, b, src);
    if (!src_ty.isIntType()) return error.InvalidCoercion;

    const src_sign = src_ty.intSign();
    const dest_sign = dest_ty.intSign();
    switch (src_ty.tag) {
        .comptime_uint => {
            const val: u64 = analyzer.refToInt(src);
            if ((val >= dest_ty.intMinValue()) and (val <= dest_ty.intMaxValue())) {
                const index = try b.addConstant(dest_ty, .{ .int = val });
                return Mir.indexToRef(index);
            } else return error.Truncated;
        },
        .comptime_sint => {
            const val: i64 = @bitCast(i64, analyzer.refToInt(src));
            if ((val >= dest_ty.intMinValue()) and (val <= dest_ty.intMaxValue())) {
                const index = try b.addConstant(dest_ty, .{ .int = @bitCast(u64, val) });
                return Mir.indexToRef(index);
            } else return error.Truncated;
        },
        .u1, .u8, .u16, .u32, .u64 => {
            if (src_sign != dest_sign) return error.Truncated;
            if (dest_ty.intMaxValue() < src_ty.intMaxValue()) return error.Truncated;
            if (dest_ty.intMaxValue() == src_ty.intMaxValue()) return src;
            const index = try b.addInst(.{
                .tag = .zext,
                .data = .{ .ty_op = .{ .ty = dest_ty, .op = src } },
            });
            return Mir.indexToRef(index);
        },
        .i8, .i16, .i32, .i64 => {
            if (src_sign != dest_sign) return error.Truncated;
            const smax = src_ty.intMaxValue();
            const dmax = dest_ty.intMaxValue();
            const smin = src_ty.intMinValue();
            const dmin = dest_ty.intMinValue();
            if (dmax < smax) return error.Truncated;
            if (dmin > smin) return error.Truncated;
            if ((dmax == smax) and (dmin == smin)) return src;
            const index = try b.addInst(.{
                .tag = .sext,
                .data = .{ .ty_op = .{ .ty = dest_ty, .op = src } },
            });
            return Mir.indexToRef(index);
        },
        else => return error.InvalidCoercion,
    }
}

fn coerceToComptimeInt(analyzer: *Analyzer, b: *Block, src: Mir.Ref, dest_ty: Type) !Mir.Ref {
    const src_ty = try Analyzer.resolveTy(analyzer, b, src);
    switch (dest_ty.tag) {
        .comptime_uint => switch (src_ty.tag) {
            .comptime_uint => return src,
            .comptime_sint => {
                const val: i64 = @bitCast(i64, analyzer.refToInt(src));
                if (val < 0) return error.Truncated;
                const index = try b.addConstant(dest_ty, .{ .int = @bitCast(u64, val) });
                return Mir.indexToRef(index);
            },
            else => unreachable,
        },
        .comptime_sint => switch (src_ty.tag) {
            .comptime_uint => {
                const val: u64 = analyzer.refToInt(src);
                if (val > std.math.maxInt(i64)) return error.Truncated;
                const index = try b.addConstant(dest_ty, .{ .int = val });
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
    switch (lty.tag) {
        .comptime_uint, .comptime_sint, .comptime_float => return rty,
        .u1, .u8, .u16, .u32, .u64,
        .i8, .i16, .i32, .i64 => {
            switch (rty.tag) {
                .comptime_uint, .comptime_sint, .comptime_float => return lty,
                .u1, .u8, .u16, .u32, .u64,
                .i8, .i16, .i32, .i64 => {
                    const lsize = try lty.size();
                    const rsize = try rty.size();
                    return if (rsize > lsize) rty else lty;
                },
                else => return error.NotImplemented,
            }
        },
        else => return error.NotImplemented,
    }
}
