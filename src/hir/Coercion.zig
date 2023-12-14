const std = @import("std");
const Type = @import("type.zig").Type;
const Hir = @import("../Hir.zig");
const BlockEditor = @import("BlockEditor.zig");
const Allocator = std.mem.Allocator;

pub const Error = error{
    InvalidCoercion,
    Truncated,
};

const Coercion = @This();
// pub const Coercion = struct {
src_block: *BlockEditor,
b: *BlockEditor,
gpa: Allocator,
coerce_inst: ?Hir.Index,
src: Hir.Index,
src_type: Type,
dest_type: Type,

pub fn coerce(self: *Coercion) !Hir.Index {
    return switch (self.dest_type.kind()) {
        .uint => self.uint(),
        .sint => self.sint(),
        .float => self.float(),
        else => error.NotImplemented,
    };
}

// coerce anything to a fixed-size unsigned int
fn uint(self: *Coercion) !Hir.Index {
    const b = self.b;
    const hg = b.hg;

    switch (self.src_type.kind()) {
        // can coerce to a fixed uint if the comptime value fits in bounds
        .comptime_uint => {
            const val: u64 = hg.instToInt(self.src);
            if (val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = try b.addType(self.dest_type),
                .val = try b.addIntValue(self.dest_type, val),
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        // comptime sints are always < 0, so can never fit in a uint
        .comptime_sint => return error.Truncated,
        // can coerce only if the destination uint is larger (same size would be caught earlier)
        .uint => {
            if (self.dest_type.basic.width < self.src_type.basic.width) {
                return error.Truncated;
            }
            const zext = try b.add(.zext, .{
                .ty = try b.addType(self.dest_type),
                .val = self.src,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(zext);
            return zext;
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
fn sint(self: *Coercion) !Hir.Index {
    const b = self.b;
    const hg = b.hg;

    switch (self.src_type.kind()) {
        // can coerce to a fixed sint if the comptime value fits in bounds
        .comptime_uint => {
            const val: u64 = hg.instToInt(self.src);
            if (val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = try b.addType(self.dest_type),
                .val = try b.addIntValue(self.dest_type, val),
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        // can coerce to a fixed sint if the comptime value fits in bounds
        .comptime_sint => {
            const val: i64 = @bitCast(hg.instToInt(self.src));
            if (val < self.dest_type.minInt() or val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = try b.addType(self.dest_type),
                .val = try b.addIntValue(self.dest_type, @bitCast(val)),
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        // could overflow/underflow, so not allowed
        .uint => return error.Truncated,
        // can coerce only if the destination sint is larger (same size would be caught earlier)
        .sint => {
            if (self.dest_type.basic.width < self.src_type.basic.width) {
                return error.Truncated;
            }
            const sext = try b.add(.sext, .{
                .ty = try b.addType(self.dest_type),
                .val = self.src,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(sext);
            return sext;
        },
        // lossy
        .float => return error.Truncated, // TODO: more specific error
        else => return error.InvalidCoercion,
    }
}

// coerce anything to a fixed-size float
fn float(self: *Coercion) !Hir.Index {
    const b = self.b;
    const hg = b.hg;

    switch (self.src_type.kind()) {
        // can coerce to a fixed float if the comptime value fits in bounds
        // TODO: we don't check that right now
        .comptime_float => {
            const val: f64 = hg.instToFloat(self.src);
            const constant = try b.add(.constant, .{
                .ty = try b.addType(self.dest_type),
                .val = try b.addFloatValue(val),
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        // lossy and ambiguous, so not allowed
        .comptime_uint, .comptime_sint, .uint, .sint => return error.Truncated,
        // can coerce only if the destination float is larger (same size would be caught earlier)
        .float => {
            if (self.dest_type.basic.width < self.src_type.basic.width) {
                return error.Truncated;
            }
            const fpext = try b.add(.fpext, .{
                .ty = try b.addType(self.dest_type),
                .val = self.src,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(fpext);
            return fpext;
        },
        else => return error.InvalidCoercion,
    }
}

fn maybeReplaceWith(self: *Coercion, new: Hir.Index) !void {
    if (self.coerce_inst) |old| {
        try self.src_block.replaceAllUsesWith(old, new);
    }
}
// };

// pub fn coerce(src_block: *BlockEditor, b: *BlockEditor, inst: Hir.Index, src: Hir.Index, dest_type: Type) !void {
//     // if the source has the dest type, this is a nop
//     const src_type = try b.hg.resolveType(src);
//     if (src_type.eql(dest_type)) return;
//
//     // otherwise, dispatch a coercion function depending on the destination type
//     var info = Coercion{
//         .src_block = src_block,
//         .b = b,
//         .gpa = b.hg.gpa,
//         .coerce_inst = inst,
//         .src = src,
//         .src_type = src_type,
//         .dest_type = dest_type,
//     };
//
//     return switch (dest_type.kind()) {
//         .uint => info.uint(),
//         .sint => info.sint(),
//         .float => info.float(),
//         else => error.NotImplemented,
//     };
// }
