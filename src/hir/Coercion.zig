const std = @import("std");
const Type = @import("type.zig").Type;
const Hir = @import("../Hir.zig");
const Value = @import("../value.zig").Value;
const BlockEditor = @import("BlockEditor.zig");
const Allocator = std.mem.Allocator;

pub const Error = error{
    InvalidCoercion,
    Truncated,
    OutOfMemory,
    NotImplemented,
};

const Coercion = @This();
src_block: *BlockEditor,
b: *BlockEditor,
gpa: Allocator,
coerce_inst: ?Hir.Index,
src: Hir.Index,
src_type: Type,
dest_type_ref: Hir.Index,
dest_type: Type,

pub fn coerce(self: *Coercion) !Hir.Index {
    return switch (self.dest_type.kind()) {
        .uint => self.uint(),
        .sint => self.sint(),
        .float => self.float(),
        .array => self.array(),
        else => {
            std.log.err("coercion from {} to {} unimplemented or not allowed", .{ self.src_type.kind(), self.dest_type.kind() });
            return error.NotImplemented;
        },
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
                .ty = self.dest_type_ref,
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
                .ty = self.dest_type_ref,
                .val = self.src,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(zext);
            return zext;
        },
        // could overflow/underflow, so not allowed
        .sint => {
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
                .ty = self.dest_type_ref,
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
                .ty = self.dest_type_ref,
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
                .ty = self.dest_type_ref,
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
                .ty = self.dest_type_ref,
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
                .ty = self.dest_type_ref,
                .val = self.src,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(fpext);
            return fpext;
        },
        else => return error.InvalidCoercion,
    }
}

fn array(self: *Coercion) Error!Hir.Index {
    const b = self.b;
    const hg = b.hg;

    switch (self.src_type.kind()) {
        // can coerce to a fixed float if the comptime value fits in bounds
        // TODO: we don't check that right now
        .comptime_array => {
            const src_type = self.src_type.extended.cast(Type.ComptimeArray).?;
            const dest_type = self.dest_type.extended.cast(Type.Array).?;

            // first check that the lengths match
            if (src_type.count != dest_type.count) {
                return error.Truncated;
            }

            // now coerce every source element to the destination type
            const data = hg.get(self.src, .constant);
            const src = hg.values.items[data.val].extended.cast(Value.Array).?;

            const dest_array = try hg.gpa.create(Value.Array);
            dest_array.* = .{ .elements = try hg.gpa.alloc(u32, src.elements.len) };
            const array_value: Value = .{ .extended = &dest_array.base };
            const dest_element_type = try b.addType(dest_type.element);
            std.debug.print("coercing array: elements = {any} {}\n", .{ src.elements, data.val });
            for (src.elements, 0..) |element, i| {
                const element_type = try hg.resolveType(element);
                var coercion = Coercion{
                    .src_block = self.src_block,
                    .b = b,
                    .gpa = b.hg.gpa,
                    .coerce_inst = null,
                    .src = element,
                    .src_type = element_type,
                    .dest_type_ref = dest_element_type,
                    .dest_type = dest_type.element,
                };
                dest_array.elements[i] = try coercion.coerce();
            }

            const constant = try b.add(.constant, .{
                .ty = self.dest_type_ref,
                .val = try b.addValue(array_value),
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        .array => unreachable, // TODO
        else => return error.InvalidCoercion,
    }
}

fn maybeReplaceWith(self: *Coercion, new: Hir.Index) !void {
    if (self.coerce_inst) |old| {
        try self.src_block.replaceAllUsesWith(old, new);
    }
}
