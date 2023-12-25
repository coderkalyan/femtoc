const std = @import("std");
const Fir = @import("../fir/Fir.zig");
const Air = @import("Air.zig");
const Sema = @import("Sema.zig");
const Type = @import("type.zig").Type;
const Value = @import("../value.zig").Value;
const Allocator = std.mem.Allocator;

pub const Error = error{
    InvalidCoercion,
    Truncated,
    OutOfMemory,
    NotImplemented,
};

const Coercion = @This();
gpa: Allocator,
coerce_inst: ?Fir.Index,
src: Fir.Index,
src_type: InternPool.Index,
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
fn uint(self: *Coercion) !Air.Ref {
    const b = self.b;
    const hg = b.hg;

    switch (self.src_type.kind()) {
        // can coerce to a fixed uint if the comptime value fits in bounds
        .comptime_uint => {
            const src = hg.get(self.src, .constant);
            const val = hg.pool.getValue(src.val).integer;
            if (val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = self.dest_type_ref,
                .val = src.val,
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
            const src = hg.get(self.src, .constant);
            const val = hg.pool.getValue(src.val).integer;
            if (val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = self.dest_type_ref,
                .val = src.val,
                .node = undefined, // TODO: node annotation
            });
            try self.maybeReplaceWith(constant);
            return constant;
        },
        // can coerce to a fixed sint if the comptime value fits in bounds
        .comptime_sint => {
            const src = hg.get(self.src, .constant);
            const val: i64 = @bitCast(hg.pool.getValue(src.val).integer);
            if (val < self.dest_type.minInt() or val > self.dest_type.maxInt()) {
                return error.Truncated;
            }
            const constant = try b.add(.constant, .{
                .ty = self.dest_type_ref,
                .val = src.val,
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
            const src = hg.get(self.src, .constant);
            const constant = try b.add(.constant, .{
                .ty = self.dest_type_ref,
                .val = src.val,
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
            const dest_element_type = try b.addType(dest_type.element);

            // first check that the lengths match
            if (src_type.count != dest_type.count) {
                return error.Truncated;
            }

            // now coerce every source element to the destination type
            const data = hg.get(self.src, .array_init);
            const src_elements = hg.extra.items[data.elements_start..data.elements_end];
            const scratch_top = b.scratch.items.len;
            defer b.scratch.shrinkRetainingCapacity(scratch_top);

            try b.scratch.ensureUnusedCapacity(hg.arena, src_elements.len);
            for (src_elements) |element| {
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
                b.scratch.appendAssumeCapacity(try coercion.coerce());
            }

            const elements = b.scratch.items[scratch_top..];
            // try to upgrade this to a comptime known array
            var comptime_known = true;
            for (elements) |element| {
                if (hg.insts.items(.tag)[element] != .constant) {
                    comptime_known = false;
                    break;
                }
            }

            if (comptime_known) {
                const handle_scratch_top = b.scratch.items.len;
                defer b.scratch.shrinkRetainingCapacity(handle_scratch_top);
                try b.scratch.ensureUnusedCapacity(hg.arena, elements.len);

                for (elements) |element| {
                    const element_data = hg.get(element, .constant);
                    b.scratch.appendAssumeCapacity(element_data.val);
                }
                const comptime_elements = b.scratch.items[handle_scratch_top..];
                const value = try Value.createArray(hg.pool.arena, comptime_elements);
                const new_inst = try b.add(.constant, .{
                    .ty = try b.addType(self.dest_type),
                    .val = try hg.pool.internValue(value),
                    .node = data.node,
                });
                try self.maybeReplaceWith(new_inst);
                return new_inst;
            } else {
                const new_inst = try b.addArrayInit(elements, data.node);
                try self.maybeReplaceWith(new_inst);
                return new_inst;
            }
        },
        .array => {
            // if the element type is comptime, and the destination type isn't,
            // we can coerce each element over. otherwise, there's nothing to do
            // const src_type = self.src_type.extended.cast(Type.Array).?;
            // const dest_type = self.dest_type.extended.cast(Type.Array).?;
            // switch (src_type.element.kind()) {
            //     .comptime_uint, .comptime_sint, .comptime_float,
            //     => {
            //
            //     },
            //     else =>
            // }
            // std.debug.print("coerce array: {} {}\n", .{ src_type, dest_type });
            unreachable;
        },
        else => return error.InvalidCoercion,
    }
}

fn maybeReplaceWith(self: *Coercion, new: Hir.Index) !void {
    if (self.coerce_inst) |old| {
        try self.src_block.replaceAllUsesWith(old, new);
    }
}
