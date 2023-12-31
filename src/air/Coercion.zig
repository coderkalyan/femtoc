const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Fir = @import("../fir/Fir.zig");
const Air = @import("Air.zig");
const Sema = @import("Sema.zig");
const Type = @import("type.zig").Type;
const Allocator = std.mem.Allocator;

pub const Error = error{
    InvalidCoercion,
    Truncated,
    OutOfMemory,
    NotImplemented,
};

const Coercion = @This();
b: *Sema.Block,
src: Air.Index,
dest_type: InternPool.Index,

pub fn coerce(self: *Coercion) !Air.Index {
    const src_type = self.b.sema.tempAir().typeOf(self.src);
    if (src_type == self.dest_type) return self.src;

    const dest_type = self.b.pool.indexToKey(self.dest_type).ty;
    return switch (dest_type) {
        .int => |int| switch (int.sign) {
            .unsigned => self.uint(),
            .signed => self.sint(),
        },
        // .float => self.float(),
        .array => self.array(),
        else => {
            const src_key = self.b.pool.indexToKey(src_type).ty;
            std.log.err("coercion from {} to {} unimplemented or not allowed", .{ src_key, dest_type });
            return error.NotImplemented;
        },
    };
}

// coerce anything to a fixed-size unsigned int
fn uint(self: *Coercion) !Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToKey(self.dest_type).ty;

    switch (b.pool.indexToKey(src_type).ty) {
        // can coerce to a fixed uint if the comptime value fits in bounds
        .comptime_int => |ty| {
            // comptime sints are always < 0, so can never fit in a uint
            if (ty.sign == .signed) return error.Truncated;

            const src = b.sema.insts.get(@intFromEnum(self.src)).constant;
            const tv = b.pool.indexToKey(src).tv;
            const val = tv.val.integer;
            if (val > dest_type.maxInt()) {
                return error.Truncated;
            }
            return b.addConstant(.{ .tv = .{
                .ty = self.dest_type,
                .val = .{ .integer = val },
            } });
        },
        // can coerce only if the destination uint is larger (same size would be caught earlier)
        .int => |ty| {
            // could overflow/underflow, so not allowed
            if (ty.sign == .signed) return error.Truncated;
            if (dest_type.int.width < ty.width) return error.Truncated;

            return b.add(.{ .zext = .{
                .ty = self.dest_type,
                .operand = self.src,
            } });
        },
        // lossy
        .float => return error.Truncated, // TODO: more specific error
        else => return error.InvalidCoercion,
    }
}

// coerce anything to a fixed-size signed int
fn sint(self: *Coercion) !Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToKey(self.dest_type).ty;

    switch (b.pool.indexToKey(src_type).ty) {
        .comptime_int => |ty| switch (ty.sign) {
            // can coerce to a fixed uint if the comptime value fits in bounds
            .unsigned => {
                const src = b.sema.insts.get(@intFromEnum(self.src)).constant;
                const tv = b.pool.indexToKey(src).tv;
                const val = tv.val.integer;
                if (val > dest_type.maxInt()) {
                    return error.Truncated;
                }
                return b.addConstant(.{ .tv = .{
                    .ty = self.dest_type,
                    .val = .{ .integer = val },
                } });
            },
            // can coerce to a fixed sint if the comptime value fits in bounds
            .signed => {
                const src = b.sema.insts.get(@intFromEnum(self.src)).constant;
                const tv = b.pool.indexToKey(src).tv;
                const val: i64 = @bitCast(tv.val.integer);
                if (val < dest_type.minInt() or val > dest_type.maxInt()) {
                    return error.Truncated;
                }
                return b.addConstant(.{ .tv = .{
                    .ty = self.dest_type,
                    .val = .{ .integer = @bitCast(val) },
                } });
            },
        },
        // can coerce only if the destination sint is larger (same size would be caught earlier)
        .int => |ty| {
            // could overflow/underflow, so not allowed
            if (ty.sign == .unsigned) return error.Truncated;
            if (dest_type.int.width < ty.width) return error.Truncated;

            return b.add(.{ .sext = .{
                .ty = self.dest_type,
                .operand = self.src,
            } });
        },
        // lossy
        .float => return error.Truncated, // TODO: more specific error
        else => return error.InvalidCoercion,
    }
}

// coerce anything to a fixed-size float
// fn float(self: *Coercion) !Hir.Index {
//     const b = self.b;
//     const hg = b.hg;
//
//     switch (self.src_type.kind()) {
//         // can coerce to a fixed float if the comptime value fits in bounds
//         // TODO: we don't check that right now
//         .comptime_float => {
//             const src = hg.get(self.src, .constant);
//             const constant = try b.add(.constant, .{
//                 .ty = self.dest_type_ref,
//                 .val = src.val,
//                 .node = undefined, // TODO: node annotation
//             });
//             try self.maybeReplaceWith(constant);
//             return constant;
//         },
//         // lossy and ambiguous, so not allowed
//         .comptime_uint, .comptime_sint, .uint, .sint => return error.Truncated,
//         // can coerce only if the destination float is larger (same size would be caught earlier)
//         .float => {
//             if (self.dest_type.basic.width < self.src_type.basic.width) {
//                 return error.Truncated;
//             }
//             const fpext = try b.add(.fpext, .{
//                 .ty = self.dest_type_ref,
//                 .val = self.src,
//                 .node = undefined, // TODO: node annotation
//             });
//             try self.maybeReplaceWith(fpext);
//             return fpext;
//         },
//         else => return error.InvalidCoercion,
//     }
// }
//
fn array(self: *Coercion) Error!Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToKey(self.dest_type).ty;

    switch (b.pool.indexToKey(src_type).ty) {
        .comptime_array => |ty| {
            // first check that the lengths match
            if (ty.count != dest_type.array.count) {
                return error.Truncated;
            }

            // now coerce every source element to the destination type
            const src = b.sema.insts.get(@intFromEnum(self.src)).constant;
            const tv = b.pool.indexToKey(src).tv;
            const comptime_array = tv.val.array;
            const src_elements = b.pool.extra.items[comptime_array.start..comptime_array.end];
            const scratch_top = b.sema.scratch.items.len;
            defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
            try b.sema.scratch.ensureUnusedCapacity(b.arena, src_elements.len);
            for (src_elements) |element| {
                // TODO: can we avoid creating this temporary constant? might
                // require some coercion refactoring
                // for now, we add this without linking it to the block so
                // it doesn't show up
                const src_element = try b.sema.addConstant(b.pool.indexToKey(@enumFromInt(element)));
                var coercion = Coercion{
                    .b = b,
                    .src = src_element,
                    .dest_type = dest_type.array.element,
                };
                const constant = try coercion.coerce();
                // const constant: Air.Index = @enumFromInt(element);
                const element_tv = b.sema.insts.get(@intFromEnum(constant)).constant;
                b.sema.scratch.appendAssumeCapacity(@intFromEnum(element_tv));
            }

            const tvs = b.sema.scratch.items[scratch_top..];
            const elements_start: u32 = @intCast(b.pool.extra.items.len);
            try b.pool.extra.appendSlice(b.pool.gpa, tvs);
            const elements_end: u32 = @intCast(b.pool.extra.items.len);
            const air_inst = try b.addConstant(.{ .tv = .{
                .ty = try b.pool.getOrPut(.{ .ty = .{ .array = .{
                    .element = dest_type.array.element,
                    .count = @intCast(src_elements.len),
                } } }),
                .val = .{ .array = .{ .start = elements_start, .end = elements_end } },
            } });
            return air_inst;
            // try b.mapInst(self.src, air_inst);
            // // try to upgrade this to a comptime known array
            // var comptime_known = true;
            // for (elements) |element| {
            //     if (hg.insts.items(.tag)[element] != .constant) {
            //         comptime_known = false;
            //         break;
            //     }
            // }
            //
            // if (comptime_known) {
            //     const handle_scratch_top = b.sema.scratch.items.len;
            //     defer b.sema.scratch.shrinkRetainingCapacity(handle_scratch_top);
            //     try b.sema.scratch.ensureUnusedCapacity(hg.arena, elements.len);
            //
            //     for (elements) |element| {
            //         const element_data = hg.get(element, .constant);
            //         b.sema.scratch.appendAssumeCapacity(element_data.val);
            //     }
            //     const comptime_elements = b.sema.scratch.items[handle_scratch_top..];
            //     const value = try Value.createArray(hg.pool.arena, comptime_elements);
            //     const new_inst = try b.add(.constant, .{
            //         .ty = try b.addType(self.dest_type),
            //         .val = try hg.pool.internValue(value),
            //         .node = data.node,
            //     });
            //     try self.maybeReplaceWith(new_inst);
            //     return new_inst;
            // } else {
            //     const new_inst = try b.addArrayInit(elements, data.node);
            //     try self.maybeReplaceWith(new_inst);
            //     return new_inst;
            // }
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
//
// fn maybeReplaceWith(self: *Coercion, new: Hir.Index) !void {
//     if (self.coerce_inst) |old| {
//         try self.src_block.replaceAllUsesWith(old, new);
//     }
// }
