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
        .float => self.float(),
        .array => self.array(),
        .slice => self.slice(),
        .pointer => self.pointer(),
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

            const src = b.sema.instData(self.src).constant;
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
        else => |ty| {
            std.log.err("coercion from {} to {} unimplemented or not allowed", .{ ty, dest_type });
            return error.InvalidCoercion;
        },
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
                const src = b.sema.instData(self.src).constant;
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
                const src = b.sema.instData(self.src).constant;
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
fn float(self: *Coercion) !Air.Index {
    const b = self.b;
    const src_type = b.typeOf(self.src);
    const dest_type = b.pool.indexToType(self.dest_type);

    switch (b.pool.indexToType(src_type)) {
        // can coerce to a fixed float if the comptime value fits in bounds
        // TODO: we don't check that right now
        .comptime_float => {
            const src = b.sema.instData(self.src).constant;
            const tv = b.pool.indexToKey(src).tv;
            const val = tv.val.float;
            return b.addConstant(.{ .tv = .{
                .ty = self.dest_type,
                .val = .{ .float = val },
            } });
        },
        // lossy and ambiguous, so not allowed
        .comptime_int, .int => return error.Truncated,
        // can coerce only if the destination float is larger (same size would be caught earlier)
        .float => |ty| {
            if (dest_type.float.width < ty.width) return error.Truncated;

            return b.add(.{ .fpext = .{
                .ty = self.dest_type,
                .operand = self.src,
            } });
        },
        else => return error.InvalidCoercion,
    }
}

fn array(self: *Coercion) Error!Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToKey(self.dest_type).ty;
    // const _src_type = b.pool.indexToType(src_type);

    switch (b.pool.indexToKey(src_type).ty) {
        // TODO: this might need to be thrown out
        .array => |ty| {
            // first check that the lengths match
            if (ty.count != dest_type.array.count) {
                return error.Truncated;
            }

            // now coerce every source element to the destination type
            const src = b.sema.instData(self.src).constant;
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
                const element_tv = b.sema.instData(constant).constant;
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
        },
        // .array => |ty| {
        //     _ = ty;
        //     unreachable;
        // first check that the lengths match
        // if (ty.count != dest_type.array.count) {
        //     return error.Truncated;
        // }
        //
        // // now coerce every source element to the destination type
        // // for (0..ty.count) |i| {
        // //     const src = try b.addIndexVal
        // // }
        // const src = b.sema.insts.get(@intFromEnum(self.src)).constant;
        // const tv = b.pool.indexToKey(src).tv;
        // const comptime_array = tv.val.array;
        // const src_elements = b.pool.extra.items[comptime_array.start..comptime_array.end];
        // const scratch_top = b.sema.scratch.items.len;
        // defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
        // try b.sema.scratch.ensureUnusedCapacity(b.arena, src_elements.len);
        // for (src_elements) |element| {
        //     // TODO: can we avoid creating this temporary constant? might
        //     // require some coercion refactoring
        //     // for now, we add this without linking it to the block so
        //     // it doesn't show up
        //     const src_element = try b.sema.addConstant(b.pool.indexToKey(@enumFromInt(element)));
        //     var coercion = Coercion{
        //         .b = b,
        //         .src = src_element,
        //         .dest_type = dest_type.array.element,
        //     };
        //     const constant = try coercion.coerce();
        //     // const constant: Air.Index = @enumFromInt(element);
        //     const element_tv = b.sema.insts.get(@intFromEnum(constant)).constant;
        //     b.sema.scratch.appendAssumeCapacity(@intFromEnum(element_tv));
        // }

        // const tvs = b.sema.scratch.items[scratch_top..];
        // const elements_start: u32 = @intCast(b.pool.extra.items.len);
        // try b.pool.extra.appendSlice(b.pool.gpa, tvs);
        // const elements_end: u32 = @intCast(b.pool.extra.items.len);
        // const air_inst = try b.addConstant(.{ .tv = .{
        //     .ty = try b.pool.getOrPut(.{ .ty = .{ .array = .{
        //         .element = dest_type.array.element,
        //         .count = @intCast(src_elements.len),
        //     } } }),
        //     .val = .{ .array = .{ .start = elements_start, .end = elements_end } },
        // } });
        // return air_inst;
        // },
        // .array => {
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
        //     unreachable;
        // },
        else => return error.InvalidCoercion,
    }
}

fn slice(self: *Coercion) Error!Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToKey(self.dest_type).ty;

    switch (b.pool.indexToKey(src_type).ty) {
        .pointer => |data| {
            const pointee = b.pool.indexToKey(data.pointee).ty;
            switch (pointee) {
                .array => |arr| {
                    if (dest_type.slice.element != arr.element) return error.InvalidCoercion;

                    const ptr = self.src;
                    const len = try b.addConstant(.{ .tv = .{
                        .ty = try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 64 } } }),
                        .val = .{ .integer = arr.count },
                    } });
                    const slice_type = try b.pool.getOrPut(.{ .ty = .{
                        .slice = .{ .element = arr.element },
                    } });
                    const pl = try b.sema.addExtra(Air.Inst.SliceInit{
                        .ptr = ptr,
                        .len = len,
                        .ty = slice_type,
                    });
                    return b.add(.{ .slice_init = .{ .pl = pl } });
                },
                else => return error.InvalidCoercion,
            }
        },
        else => |tag| {
            std.debug.print("{} {}\n", .{ dest_type.slice.element, b.pool.indexToKey(src_type).ty.slice.element });
            std.debug.print("{} {}\n", .{ tag, self.dest_type });
            std.debug.print("{} {}\n", .{ src_type, self.dest_type });
            return error.InvalidCoercion;
        },
    }
}

fn pointer(self: *Coercion) Error!Air.Index {
    const b = self.b;
    const src_type = b.sema.tempAir().typeOf(self.src);
    const dest_type = b.pool.indexToType(self.dest_type).pointer;

    switch (b.pool.indexToType(src_type)) {
        .pointer => |src_pointer| {
            if (src_pointer.pointee != dest_type.pointee) return error.InvalidCoercion;
            // cannot erase const
            if (!src_pointer.mutable and dest_type.mutable) return error.InvalidCoercion;
            return self.src;
        },
        else => {
            return error.InvalidCoercion;
        },
    }
}
