const std = @import("std");
const Hir = @import("../Hir.zig");
const Type = @import("type.zig").Type;
const Value = @import("../value.zig").Value;
const TypedValue = @import("TypedValue.zig");
const Allocator = std.mem.Allocator;

const DeclInfo = @This();

inline_block: Hir.Index,
name: u32,
initializer: ?TypedValue,
ty: ?Type,
mutable: bool,
linkage: Linkage,
// kind: Kind,
//
// pub const Kind = enum {
//
// };

const Ref = union {
    ty: Type,
    decl_handle: *DeclInfo,
    constant: Hir.Index,
};

pub const Linkage = enum {
    internal,
    external,
};

pub fn generate(hir: *const Hir, gpa: Allocator, inline_block: Hir.Index, name: u32) !*DeclInfo {
    var map = std.AutoHashMap(Hir.Index, Ref).init(gpa);
    defer map.deinit();

    const block = hir.get(inline_block, .block_inline);
    const insts = hir.block_slices[block.head];
    for (insts) |inst| {
        switch (hir.insts.items(.tag)[inst]) {
            .ty => {
                const data = hir.get(inst, .ty);
                try map.put(inst, .{ .ty = data.ty });
            },
            .constant => try map.put(inst, .{ .constant = inst }),
            .global_handle => {
                const decl = try gpa.create(DeclInfo);
                decl.* = .{
                    .inline_block = inline_block,
                    .name = name,
                    .initializer = null,
                    .ty = null,
                    .mutable = false,
                    .linkage = .internal,
                };
                try map.put(inst, .{ .decl_handle = decl });
            },
            .global_set_mutable => {
                const data = hir.get(inst, .global_set_mutable);
                const handle = map.get(data.operand).?.decl_handle;
                handle.mutable = true;
            },
            .global_set_type => {
                const data = hir.get(inst, .global_set_type);
                const handle = map.get(data.handle).?.decl_handle;
                const ty = map.get(data.operand).?.ty;
                handle.ty = ty;
            },
            .global_set_init => {
                const data = hir.get(inst, .global_set_init);
                const handle = map.get(data.handle).?.decl_handle;
                const constant_inst = map.get(data.operand).?.constant;
                const constant = hir.get(constant_inst, .constant);

                const ty = try hir.resolveType(gpa, constant.ty);
                const val = hir.pool.getValue(constant.val);
                const tv = TypedValue{ .ty = ty, .val = val };
                handle.initializer = tv;
            },
            .global_set_linkage_external => {
                const data = hir.get(inst, .global_set_linkage_external);
                const handle = map.get(data.operand).?.decl_handle;
                handle.linkage = .external;
            },
            .yield_inline => {
                const data = hir.get(inst, .yield_inline);
                const handle = map.get(data.operand).?.decl_handle;
                return handle;
            },
            else => |tag| {
                std.log.err("encountered invalid decl instruction {}\n", .{tag});
                unreachable;
            },
        }
    }

    unreachable;
}
