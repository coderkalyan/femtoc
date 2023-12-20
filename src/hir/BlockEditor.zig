const std = @import("std");
const Allocator = std.mem.Allocator;
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Type = @import("type.zig").Type;
const Value = @import("../value.zig").Value;
const InternPool = @import("../InternPool.zig");

const Inst = Hir.Inst;
const ValueHandle = InternPool.ValueHandle;
const BlockEditor = @This();

hg: *HirGen,
insts: std.ArrayListUnmanaged(Hir.Index),
scratch: std.ArrayListUnmanaged(u32),

pub fn init(hg: *HirGen) !BlockEditor {
    return .{
        .hg = hg,
        .insts = .{},
        .scratch = .{},
    };
}

pub fn clone(hg: *HirGen, insts: []Hir.Index) !BlockEditor {
    var arraylist = std.ArrayListUnmanaged(Hir.Index){};
    try arraylist.ensureUnusedCapacity(hg.gpa, insts.len);
    arraylist.appendSliceAssumeCapacity(insts);

    return .{
        .hg = hg,
        .insts = arraylist,
        .scratch = .{},
    };
}

pub inline fn linkInst(b: *BlockEditor, inst: Hir.Index) !void {
    try b.insts.append(b.hg.gpa, inst);
}

// replaces an existing instruction (at a specified array index) with a new one
// be very careful using this, if you're not sure just use addInst or linkInst
pub inline fn replaceInst(b: *BlockEditor, index: usize, inst: Hir.Index) void {
    b.insts.items[index] = inst;
}

pub inline fn insertInst(b: *BlockEditor, index: usize, inst: Hir.Index) !void {
    try b.insts.insert(b.hg.gpa, index, inst);
}

pub inline fn removeInst(b: *BlockEditor, index: usize) !void {
    _ = b.insts.orderedRemove(index);
}

pub fn addInst(b: *BlockEditor, inst: Inst) !Hir.Index {
    const index = try b.hg.addInstUnlinked(inst);
    try b.linkInst(index);

    return index;
}

pub inline fn numInsts(b: *BlockEditor) u32 {
    return @intCast(b.insts.items.len);
}

pub fn addInner(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Inst {
    const active_field = comptime Hir.activeDataField(tag);
    const inst: Hir.Inst = switch (active_field) {
        .placeholder => .{ .tag = tag, .data = .{ .placeholder = {} } },
        .int,
        .float,
        .ty,
        .node,
        .token,
        => inst: {
            const field_name = @tagName(active_field);
            const result = @unionInit(Hir.Inst.Data, field_name, @field(data, field_name));
            break :inst .{ .tag = tag, .data = result };
        },
        .un_node,
        .un_tok,
        .ty_op,
        => inst: {
            const T = std.meta.TagPayloadByName(Inst.Data, @tagName(active_field));
            var inner: T = undefined;
            const fields = std.meta.fields(@TypeOf(data));
            inline for (fields) |field| {
                @field(inner, field.name) = @field(data, field.name);
            }
            const field_name = @tagName(active_field);
            const result = @unionInit(Hir.Inst.Data, field_name, inner);
            break :inst .{ .tag = tag, .data = result };
        },
        .pl_node => inst: {
            const T = Hir.payloadType(tag);
            var payload: T = undefined;
            const fields = std.meta.fields(T);
            inline for (fields) |field| {
                @field(payload, field.name) = @field(data, field.name);
            }
            const pl = try b.hg.addExtra(payload);
            break :inst .{ .tag = tag, .data = .{ .pl_node = .{ .pl = pl, .node = data.node } } };
        },
        .pl_tok => unreachable,
    };

    return inst;
}

pub fn add(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Index {
    const inst = try b.addInner(tag, data);
    if (Hir.InstData(tag) == void) {} else if (@hasField(Hir.InstData(tag), "node")) {
        try b.hg.annot.append(b.hg.gpa, .{ .node = data.node });
    } else if (@hasField(Hir.InstData(tag), "tok")) {
        try b.hg.annot.append(b.hg.gpa, .{ .token = data.tok });
    } else {
        try b.hg.annot.append(b.hg.gpa, undefined);
    }
    return b.addInst(inst);
}

pub fn addUnlinked(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Index {
    const inst = try b.addInner(tag, data);
    if (Hir.InstData(tag) == void) {} else if (@hasField(Hir.InstData(tag), "node")) {
        try b.hg.annot.append(b.hg.gpa, .{ .node = data.node });
    } else if (@hasField(Hir.InstData(tag), "tok")) {
        try b.hg.annot.append(b.hg.gpa, .{ .token = data.tok });
    } else {
        try b.hg.annot.append(b.hg.gpa, undefined);
    }
    return b.hg.addInstUnlinked(inst);
}

pub fn insert(b: *BlockEditor, i: usize, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Index {
    const index = try b.addUnlinked(tag, data);
    if (Hir.InstData(tag) == void) {} else if (@hasField(Hir.InstData(tag), "node")) {
        try b.hg.annot.append(b.hg.gpa, .{ .node = data.node });
    } else if (@hasField(Hir.InstData(tag), "tok")) {
        try b.hg.annot.append(b.hg.gpa, .{ .token = data.tok });
    } else {
        try b.hg.annot.append(b.hg.gpa, undefined);
    }
    try b.insertInst(i, index);
    return index;
}

pub fn put(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag), index: Hir.Index) !void {
    const inst = try b.addInner(tag, data);
    b.hg.insts.items(.tag)[index] = inst.tag;
    b.hg.insts.items(.data)[index] = inst.data;
}

// pub fn addValue(b: *BlockEditor, val: Value) !u32 {
//     const hg = b.hg;
//     const len: u32 = @intCast(hg.values.items.len);
//     try hg.values.append(hg.gpa, val);
//
//     return len;
// }

pub fn addType(b: *BlockEditor, _ty: Type) !Hir.Index {
    return b.addInst(.{
        .tag = .ty,
        .data = .{ .ty = _ty },
    });
}

pub fn addPointerTy(b: *BlockEditor, pointee: Hir.Index, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .pointer_ty,
        .data = .{ .un_node = .{ .node = node, .operand = pointee } },
    });
}

pub fn addConstant(b: *BlockEditor, _ty: Type, val: Value, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Constant{
        .val = try b.hg.pool.internValue(val),
        .ty = try b.addType(_ty),
    });

    return b.addInst(.{
        .tag = .constant,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addIntConstant(b: *BlockEditor, ty: Type, int: u64, node: Node.Index) !u32 {
    std.debug.assert(ty.basic.width <= 64);
    return b.add(.constant, .{
        .ty = try b.addType(ty),
        .val = try b.addIntValue(ty, int),
        .node = node,
    });
}

pub fn addIntValue(b: *BlockEditor, ty: Type, int: u64) !ValueHandle {
    const hg = b.hg;
    _ = ty;
    return hg.pool.internValue(.{ .integer = int });
}

pub fn addFloatConstant(b: *BlockEditor, ty: Type, f: f64, node: Node.Index) !Hir.Index {
    return b.add(.constant, .{
        .ty = ty,
        .val = b.addFloatValue(f),
        .node = node,
    });
}

pub fn addFloatValue(b: *BlockEditor, float: f64) !u32 {
    const hg = b.hg;
    return hg.pool.internValue(.{ .float = float });
}

pub fn addBlock(b: *BlockEditor, data_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(data_block.insts.items.len),
        .head = @intCast(hg.block_slices.items.len),
    });

    const slice = try data_block.insts.toOwnedSlice(hg.gpa);
    try hg.block_slices.append(hg.gpa, slice);
    try hg.annot.append(hg.gpa, .{ .node = node });

    return b.addInst(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockUnlinked(b: *BlockEditor, data_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(data_block.insts.items.len),
        .head = @intCast(hg.block_slices.items.len),
    });

    const slice = try data_block.insts.toOwnedSlice(hg.gpa);
    try hg.block_slices.append(hg.gpa, slice);
    try hg.annot.append(hg.gpa, .{ .node = node });

    return b.hg.addInstUnlinked(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockInline(b: *BlockEditor, inline_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(inline_block.insts.items.len),
        .head = @intCast(hg.block_slices.items.len),
    });

    const slice = try inline_block.insts.toOwnedSlice(hg.gpa);
    try hg.block_slices.append(hg.gpa, slice);
    try hg.annot.append(hg.gpa, .{ .node = node });

    return b.addInst(.{
        .tag = .block_inline,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockInlineUnlinked(hg: *HirGen, inline_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(inline_block.insts.items.len),
        .head = @intCast(hg.block_slices.items.len),
    });

    const slice = try inline_block.insts.toOwnedSlice(hg.gpa);
    try hg.block_slices.append(hg.gpa, slice);
    try hg.annot.append(hg.gpa, .{ .node = node });

    return hg.addInstUnlinked(.{
        .tag = .block_inline,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn updateBlock(hg: *HirGen, data_block: *BlockEditor, inst: Hir.Index) !void {
    const pl = hg.insts.items(.data)[inst].pl_node.pl;
    var data = hg.extraData(pl, Inst.Block);

    hg.gpa.free(hg.block_slices.items[data.head]);
    const slice = try data_block.insts.toOwnedSlice(hg.gpa);
    hg.block_slices.items[data.head] = slice;
}

fn replaceInstUsesWith(b: *BlockEditor, old: Hir.Index, new: Hir.Index, comptime tag: Hir.Inst.Tag, inst: Hir.Index) error{OutOfMemory}!void {
    const hg = b.hg;
    const active_field = comptime Hir.activeDataField(tag);
    switch (active_field) {
        // nothing to do here, these instructions don't store inst references
        .placeholder, .int, .float, .ty, .node, .token => {},
        inline .un_node, .un_tok, .ty_op => {
            // there's a single operand reference, replace it if it matches
            var data = hg.get(inst, tag);
            if (data.operand == old) {
                data.operand = new;
            }

            // and "replace" the instruction
            try b.put(tag, data, inst);
        },
        .pl_node, .pl_tok => {
            var data = hg.get(inst, tag);
            const payload_type = Hir.payloadType(tag);
            const fields = switch (payload_type) {
                Inst.Call => .{"ptr"},
                Inst.Binary => .{ "lref", "rref" },
                Inst.FnDecl => .{ "return_type", "body" },
                Inst.Param => .{"ty"},
                Inst.Store => .{ "ptr", "val" },
                Inst.Coerce, Inst.Extend => .{ "val", "ty" },
                Inst.Constant => .{"ty"},
                Inst.BranchSingle => .{ "condition", "exec_true" },
                Inst.BranchDouble => .{ "condition", "exec_true", "exec_false" },
                Inst.Loop => .{ "condition", "body" },
                Inst.Block => .{},
                Inst.Module => .{},
                Inst.ParamType => .{"ptr"},
                Inst.GlobalOperand => .{ "handle", "operand" },
                Inst.FunctionType => .{"return_type"},
                Inst.ArrayInitializer => .{},
                Inst.ArrayType => .{ "element_type", "count" },
                Inst.ArrayAccess => .{ "array", "index" },
                Inst.SliceInit => .{ "element_type", "ptr", "len" },
                else => unreachable,
            };

            inline for (fields) |field| {
                if (@field(data, field) == old) {
                    @field(data, field) = new;
                }
            }

            // and "replace" the instruction
            try b.put(tag, data, inst);

            switch (tag) {
                .call => {
                    data = hg.get(inst, .call);
                    const old_args = hg.extra.items[data.args_start..data.args_end];
                    for (old_args, 0..) |old_arg, i| {
                        if (old_arg == old) {
                            old_args[i] = new;
                        }
                    }
                },
                .create_function_type => {
                    data = hg.get(inst, .create_function_type);
                    const old_params = hg.extra.items[data.params_start..data.params_end];
                    for (old_params, 0..) |old_param, i| {
                        if (old_param == old) {
                            old_params[i] = new;
                        }
                    }
                },
                .array_init => {
                    data = hg.get(inst, .array_init);
                    const elements = hg.extra.items[data.elements_start..data.elements_end];
                    for (elements, 0..) |old_element, i| {
                        if (old_element == old) {
                            elements[i] = new;
                        }
                    }
                },
                .array => {
                    data = hg.get(inst, .array);
                    const elements = hg.extra.items[data.elements_start..data.elements_end];
                    for (elements, 0..) |old_element, i| {
                        if (old_element == old) {
                            elements[i] = new;
                        }
                    }
                },
                else => {},
            }
        },
    }

    try hg.explore(inst, replaceBlockUsesWith, .{ b, old, new });
}

fn replaceBlockUsesWith(b: *BlockEditor, old: Hir.Index, new: Hir.Index, block: Hir.Index) !void {
    const hg = b.hg;
    const block_data = hg.get(block, .block);
    const insts = hg.block_slices.items[block_data.head];
    for (insts) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            inline else => |tag| try b.replaceInstUsesWith(old, new, tag, inst),
        }
    }
}

pub fn replaceAllUsesWith(b: *BlockEditor, old: Hir.Index, new: Hir.Index) !void {
    const hg = b.hg;
    for (b.insts.items) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            inline else => |tag| try b.replaceInstUsesWith(old, new, tag, inst),
        }
    }
}

pub fn commit(b: *BlockEditor) Inst.Block {
    return .{
        .len = @intCast(b.insts.len),
        .head = b.insts.head,
    };
}

pub fn addCall(b: *BlockEditor, ptr: Hir.Index, args: []u32, node: Node.Index) !Hir.Index {
    const args_start: u32 = @intCast(b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, args);
    const args_end: u32 = @intCast(b.hg.extra.items.len);

    return b.add(.call, .{
        .ptr = ptr,
        .args_start = args_start,
        .args_end = args_end,
        .node = node,
    });
}

pub fn addFunctionType(b: *BlockEditor, return_type: Hir.Index, params: []Hir.Index, node: Node.Index) !Hir.Index {
    const params_start: u32 = @intCast(b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, params);
    const params_end: u32 = @intCast(b.hg.extra.items.len);

    return b.add(.create_function_type, .{
        .return_type = return_type,
        .params_start = params_start,
        .params_end = params_end,
        .node = node,
    });
}

pub fn addArrayInit(b: *BlockEditor, elements: []Hir.Index, node: Node.Index) !Hir.Index {
    const elements_start: u32 = @intCast(b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, elements);
    const elements_end: u32 = @intCast(b.hg.extra.items.len);

    return b.add(.array_init, .{
        .elements_start = elements_start,
        .elements_end = elements_end,
        .node = node,
    });
}

pub fn addArray(b: *BlockEditor, elements: []Hir.Index, node: Node.Index) !Hir.Index {
    const elements_start: u32 = @intCast(b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, elements);
    const elements_end: u32 = @intCast(b.hg.extra.items.len);

    return b.add(.array, .{
        .elements_start = elements_start,
        .elements_end = elements_end,
        .node = node,
    });
}

pub fn addFnDecl(b: *BlockEditor, params: []u32, return_type: Hir.Index, body: Hir.Index, hash: u64, node: Node.Index) !Hir.Index {
    const params_start: u32 = @intCast(b.hg.extra.items.len);
    try b.hg.extra.appendSlice(b.hg.gpa, params);
    const params_end: u32 = @intCast(b.hg.extra.items.len);

    const pl = try b.hg.addExtra(Inst.FnDecl{
        .params_start = params_start,
        .params_end = params_end,
        .return_type = return_type,
        .body = body,
        .hash_lower = @truncate(hash),
        .hash_upper = @truncate(hash >> 32),
    });

    try b.hg.annot.append(b.hg.gpa, .{ .node = node });

    return b.addInst(.{
        .tag = .fn_decl,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBinary(b: *BlockEditor, tag: Hir.Inst.Tag, lref: Hir.Index, rref: Hir.Index, node: Node.Index) !Hir.Index {
    return switch (tag) {
        inline .add,
        .sub,
        .mul,
        .div,
        .mod,
        .cmp_eq,
        .cmp_ne,
        .cmp_gt,
        .cmp_ge,
        .cmp_lt,
        .cmp_le,
        .icmp_eq,
        .icmp_ne,
        .icmp_ugt,
        .icmp_uge,
        .icmp_ult,
        .icmp_ule,
        .icmp_sgt,
        .icmp_sge,
        .icmp_slt,
        .icmp_sle,
        .fcmp_gt,
        .fcmp_ge,
        .fcmp_lt,
        .fcmp_le,
        => |t| try b.add(t, .{ .lref = lref, .rref = rref, .node = node }),
        else => unreachable,
    };
}

pub fn addAllocaUnlinked(b: *BlockEditor, ty: Hir.Index, slot_ty: Hir.Index, node: Node.Index) !Hir.Index {
    try b.hg.annot.append(b.hg.gpa, .{ .node = node });
    return b.hg.addInstUnlinked(.{
        .tag = .alloca,
        .data = .{ .ty_op = .{ .ty = ty, .operand = slot_ty } },
    });
}
