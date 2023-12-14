const std = @import("std");
const Allocator = std.mem.Allocator;
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Type = @import("type.zig").Type;
const Value = @import("../value.zig").Value;

const Inst = Hir.Inst;
const BlockEditor = @This();

hg: *HirGen,
// insts: InstList,
insts: std.ArrayListUnmanaged(Hir.Index),
// TODO: this leaks once the editor is discarded, we need a deinit for BlockEditor
remaps: std.AutoHashMapUnmanaged(Hir.Index, Hir.Index),
scratch: std.ArrayListUnmanaged(u32),

pub fn init(hg: *HirGen) !BlockEditor {
    return .{
        .hg = hg,
        .insts = .{},
        .remaps = .{},
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
        .remaps = .{},
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

pub inline fn addRemap(b: *BlockEditor, src: Hir.Index, dest: Hir.Index) !void {
    // TODO: should we use arena?
    try b.remaps.put(b.hg.gpa, src, dest);
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
    return b.addInst(inst);
}

pub fn addUnlinked(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Index {
    const inst = try b.addInner(tag, data);
    return b.hg.addInstUnlinked(inst);
}

pub fn insert(b: *BlockEditor, i: usize, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag)) !Hir.Index {
    const index = try b.addUnlinked(tag, data);
    try b.insertInst(i, index);
    return index;
}

pub fn put(b: *BlockEditor, comptime tag: Hir.Inst.Tag, data: Hir.InstData(tag), index: Hir.Index) !void {
    const inst = try b.addInner(tag, data);
    b.hg.insts.items(.tag)[index] = inst.tag;
    b.hg.insts.items(.data)[index] = inst.data;
}

pub fn addValue(b: *BlockEditor, val: Value) !u32 {
    const hg = b.hg;
    const len: u32 = @intCast(hg.values.items.len);
    try hg.values.append(hg.gpa, val);

    return len;
}

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
        .val = try b.addValue(val),
        .ty = try b.addType(_ty),
    });

    return b.addInst(.{
        .tag = .constant,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addIntConstant(b: *BlockEditor, ty: Type, int: u64, node: Node.Index) !u32 {
    std.debug.assert(ty.basic.width <= 64);
    if (int == 0) return b.addConstant(ty, .{ .basic = .{ .kind = .zero } }, node);
    if (int == 1) return b.addConstant(ty, .{ .basic = .{ .kind = .one } }, node);
    if (ty.basic.kind == .comptime_uint or ty.basic.kind == .comptime_sint) {
        if (int <= std.math.maxInt(u32)) {
            var payload = try b.hg.gpa.create(Value.U32);
            payload.* = .{ .int = @truncate(int) };
            return b.addConstant(ty, .{ .extended = &payload.base }, node);
        } else {
            var payload = try b.hg.gpa.create(Value.U64);
            payload.* = .{ .int = int };
            return b.addConstant(ty, .{ .extended = &payload.base }, node);
        }
    } else if (ty.basic.width <= 32) {
        var payload = try b.hg.gpa.create(Value.U32);
        payload.* = .{ .int = @truncate(int) };
        return b.addConstant(ty, .{ .extended = &payload.base }, node);
    } else {
        var payload = try b.hg.gpa.create(Value.U64);
        payload.* = .{ .int = int };
        return b.addConstant(ty, .{ .extended = &payload.base }, node);
    }
}

pub fn addIntValue(b: *BlockEditor, ty: Type, int: u64) !u32 {
    if (int == 0) return b.addValue(.{ .basic = .{ .kind = .zero } });
    if (int == 1) return b.addValue(.{ .basic = .{ .kind = .one } });

    const hg = b.hg;
    if (ty.basic.kind == .comptime_uint or ty.basic.kind == .comptime_sint) {
        if (int <= std.math.maxInt(u32)) {
            var payload = try hg.gpa.create(Value.U32);
            payload.* = .{ .int = @truncate(int) };
            return b.addValue(.{ .extended = &payload.base });
        } else {
            var payload = try hg.gpa.create(Value.U64);
            payload.* = .{ .int = int };
            return b.addValue(.{ .extended = &payload.base });
        }
    } else if (ty.basic.width <= 32) {
        var payload = try hg.gpa.create(Value.U32);
        payload.* = .{ .int = @truncate(int) };
        return b.addValue(.{ .extended = &payload.base });
    } else {
        var payload = try b.hg.gpa.create(Value.U64);
        payload.* = .{ .int = int };
        return b.addValue(.{ .extended = &payload.base });
    }
}

pub fn addFloatConstant(b: *BlockEditor, ty: Type, f: f64, node: Node.Index) !u32 {
    var payload = try b.hg.gpa.create(Value.F64);
    payload.* = .{ .float = f };
    return b.addConstant(ty, .{ .extended = &payload.base }, node);
}

pub fn addFloatValue(b: *BlockEditor, float: f64) !u32 {
    var payload = try b.hg.gpa.create(Value.F64);
    payload.* = .{ .float = float };
    return b.addValue(.{ .extended = &payload.base });
}

pub fn addBlock(b: *BlockEditor, data_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(data_block.insts.items.len),
        .head = @intCast(hg.block_slices.items.len),
    });

    const slice = try data_block.insts.toOwnedSlice(hg.gpa);
    try hg.block_slices.append(hg.gpa, slice);

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

    commitRemap(hg, &data_block.remaps, data.head);
}

fn replaceInstUsesWith(b: *BlockEditor, old: Hir.Index, new: Hir.Index, comptime tag: Hir.Inst.Tag, inst: Hir.Index) error{OutOfMemory}!void {
    const hg = b.hg;
    const active_field = comptime Hir.activeDataField(tag);
    switch (active_field) {
        // nothing to do here, these instructions don't store inst references
        .placeholder, .int, .float, .ty, .node, .token => {},
        inline .un_node, .un_tok => {
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
                else => unreachable,
            };

            inline for (fields) |field| {
                if (@field(data, field) == old) {
                    @field(data, field) = new;
                }
            }

            // and "replace" the instruction
            try b.put(tag, data, inst);
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

pub fn commitRemap(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Index), head: u32) void {
    const slice = hg.block_slices.items[head];
    for (slice) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            .int,
            .float,
            .ty,
            .none,
            => {},
            .add,
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
            .lsl,
            .lsr,
            .asl,
            .asr,
            => {
                // Binary
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 0); // lref
                remapIndexPl(hg, remaps, pl + 1); // rref
            },
            .coerce => {
                // Coerce
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 0); // val
                remapIndexPl(hg, remaps, pl + 1); // ty
            },
            .constant => {
                // Constant
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 1); // ty
            },
            .zext, .sext, .fpext => {
                // Extend
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 0); // val
                remapIndexPl(hg, remaps, pl + 1); // ty
            },
            .pointer_ty,
            .push,
            .load,
            .neg,
            .log_not,
            .bit_not,
            .global_mut,
            .link_extern,
            => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_node.operand;
                remapIndex(hg, remaps, op);
            },
            .store => {
                // Store
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 0); // ptr
                remapIndexPl(hg, remaps, pl + 1); // val
            },
            .fn_decl => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.FnDecl);

                const block_pl = hg.insts.items(.data)[data.body].pl_node.pl;
                const block_data = hg.extraData(block_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, block_data.head);
            },
            .param => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 1); // ty
            },
            .call => {
                // Call
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.Call);
                remapIndexPl(hg, remaps, pl + 0); // addr

                var extra_index: u32 = 0;
                while (extra_index < data.args_len) : (extra_index += 1) {
                    remapIndexPl(hg, remaps, pl + 2 + extra_index);
                }
            },
            .block, .block_inline => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.Block);

                commitRemap(hg, remaps, data.head);
            },
            .branch_single => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.BranchSingle);

                remapIndexPl(hg, remaps, pl + 0); // condition

                const block_pl = hg.insts.items(.data)[data.exec_true].pl_node.pl;
                const block_data = hg.extraData(block_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, block_data.head);
            },
            .branch_double => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.BranchDouble);

                remapIndexPl(hg, remaps, pl + 0); // condition

                const block1_pl = hg.insts.items(.data)[data.exec_true].pl_node.pl;
                const block1_data = hg.extraData(block1_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, block1_data.head);

                const block2_pl = hg.insts.items(.data)[data.exec_false].pl_node.pl;
                const block2_data = hg.extraData(block2_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, block2_data.head);
            },
            .loop => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.Loop);

                const condition_pl = hg.insts.items(.data)[data.condition].pl_node.pl;
                const condition_data = hg.extraData(condition_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, condition_data.head);

                const body_pl = hg.insts.items(.data)[data.body].pl_node.pl;
                const body_data = hg.extraData(body_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, body_data.head);
            },
            .loop_break => {},
            .ret_implicit, .yield_implicit => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_tok.operand;
                remapIndex(hg, remaps, op);
            },
            .ret_node, .yield_node, .yield_inline => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_node.operand;
                remapIndex(hg, remaps, op);
            },
            .alloca, .load_global => {},
            // .dbg_value, .dbg_declare, .dbg_assign => {},
            .module => unreachable,
        }
    }
}

fn remapIndex(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Index), inst: *Hir.Index) void {
    _ = hg;
    if (remaps.get(inst.*)) |new| {
        inst.* = new;
    }
}

fn remapIndexPl(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Index), pl: u32) void {
    const index = &hg.extra.items[pl];
    if (remaps.get(index.*)) |new| {
        index.* = new;
    }
}

pub fn commit(b: *BlockEditor) Inst.Block {
    return .{
        .len = @intCast(b.insts.len),
        .head = b.insts.head,
    };
}

pub fn addCall(b: *BlockEditor, ptr: Hir.Index, args: []u32, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Call{
        .ptr = ptr,
        .args_len = @intCast(args.len),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, args);

    return b.addInst(.{
        .tag = .call,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
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

pub fn addParam(b: *BlockEditor, name: u32, ty: Hir.Index, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Param{
        .name = name,
        .ty = ty,
    });

    return b.hg.addInstUnlinked(.{
        .tag = .param,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addLoop(b: *BlockEditor, cond: Hir.Index, body: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Loop{
        .condition = cond,
        .body = body,
    });

    return b.addInst(.{
        .tag = .loop,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addZext(b: *BlockEditor, val: Hir.Index, ty: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .zext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addSext(b: *BlockEditor, val: Hir.Index, ty: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .sext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addFpext(b: *BlockEditor, val: Hir.Index, ty: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .fpext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addAllocaUnlinked(b: *BlockEditor, ty: Hir.Index, node: Node.Index) !Hir.Index {
    return b.hg.addInstUnlinked(.{
        .tag = .alloca,
        .data = .{ .un_node = .{ .operand = ty, .node = node } },
    });
}
