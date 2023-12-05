const std = @import("std");
const Allocator = std.mem.Allocator;
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Type = @import("../typing.zig").Type;
const Value = @import("../value.zig").Value;

const Inst = Hir.Inst;
const BlockEditor = @This();

hg: *HirGen,
// insts: InstList,
insts: std.ArrayListUnmanaged(Hir.Index),
// TODO: this leaks once the editor is discarded, we need a deinit for BlockEditor
remaps: std.AutoHashMapUnmanaged(Hir.Index, Hir.Ref),
scratch: std.ArrayListUnmanaged(u32),

pub fn init(hg: *HirGen) !BlockEditor {
    return .{
        .hg = hg,
        .insts = .{},
        .remaps = .{},
        .scratch = .{},
    };
}

// pub fn edit(hg: *HirGen, block: *const Hir.Inst.Block) !BlockEditor {
// return .{ .hg = hg, .insts = .{
//     .gpa = hg.gpa,
//     .tape = &hg.block_tape,
//     .head = block.head,
//     .len = block.len,
//     .cursor = .{ .node = block.head, .pos = 0 },
//     .swap_next = null,
// }, .scratch = .{} };
// }

pub inline fn linkInst(b: *BlockEditor, inst: Hir.Index) !void {
    try b.insts.append(b.hg.gpa, inst);
}

pub fn addInst(b: *BlockEditor, inst: Inst) !Hir.Index {
    const index = try b.hg.addInstUnlinked(inst);
    try b.linkInst(index);

    return index;
}

pub inline fn numInsts(b: *BlockEditor) u32 {
    return @intCast(b.insts.items.len);
}

pub inline fn addRemap(b: *BlockEditor, src: Hir.Index, dest: Hir.Ref) !void {
    // TODO: should we use arena?
    try b.remaps.put(b.hg.gpa, src, dest);
}

pub fn resolveRef(b: *BlockEditor, src: Hir.Ref) Hir.Ref {
    if (Hir.Inst.refToIndex(src)) |index| {
        if (b.remaps.get(index)) |remap| {
            return remap;
        } else {
            return Hir.Inst.indexToRef(index);
        }
    } else {
        return src;
    }
}

pub fn addValue(b: *BlockEditor, val: Value) !u32 {
    const hg = b.hg;
    const len: u32 = @intCast(hg.values.items.len);
    try hg.values.append(hg.gpa, val);

    return len;
}

// turns a type into a reference to be passed into an instruction
// for many basic types, there are built in refs
// special, derived, and aggregate types insert a type
pub fn typeToRef(b: *BlockEditor, ty: Type) !Hir.Ref {
    return switch (ty.kind()) {
        .void => .void_ty,
        .uint => switch (ty.basic.width) {
            1 => .bool_ty,
            8 => .u8_ty,
            16 => .u16_ty,
            32 => .u32_ty,
            64 => .u64_ty,
            else => Hir.Inst.indexToRef(try b.addType(ty)),
        },
        .sint => switch (ty.basic.width) {
            8 => .i8_ty,
            16 => .i16_ty,
            32 => .i32_ty,
            64 => .i64_ty,
            else => Hir.Inst.indexToRef(try b.addType(ty)),
        },
        .float => switch (ty.basic.width) {
            32 => .f32_ty,
            64 => .f64_ty,
            else => error.NotImplemented,
        },
        .comptime_uint => .comptime_uint,
        .comptime_sint => .comptime_sint,
        .comptime_float => .comptime_float,
        else => return Hir.Inst.indexToRef(try b.addType(ty)),
    };
}

// pub fn addType(b: *BlockEditor, _ty: Type) !u32 {
pub fn addType(b: *BlockEditor, _ty: Type) !Hir.Index {
    // const hg = b.hg;
    // const len: u32 = @intCast(hg.types.items.len);
    // try hg.types.append(hg.gpa, _ty);

    // return len;
    return b.addInst(.{
        .tag = .ty,
        .data = .{ .ty = _ty },
    });
}

pub fn addInt(b: *BlockEditor, int: u64) !Hir.Index {
    return b.addInst(.{
        .tag = .int,
        .data = .{ .int = int },
    });
}

pub fn addFloat(b: *BlockEditor, float: f64) !Hir.Index {
    return b.addInst(.{
        .tag = .float,
        .data = .{ .float = float },
    });
}

pub fn addConstant(b: *BlockEditor, _ty: Type, val: Value, node: Node.Index) !Hir.Index {
    const ty_ref = try b.typeToRef(_ty);
    const pl = try b.hg.addExtra(Inst.Constant{
        .val = try b.addValue(val),
        .ty = ty_ref,
    });

    return b.addInst(.{
        .tag = .constant,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addIntConstant(b: *BlockEditor, ty: Type, int: u64, node: Node.Index) !u32 {
    std.debug.assert(ty.basic.width <= 64);
    if (int == 0) return b.addConstant(ty, .{ .tag = .zero }, node);
    if (int == 1) return b.addConstant(ty, .{ .tag = .one }, node);
    if (ty.basic.kind == .comptime_uint or ty.basic.kind == .comptime_sint) {
        if (int <= std.math.maxInt(u32)) {
            var payload = try b.hg.gpa.create(Value.Payload.U32);
            payload.* = .{ .int = @truncate(int) };
            return b.addConstant(ty, .{ .payload = &payload.base }, node);
        } else {
            var payload = try b.hg.gpa.create(Value.Payload.U64);
            payload.* = .{ .int = int };
            return b.addConstant(ty, .{ .payload = &payload.base }, node);
        }
    } else if (ty.basic.width <= 32) {
        var payload = try b.hg.gpa.create(Value.Payload.U32);
        payload.* = .{ .int = @truncate(int) };
        return b.addConstant(ty, .{ .payload = &payload.base }, node);
    } else {
        var payload = try b.hg.gpa.create(Value.Payload.U64);
        payload.* = .{ .int = int };
        return b.addConstant(ty, .{ .payload = &payload.base }, node);
    }
}

pub fn addFloatConstant(b: *BlockEditor, ty: Type, f: f64, node: Node.Index) !u32 {
    var payload = try b.hg.gpa.create(Value.Payload.F64);
    payload.* = .{ .float = f };
    return b.addConstant(ty, .{ .payload = &payload.base }, node);
}

pub fn addBinary(b: *BlockEditor, l: Hir.Ref, r: Hir.Ref, tag: Inst.Tag, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Binary{
        .lref = l,
        .rref = r,
    });

    return b.addInst(.{
        .tag = tag,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addUnary(b: *BlockEditor, op: Hir.Ref, tag: Inst.Tag, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = tag,
        .data = .{ .un_node = .{ .operand = op, .node = node } },
    });
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

pub fn commitRemap(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Ref), head: u32) void {
    const slice = hg.block_slices.items[head];
    for (slice) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            .int, .float, .ty => {},
            .add, .sub, .mul, .div, .mod, .cmp_eq, .cmp_ne, .cmp_gt, .cmp_ge, .cmp_lt, .cmp_le, .icmp_eq, .icmp_ne, .icmp_ugt, .icmp_uge, .icmp_ult, .icmp_ule, .icmp_sgt, .icmp_sge, .icmp_slt, .icmp_sle, .fcmp_gt, .fcmp_ge, .fcmp_lt, .fcmp_le => {
                // Binary
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapRefPl(hg, remaps, pl + 0); // lref
                remapRefPl(hg, remaps, pl + 1); // rref
            },
            .lsl, .lsr, .asl, .asr => unreachable,
            .coerce => {
                // Coerce
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapRefPl(hg, remaps, pl + 0); // val
                remapRefPl(hg, remaps, pl + 1); // ty
            },
            .constant => {
                // Constant
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapRefPl(hg, remaps, pl + 1); // ty
            },
            .zext, .sext, .fpext => {
                // Extend
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapRefPl(hg, remaps, pl + 0); // val
                remapRefPl(hg, remaps, pl + 1); // ty
            },
            .push, .global_mut, .link_extern => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_node.operand;
                remapRef(hg, remaps, op);
            },
            .neg, .log_not, .bit_not => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_node.operand;
                remapRef(hg, remaps, op);
            },
            .load => {
                // Payload
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                if (remaps.get(pl)) |new| {
                    hg.insts.items(.data)[inst].pl_node.pl = Hir.Inst.refToIndex(new).?;
                }
            },
            .store => {
                // Store
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                remapIndexPl(hg, remaps, pl + 0); // addr
                remapRefPl(hg, remaps, pl + 1); // val
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
                remapRefPl(hg, remaps, pl + 1); // ty
            },
            .call => {
                // Call
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.Call);
                remapRefPl(hg, remaps, pl + 0); // addr

                var extra_index: u32 = 0;
                while (extra_index < data.args_len) : (extra_index += 1) {
                    // std.debug.print("remapping {} {}\n", .{ extra_index, hg.extra.items[extra_index] });
                    remapRefPl(hg, remaps, pl + 2 + extra_index);
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

                remapRefPl(hg, remaps, pl + 0); // condition

                const block_pl = hg.insts.items(.data)[data.exec_true].pl_node.pl;
                const block_data = hg.extraData(block_pl, Hir.Inst.Block);
                commitRemap(hg, remaps, block_data.head);
            },
            .branch_double => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const data = hg.extraData(pl, Hir.Inst.BranchDouble);

                remapRefPl(hg, remaps, pl + 0); // condition

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
                remapRef(hg, remaps, op);
            },
            .ret_node, .yield_node, .yield_inline => {
                // unary operand
                var op = &hg.insts.slice().items(.data)[inst].un_node.operand;
                remapRef(hg, remaps, op);
            },
            .alloca, .load_global => {},
            .dbg_value, .dbg_declare, .dbg_assign => {},
            .module => unreachable,
        }
    }
}

// TODO: probably better to repalce hashmap with slice
fn remapRefPl(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Ref), pl: u32) void {
    const ref = &hg.extra.items[pl];
    if (Hir.Inst.refToIndex(@enumFromInt(hg.extra.items[pl]))) |inst| {
        if (remaps.get(inst)) |new| {
            ref.* = @intFromEnum(new);
        }
    }
}

fn remapRef(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Ref), ref: *Hir.Ref) void {
    _ = hg;
    if (Hir.Inst.refToIndex(ref.*)) |inst| {
        if (remaps.get(inst)) |new| {
            ref.* = new;
        }
    }
}

fn remapIndexPl(hg: *HirGen, remaps: *std.AutoHashMapUnmanaged(Hir.Index, Hir.Ref), pl: u32) void {
    const index = &hg.extra.items[pl];
    if (remaps.get(index.*)) |new| {
        index.* = Hir.Inst.refToIndex(new).?;
    }
}

pub fn commit(b: *BlockEditor) Inst.Block {
    return .{
        .len = @intCast(b.insts.len),
        .head = b.insts.head,
    };
}

pub fn addCall(b: *BlockEditor, addr: Hir.Ref, args: []u32, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Call{
        .addr = addr,
        .args_len = @intCast(args.len),
    });
    try b.hg.extra.appendSlice(b.hg.gpa, args);

    return b.addInst(.{
        .tag = .call,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addFnDecl(b: *BlockEditor, params: []u32, return_type: Hir.Ref, body: Hir.Index, hash: u64, node: Node.Index) !Hir.Index {
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

pub fn addParam(b: *BlockEditor, name: u32, ty_ref: Hir.Ref, node: Node.Index) !Hir.Index {
    const hg = b.hg;
    const pl = try hg.addExtra(Inst.Param{
        .name = name,
        .ty = ty_ref,
    });

    return b.hg.addInstUnlinked(.{
        .tag = .param,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addDebugValue(b: *BlockEditor, val: Hir.Ref, name: u32, node: Node.Index) !Hir.Index {
    // TODO: enum and member naming
    const pl = try b.hg.addExtra(Inst.DebugValue{
        .name = name,
        .value = val,
    });

    return b.addInst(.{
        .tag = .dbg_value,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addPush(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .push,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addGlobal(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .global,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addGlobalMut(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .global_mut,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addLoad(b: *BlockEditor, addr: Hir.Index, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .load,
        .data = .{ .pl_node = .{ .pl = addr, .node = node } },
    });
}

pub fn addLoadGlobal(b: *BlockEditor, decl: Hir.Index, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .load_global,
        .data = .{ .pl_node = .{ .pl = decl, .node = node } },
    });
}

pub fn addStore(b: *BlockEditor, addr: Hir.Index, val: Hir.Ref, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Store{
        .addr = addr,
        .val = val,
    });

    return b.addInst(.{
        .tag = .store,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBranchSingle(b: *BlockEditor, cond: Hir.Ref, exec: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.BranchSingle{
        .condition = cond,
        .exec_true = exec,
    });

    return b.addInst(.{
        .tag = .branch_single,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBranchDouble(b: *BlockEditor, cond: Hir.Ref, x: Hir.Index, y: Hir.Index, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.BranchDouble{
        .condition = cond,
        .exec_true = x,
        .exec_false = y,
    });

    return b.addInst(.{
        .tag = .branch_double,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addRetNode(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .ret_node,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addRetImplicit(b: *BlockEditor, val: Hir.Ref, tok: Ast.TokenIndex) !Hir.Index {
    return b.addInst(.{
        .tag = .ret_implicit,
        .data = .{ .un_tok = .{ .operand = val, .tok = tok } },
    });
}

pub fn addYieldImplicit(b: *BlockEditor, val: Hir.Ref, tok: Ast.TokenIndex) !Hir.Index {
    return b.addInst(.{
        .tag = .yield_implicit,
        .data = .{ .un_tok = .{ .operand = val, .tok = tok } },
    });
}

pub fn addYieldInline(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .yield_inline,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
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

pub fn addBreak(b: *BlockEditor, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .loop_break,
        .data = .{ .node = node },
    });
}

pub fn addLinkExtern(b: *BlockEditor, ref: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .link_extern,
        .data = .{ .un_node = .{ .operand = ref, .node = node } },
    });
}

pub fn addZext(b: *BlockEditor, val: Hir.Ref, ty: Hir.Ref, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .zext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addSext(b: *BlockEditor, val: Hir.Ref, ty: Hir.Ref, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .sext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addFpext(b: *BlockEditor, val: Hir.Ref, ty: Hir.Ref, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Hir.Inst.Extend{
        .val = val,
        .ty = ty,
    });

    return b.addInst(.{
        .tag = .fpext,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addAllocaUnlinked(b: *BlockEditor, ty: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.hg.addInstUnlinked(.{
        .tag = .alloca,
        .data = .{ .un_node = .{ .operand = ty, .node = node } },
    });
}
