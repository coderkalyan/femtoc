const std = @import("std");
const Allocator = std.mem.Allocator;
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const InstList = @import("InstList.zig");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Type = @import("../typing.zig").Type;
const Value = @import("../value.zig").Value;

const Inst = Hir.Inst;
const BlockEditor = @This();

hg: *HirGen,
insts: InstList,
scratch: std.ArrayListUnmanaged(u32),

pub fn init(hg: *HirGen) !BlockEditor {
    return .{
        .hg = hg,
        .insts = try InstList.init(hg.gpa, &hg.block_tape),
        .scratch = .{},
    };
}

pub fn edit(hg: *HirGen, block: *const Hir.Inst.Block) !BlockEditor {
    return .{ .hg = hg, .insts = .{
        .gpa = hg.gpa,
        .tape = &hg.block_tape,
        .head = block.head,
        .len = block.len,
        .cursor = .{ .node = block.head, .pos = 0 },
    }, .scratch = .{} };
}

pub fn addInst(b: *BlockEditor, inst: Inst) !Hir.Index {
    const index = try b.hg.addInstUnlinked(inst);
    try b.insts.linkInst(index);

    return index;
}

pub inline fn numInsts(b: *BlockEditor) u32 {
    return @intCast(b.insts.len);
}

pub fn addValue(b: *BlockEditor, val: Value) !u32 {
    const hg = b.hg;
    const len: u32 = @intCast(hg.values.items.len);
    try hg.values.append(hg.gpa, val);

    return len;
}

pub fn addType(b: *BlockEditor, _ty: Type) !u32 {
    const hg = b.hg;
    const len: u32 = @intCast(hg.types.items.len);
    try hg.types.append(hg.gpa, _ty);

    return len;
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

pub fn addConstant(b: *BlockEditor, _ty: Type, val: Value, node: Node.Index) !u32 {
    const pl = try b.hg.addExtra(Inst.Constant{
        .val = try addValue(b, val),
        .ty = try addType(b, _ty),
    });

    return b.addInst(.{
        .tag = .constant,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
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

pub fn addBlock(b: *BlockEditor, data_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Block{
        .len = @intCast(data_block.insts.len),
        .head = data_block.insts.head,
    });

    return b.addInst(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockUnlinked(b: *BlockEditor, data_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Block{
        .len = @intCast(data_block.insts.len),
        .head = data_block.insts.head,
    });

    return b.hg.addInstUnlinked(.{
        .tag = .block,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockInline(b: *BlockEditor, inline_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const pl = try b.hg.addExtra(Inst.Block{
        .len = @intCast(inline_block.insts.len),
        .head = inline_block.insts.head,
    });

    return b.addInst(.{
        .tag = .block_inline,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
}

pub fn addBlockInlineUnlinked(hg: *HirGen, inline_block: *BlockEditor, node: Node.Index) !Hir.Index {
    const pl = try hg.addExtra(Inst.Block{
        .len = @intCast(inline_block.insts.len),
        .head = inline_block.insts.head,
    });

    return hg.addInstUnlinked(.{
        .tag = .block_inline,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
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

    const index: u32 = @intCast(hg.insts.len);
    try hg.insts.append(hg.gpa, .{
        .tag = .param,
        .data = .{ .pl_node = .{ .pl = pl, .node = node } },
    });
    return index;
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

pub fn addDeclConst(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .decl_const,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addDeclMut(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .decl_mut,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}

pub fn addDeclExport(b: *BlockEditor, val: Hir.Ref, node: Node.Index) !Hir.Index {
    return b.addInst(.{
        .tag = .decl_export,
        .data = .{ .un_node = .{ .operand = val, .node = node } },
    });
}
