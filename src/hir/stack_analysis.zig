// The stack analysis pass is a small pass that consumes push instructions,
// emitting alloca and store instructions instead. All alloca instructions
// are coalesced to the beginning of a function body, allowing easier analysis
// by LLVM (mem2reg doesn't work without this) and other passes.
// In the future, this pass will also perform other analysis like detecting
// objects returned by reference from inside the stack frame, and monitoring
// the size of the stack.

const std = @import("std");
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const Value = @import("../value.zig").Value;
const BlockEditor = @import("BlockEditor.zig");

const indexToRef = Hir.Inst.indexToRef;
const refToIndex = Hir.Inst.refToIndex;

pub fn executePass(hg: *HirGen, module_index: Hir.Index) !void {
    const module_pl = hg.insts.items(.data)[module_index].pl_node.pl;
    const module_data = hg.extraData(module_pl, Hir.Inst.Module);

    var extra_index: u32 = 0;
    while (extra_index < module_data.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        // load the inline block
        const block_inline_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const block_inline = hg.extraData(block_inline_pl, Hir.Inst.Block);

        const slice = hg.block_slices.items[block_inline.head];
        for (slice) |block_inst| {
            if (hg.insts.items(.tag)[block_inst] == .constant) {
                const const_data = hg.insts.items(.data)[block_inst];
                const constant = hg.extraData(const_data.pl_node.pl, Hir.Inst.Constant);
                const ty = try hg.resolveType(constant.ty);

                if (ty.kind() == .function) {
                    const val = hg.values.items[constant.val];
                    const payload = val.extended.cast(Value.Function).?;
                    try processFunction(hg, payload.body);
                }
            }
        }
    }
}

fn processFunction(hg: *HirGen, body: Hir.Index) !void {
    var allocas = std.ArrayListUnmanaged(Hir.Index){};
    defer allocas.deinit(hg.arena);

    try processBlock(hg, body, &allocas); // search through and replace pushes
    // now recreate the block with all allocas at beginning
    var b = try BlockEditor.init(hg);
    const data = hg.insts.items(.data)[body];
    const block_data = hg.extraData(data.pl_node.pl, Hir.Inst.Block);
    const insts = hg.block_slices.items[block_data.head];
    // params need to be first
    for (insts) |inst| {
        if (hg.insts.items(.tag)[inst] != .param) break;
        try b.linkInst(inst);
    }
    for (allocas.items) |alloca| {
        try b.linkInst(alloca);
    }
    for (insts) |inst| {
        if (hg.insts.items(.tag)[inst] == .param) continue;
        try b.linkInst(inst);
    }

    try BlockEditor.updateBlock(hg, &b, body);
}

fn processBlock(hg: *HirGen, block: Hir.Index, allocas: *std.ArrayListUnmanaged(Hir.Index)) !void {
    const data = hg.insts.items(.data)[block];
    const block_data = hg.extraData(data.pl_node.pl, Hir.Inst.Block);
    var b = try BlockEditor.init(hg);
    const insts = hg.block_slices.items[block_data.head];

    for (insts) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            .push => {
                const push_data = hg.insts.items(.data)[inst].un_node;
                const ty = try b.addType(try hg.resolveType(push_data.operand));
                const alloca = try b.addAllocaUnlinked(ty, push_data.node);
                try allocas.append(hg.arena, alloca);

                _ = try b.addStore(alloca, push_data.operand, push_data.node);
                try b.addRemap(inst, indexToRef(alloca));
            },
            .branch_single => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const branch_single = hg.extraData(pl, Hir.Inst.BranchSingle);

                try processBlock(hg, branch_single.exec_true, allocas);
                try b.linkInst(inst);
            },
            .branch_double => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const branch_double = hg.extraData(pl, Hir.Inst.BranchDouble);

                try processBlock(hg, branch_double.exec_true, allocas);
                try processBlock(hg, branch_double.exec_false, allocas);
                try b.linkInst(inst);
            },
            .loop => {
                const pl = hg.insts.items(.data)[inst].pl_node.pl;
                const loop = hg.extraData(pl, Hir.Inst.Loop);

                try processBlock(hg, loop.body, allocas);
                try b.linkInst(inst);
            },
            .block => {
                try processBlock(hg, inst, allocas);
                try b.linkInst(inst);
            },
            else => try b.linkInst(inst),
        }
    }

    try BlockEditor.updateBlock(hg, &b, block);
}
