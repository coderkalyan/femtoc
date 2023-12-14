// The implicit return pass inserts implicit "return void" insts
// to the end of any function that doesn't include an explicit return statement
// (void or non-void) under all code execution passes (i.e. branches)
// it doesn't check if void is the return type of the function - this will be
// checked and raised during type verification

const std = @import("std");
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const BlockEditor = @import("BlockEditor.zig");

pub fn executePass(hg: *HirGen, module_index: Hir.Index) !void {
    const module = hg.get(module_index, .module);

    var extra_index: u32 = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module.pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        // load the inline block
        const block_inline = hg.get(inst, .block_inline);
        const block_insts = hg.block_slices.items[block_inline.head];
        for (block_insts) |block_inst| {
            if (hg.insts.items(.tag)[block_inst] == .fn_decl) {
                const fn_decl = hg.get(block_inst, .fn_decl);

                if (!instructionReturns(hg, fn_decl.body)) {
                    const block = hg.get(fn_decl.body, .block);
                    const slice = hg.block_slices.items[block.head];
                    var b = try BlockEditor.clone(hg, slice);

                    const return_val = try b.add(.none, .{});
                    _ = try b.add(.ret_implicit, .{ .operand = return_val, .tok = undefined });
                    try BlockEditor.updateBlock(hg, &b, fn_decl.body);
                }
            }
        }
    }
}

fn instructionReturns(hg: *HirGen, inst: u32) bool {
    switch (hg.insts.items(.tag)[inst]) {
        .block => {
            // returns if this block ends in a return
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const block = hg.extraData(pl, Hir.Inst.Block);
            if (block.len == 0) {
                // empty blocks don't return
                return false;
            }

            const slice = hg.block_slices.items[block.head];
            const last = slice[slice.len - 1];

            return instructionReturns(hg, last);
        },
        .branch_single => return false, // doesn't return if branch not taken
        .branch_double => {
            // returns if both paths return
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const branch_double = hg.extraData(pl, Hir.Inst.BranchDouble);
            const lret = instructionReturns(hg, branch_double.exec_true);
            const rret = instructionReturns(hg, branch_double.exec_false);
            return lret and rret;
        },
        .ret_implicit, .ret_node => return true,
        else => return false,
    }
}
