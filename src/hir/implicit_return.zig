// The implicit return pass inserts implicit "return void" insts
// to the end of any function that doesn't include an explicit return statement
// (void or non-void) under all code execution passes (i.e. branches)
// it doesn't check if void is the return type of the function - this will be
// checked and raised during type verification

const std = @import("std");
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const BlockEditor = @import("BlockEditor.zig");
const Ref = Hir.Ref;

pub fn executePass(hg: *HirGen) !void {
    const module_index = hg.insts.len - 1;
    const module_pl = hg.insts.items(.data)[module_index].pl_node.pl;
    const data = hg.extraData(module_pl, Hir.Inst.Module);

    var extra_index: u32 = 0;
    while (extra_index < data.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        // load the inline block
        const block_inline_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const block_inline = hg.extraData(block_inline_pl, Hir.Inst.Block);

        var it = block_inline.iterate(hg.block_tape.items);
        while (it.next()) |block_inst| {
            if (hg.insts.items(.tag)[block_inst] == .fn_decl) {
                const fn_pl = hg.insts.items(.data)[block_inst].pl_node.pl;
                const fn_decl = hg.extraData(fn_pl, Hir.Inst.FnDecl);

                if (!instructionReturns(hg, fn_decl.body)) {
                    const block_pl = hg.insts.items(.data)[fn_decl.body].pl_node.pl;
                    const block = hg.extraData(block_pl, Hir.Inst.Block);
                    var editor = try BlockEditor.edit(hg, &block);

                    editor.insts.cursorSeekToTail();
                    _ = try editor.addRetImplicit(Ref.void_val, undefined);

                    const new_block = editor.commit();
                    hg.updateExtra(block_pl, new_block);
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

            var last: Hir.Index = undefined;
            var it = block.iterate(hg.block_tape.items);
            while (it.next()) |block_inst| {
                last = block_inst;
            }

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