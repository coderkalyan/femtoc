// The implicit return pass inserts implicit "return void" instructions
// to the end of any function that doesn't include an explicit return statement
// (void or non-void) under all code execution passes (i.e. branches)
// it doesn't check if void is the return type of the function - this will be
// checked and raised during type verification

const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");

fn executePass(hg: *HirGen) !void {
    _ = hg;
}

fn instructionReturns(hg: *HirGen, inst: u32) bool {
    switch (hg.instructions.items(.tag)[inst]) {
        .block => {
            // returns if this block ends in a return
            const pl = hg.instructions.items(.data)[inst].pl_node.pl;
            const block = hg.getTempHir().extraData(pl, Hir.Inst.Block);
            return blockReturns(block);
        },
        .branch_single => return false, // doesn't return if branch not taken
        .branch_double => {
            // returns if both paths return
            const data = hg.instructions.items(.data)[inst];
            const lret = instructionReturns(hg, hg.extra.items[data.pl_node.pl + 1]);
            const rret = instructionReturns(hg, hg.extra.items[data.pl_node.pl + 2]);
            return lret and rret;
        },
        .ret_implicit, .ret_node => return true,
        else => return false,
    }
}

fn blockReturns(hg: *HirGen, block: Hir.Inst.Block) bool {
    if (block.len == 0) {
        // empty blocks don't return
        return false;
    }

    var last: Hir.Index = undefined;
    var it = block.iterate();
    while (it.next()) |inst| {
        last = inst;
    }

    return instructionReturns(hg, last);
}
