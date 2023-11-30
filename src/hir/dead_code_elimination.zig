// the dead code elimination (dce) pass is run at the end of semantic analysis
// it simply passes through the each block and removes any instructions whose results
// aren't used. this isn't fancy or aggressive like LLVM's passes, but it helps to
// get rid of unused constants that have been folded, simplifies branches, etc

const std = @import("std");
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const BlockEditor = @import("BlockEditor.zig");
const Value = @import("../value.zig").Value;
const Ref = Hir.Ref;

const indexToRef = Hir.Inst.indexToRef;
const refToIndex = Hir.Inst.refToIndex;

pub fn executePass(hg: *HirGen, module_index: Hir.Index) !void {
    const module_pl = hg.insts.items(.data)[module_index].pl_node.pl;
    const module_data = hg.extraData(module_pl, Hir.Inst.Module);

    var extra_index: u32 = 0;
    while (extra_index < module_data.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        var editor = try BlockEditor.init(hg);
        try inspectBlock(&editor, inst);
    }
}

fn inspectBlock(b: *BlockEditor, inst: Hir.Index) !void {
    const hg = b.hg;
    var inner_blocks = std.ArrayListUnmanaged(u32){};
    defer inner_blocks.deinit(hg.arena);
    var usage = std.AutoHashMap(Hir.Index, u32).init(hg.arena);
    defer usage.deinit();

    const pl = hg.insts.items(.data)[inst].pl_node.pl;
    const data = hg.extraData(pl, Hir.Inst.Block);
    const slice = hg.block_slices.items[data.head];
    for (slice) |block_inst| {
        try inspectInst(b, block_inst, &usage);

        // also schedule nested blocks to be swept after this function finishes
        // going over the existing block
        // and mark useful but "unreferenced" instructions as used
        switch (hg.insts.items(.tag)[block_inst]) {
            .block, .block_inline => try inner_blocks.append(hg.arena, block_inst),
            .branch_single => {
                const branch_data = hg.insts.items(.data)[block_inst];
                const branch = hg.extraData(branch_data.pl_node.pl, Hir.Inst.BranchSingle);
                try inner_blocks.append(hg.arena, branch.exec_true);
                try usage.put(block_inst, 1);
            },
            .branch_double => {
                const branch_data = hg.insts.items(.data)[block_inst];
                const branch = hg.extraData(branch_data.pl_node.pl, Hir.Inst.BranchDouble);
                try inner_blocks.append(hg.arena, branch.exec_true);
                try inner_blocks.append(hg.arena, branch.exec_false);
                try usage.put(block_inst, 1);
            },
            .loop => {
                const loop_data = hg.insts.items(.data)[block_inst];
                const loop = hg.extraData(loop_data.pl_node.pl, Hir.Inst.Loop);
                try inner_blocks.append(hg.arena, loop.condition);
                try inner_blocks.append(hg.arena, loop.body);
                try usage.put(block_inst, 1);
            },
            .fn_decl => {
                const fn_data = hg.insts.items(.data)[block_inst];
                const fn_decl = hg.extraData(fn_data.pl_node.pl, Hir.Inst.FnDecl);
                try inner_blocks.append(hg.arena, fn_decl.body);
            },
            .constant => {
                const const_pl = hg.insts.items(.data)[block_inst].pl_node.pl;
                const const_data = hg.extraData(const_pl, Hir.Inst.Constant);
                const ty = hg.resolveType(const_data.ty);
                if (ty.kind() == .function) {
                    const val = hg.values.items[const_data.val];
                    const payload = val.payload.cast(Value.Payload.Function).?;
                    try inner_blocks.append(hg.arena, payload.func.body);
                }
            },
            .loop_break, .store, .dbg_declare, .dbg_value, .dbg_assign => {
                try usage.put(block_inst, 1);
            },
            else => {},
        }
    }

    // now rewrite the block, only linking instructions with at least one usage
    for (slice) |block_inst| {
        if (usage.get(block_inst)) |count| {
            if (count > 0) try b.linkInst(block_inst);
        }
    }
    try BlockEditor.updateBlock(hg, b, inst);

    for (inner_blocks.items) |block| {
        try inspectBlock(b, block);
    }
}

fn inspectInst(b: *BlockEditor, inst: Hir.Index, usage: *std.AutoHashMap(Hir.Index, u32)) !void {
    const hg = b.hg;
    switch (hg.insts.items(.tag)[inst]) {
        .int, .float, .ty => {},
        .add, .sub, .mul, .div, .mod, .cmp_eq, .cmp_ne, .cmp_gt, .cmp_ge, .cmp_lt, .cmp_le, .icmp_eq, .icmp_ne, .icmp_ugt, .icmp_uge, .icmp_ult, .icmp_ule, .icmp_sgt, .icmp_sge, .icmp_slt, .icmp_sle, .fcmp_gt, .fcmp_ge, .fcmp_lt, .fcmp_le => {
            // Binary
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Binary);
            try incrementUsage(usage, data.lref);
            try incrementUsage(usage, data.rref);
        },
        .lsl, .lsr, .asl, .asr => unreachable,
        .coerce => {
            // Coerce
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Coerce);
            try incrementUsage(usage, data.val);
            try incrementUsage(usage, data.ty);
        },
        .constant => {
            // Constant
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Constant);
            try incrementUsage(usage, data.ty);
        },
        .zext, .sext, .fpext => {
            // Extend
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Extend);
            try incrementUsage(usage, data.val);
            try incrementUsage(usage, data.ty);
        },
        .push => {
            // unary operand
            var op = hg.insts.slice().items(.data)[inst].un_node.operand;
            try incrementUsage(usage, op);
        },
        .load => {
            // Payload
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            try incrementUsage(usage, @enumFromInt(hg.extra.items[pl]));
        },
        .store => {
            // Store
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Store);
            try incrementUsage(usage, indexToRef(data.addr));
            try incrementUsage(usage, data.val);
        },
        .fn_decl => {}, // TODO: anything to do here?
        .param => unreachable,
        .call => {
            // Call
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.Call);
            try incrementUsage(usage, data.addr);

            var extra_index: u32 = pl + 2;
            while (extra_index < data.args_len) : (extra_index += 1) {
                try incrementUsage(usage, @enumFromInt(hg.extra.items[extra_index]));
            }
        },
        .block, .block_inline => {},
        .branch_single => {
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.BranchSingle);

            try incrementUsage(usage, data.condition);
        },
        .branch_double => {
            const pl = hg.insts.items(.data)[inst].pl_node.pl;
            const data = hg.extraData(pl, Hir.Inst.BranchDouble);

            try incrementUsage(usage, data.condition);
        },
        .loop => {},
        .loop_break => {},
        .ret_implicit, .yield_implicit => {
            // unary operand
            var op = hg.insts.slice().items(.data)[inst].un_tok.operand;
            try incrementUsage(usage, op);
        },
        .ret_node, .yield_node, .yield_inline => {
            // unary operand
            var op = hg.insts.slice().items(.data)[inst].un_node.operand;
            try incrementUsage(usage, op);
        },
        .alloca, .load_global => {},
        .dbg_value, .dbg_declare, .dbg_assign => {},
        .module => unreachable,
    }
}

fn incrementUsage(usage: *std.AutoHashMap(Hir.Index, u32), ref: Hir.Ref) !void {
    if (refToIndex(ref)) |index| {
        if (usage.get(index)) |count| {
            try usage.put(index, count + 1);
        } else {
            try usage.put(index, 1);
        }
    }
}
