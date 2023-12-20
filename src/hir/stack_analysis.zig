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
const Type = @import("type.zig").Type;
const Value = @import("../value.zig").Value;
const BlockEditor = @import("BlockEditor.zig");
const Allocator = std.mem.Allocator;

const StackAnalysis = struct {
    hg: *HirGen,
    arena: Allocator,
    allocas: std.ArrayListUnmanaged(Hir.Index),

    pub fn deinit(self: *StackAnalysis) void {
        self.allocas.deinit(self.arena);
    }

    fn analyze(self: *StackAnalysis, block: Hir.Index) !void {
        const hg = self.hg;
        const block_data = hg.get(block, .block);
        const slice = hg.block_slices.items[block_data.head];
        var b = try BlockEditor.clone(hg, slice);
        for (slice, 0..) |inst, i| {
            if (hg.insts.items(.tag)[inst] == .push) {
                const data = hg.get(inst, .push);
                // unlike pushes, alloca instructions are strongly typed
                const slot_type = try hg.resolveType(data.operand);
                const slot_type_index = try b.add(.ty, .{ .ty = slot_type });
                const ty = try Type.Pointer.init(hg.gpa, slot_type);
                const ty_index = try b.add(.ty, .{ .ty = ty });
                const node = hg.annot.items[inst].node;
                const alloca = try b.addAllocaUnlinked(ty_index, slot_type_index, node);
                // don't link this alloca in place, instead we want to
                // collect them at the top of the function
                try self.allocas.append(self.arena, alloca);
                // and make sure anything using the push handle
                // is moved over to the alloca
                try b.replaceAllUsesWith(inst, alloca);
                // replace the push in place with a store
                const store = try b.addUnlinked(.store, .{
                    .ptr = alloca,
                    .val = data.operand,
                    .node = node,
                });

                b.replaceInst(i, store);
            }

            try hg.explore(inst, analyze, .{self});
        }

        try BlockEditor.updateBlock(hg, &b, block);
    }
};

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
            if (hg.insts.items(.tag)[block_inst] == .constant) {
                const constant = hg.get(block_inst, .constant);
                const ty = try hg.resolveType(constant.ty);
                if (ty.kind() == .function) {
                    const payload = hg.pool.getValue(constant.val).function;
                    var analysis: StackAnalysis = .{
                        .hg = hg,
                        .arena = hg.arena,
                        .allocas = std.ArrayListUnmanaged(Hir.Index){},
                    };
                    try analysis.analyze(payload.body);

                    const data = hg.get(payload.body, .block);
                    const slice = hg.block_slices.items[data.head];
                    var b = try BlockEditor.clone(hg, slice);

                    // insert the allocas after the params
                    var i: u32 = 0;
                    while (hg.insts.items(.tag)[b.insts.items[i]] == .param) : (i += 1) {}
                    for (analysis.allocas.items) |alloca| {
                        try b.insertInst(i, alloca);
                        i += 1;
                    }

                    try BlockEditor.updateBlock(hg, &b, payload.body);
                }
            }
        }
    }
}
