// The type analysis pass is one of the most important (and largest)
// semantic analysis pass. It transforms the untyped "high level" HIR
// generated from the AST into concretely typed instructions.
// * global scope inline blocks (hirgen.untyped_decls) are moved into Values
// * user defined type constructs and aliases are evaluated and registered
// * all instructions are checked for type compatibility and validity
// * coerce instructions become extensions, truncations, bitcasts, or nops
// * generic arithmetic and comparison instructions are replaced with typed ones
// * function types (parameters and return types) are checked

const std = @import("std");
const Hir = @import("../Hir.zig");
const HirGen = @import("../HirGen.zig");
const BlockEditor = @import("BlockEditor.zig");
const Type = @import("../typing.zig").Type;
const Value = @import("../value.zig").Value;
const coercion = @import("coercion.zig");
const Ref = Hir.Ref;

const indexToRef = Hir.Inst.indexToRef;
const refToIndex = Hir.Inst.refToIndex;

pub fn executePass(hg: *HirGen, module_index: Hir.Index) !void {
    const module_pl = hg.insts.items(.data)[module_index].pl_node.pl;
    const module_data = hg.extraData(module_pl, Hir.Inst.Module);

    var inner_blocks = std.ArrayListUnmanaged(u32){};
    defer inner_blocks.deinit(hg.arena);

    var extra_index: u32 = 0;
    while (extra_index < module_data.len * 2) : (extra_index += 2) {
        const base = module_pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        // load the inline block
        const block_inline_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const block_inline = hg.extraData(block_inline_pl, Hir.Inst.Block);

        var editor = try BlockEditor.init(hg);

        const slice = hg.block_slices.items[block_inline.head];
        for (slice) |block_inst| {
            try processInst(&editor, block_inst, &inner_blocks);
            // TODO: we need a better solution to this
            try BlockEditor.updateBlock(hg, &editor, inst);
        }

        try BlockEditor.updateBlock(hg, &editor, inst);
    }

    for (inner_blocks.items) |inst| {
        const block_inline_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const block_inline = hg.extraData(block_inline_pl, Hir.Inst.Block);

        var editor = try BlockEditor.init(hg);

        const slice = hg.block_slices.items[block_inline.head];
        for (slice) |block_inst| {
            try processInst(&editor, block_inst, &inner_blocks);
            // TODO: we need a better solution to this
            try BlockEditor.updateBlock(hg, &editor, inst);
        }

        try BlockEditor.updateBlock(hg, &editor, inst);
    }
}

fn processInst(b: *BlockEditor, inst: Hir.Index, inner_blocks: *std.ArrayListUnmanaged(Hir.Index)) !void {
    switch (b.hg.insts.items(.tag)[inst]) {
        .fn_decl => try fnDecl(b, inst, inner_blocks),
        .int => _ = try integer(b, inst),
        .float => _ = try float(b, inst),
        .coerce => try coerce(b, inst),
        else => try b.linkInst(inst),
    }
}

fn initFunctionType(hg: *HirGen, function_inst: Hir.Index) !Type {
    const pl = hg.insts.items(.data)[function_inst].pl_node.pl;
    const fn_decl = hg.extraData(pl, Hir.Inst.FnDecl);

    const param_types = try hg.gpa.alloc(Type, fn_decl.params_end - fn_decl.params_start);
    const params = hg.extra.items[fn_decl.params_start..fn_decl.params_end];
    for (params, 0..) |inst, i| {
        const param_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const param_data = hg.extraData(param_pl, Hir.Inst.Param);
        param_types[i] = hg.resolveType(param_data.ty);
    }

    const return_type = hg.resolveType(fn_decl.return_type);
    return Type.Function.init(hg.gpa, return_type, param_types);
}

fn fnDecl(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    const hg = b.hg;
    const fn_type = try initFunctionType(hg, inst);

    const fn_data = hg.insts.items(.data)[inst].pl_node;
    const fn_decl = hg.extraData(fn_data.pl, Hir.Inst.FnDecl);

    const function = ptr: {
        const ptr = try hg.gpa.create(Hir.Inst.Function);
        ptr.return_type = fn_decl.return_type;
        ptr.body = fn_decl.body;
        ptr.params = try hg.gpa.alloc(Hir.Inst.Param, fn_decl.params_end - fn_decl.params_start);
        var extra_index: u32 = 0;
        while (extra_index < fn_decl.params_end - fn_decl.params_start) : (extra_index += 1) {
            const param_inst = hg.extra.items[fn_decl.params_start + extra_index];
            const param_pl = hg.insts.items(.data)[param_inst].pl_node.pl;
            const param_data = hg.extraData(param_pl, Hir.Inst.Param);
            ptr.params[extra_index] = param_data;
        }

        std.debug.print("{any}\n", .{ptr.params});
        break :ptr ptr;
    };

    try inner.append(hg.arena, function.body);

    const payload = try hg.gpa.create(Value.Payload.Function);
    payload.* = .{ .func = function };
    const fn_value = Value{ .payload = &payload.base };

    const constant = try b.addConstant(fn_type, fn_value, fn_data.node);
    try b.addRemap(inst, indexToRef(constant));
}

fn integer(b: *BlockEditor, inst: Hir.Index) !Hir.Index {
    const data = b.hg.insts.items(.data)[inst];
    // TODO: maybe try to store the node
    const constant = try b.addIntConstant(Type.initComptimeInt(false), data.int, undefined);
    try b.addRemap(inst, indexToRef(constant));

    return constant;
}

fn float(b: *BlockEditor, inst: Hir.Index) !Hir.Index {
    const data = b.hg.insts.items(.data)[inst];
    // TODO: maybe try to store the node
    const constant = try b.addFloatConstant(Type.initComptimeFloat(), data.float, undefined);
    try b.addRemap(inst, indexToRef(constant));

    return constant;
}

fn coerce(b: *BlockEditor, inst: Hir.Index) !void {
    const hg = b.hg;
    const data = hg.insts.items(.data)[inst];
    const coerce_data = hg.extraData(data.pl_node.pl, Hir.Inst.Coerce);

    const dest_ty = hg.resolveType(coerce_data.ty);
    const new_ref = try coercion.coerce(b, coerce_data.val, dest_ty);
    try b.addRemap(inst, new_ref);
}
