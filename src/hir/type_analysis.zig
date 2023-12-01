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
            BlockEditor.commitRemap(hg, &editor.remaps, block_inline.head);
        }

        try BlockEditor.updateBlock(hg, &editor, inst);
    }

    // for (inner_blocks.items) |inst| {
    while (inner_blocks.items.len > 0) {
        const inst = inner_blocks.pop();
        const block_inline_pl = hg.insts.items(.data)[inst].pl_node.pl;
        const block_inline = hg.extraData(block_inline_pl, Hir.Inst.Block);

        var editor = try BlockEditor.init(hg);

        const slice = hg.block_slices.items[block_inline.head];
        for (slice) |block_inst| {
            try processInst(&editor, block_inst, &inner_blocks);
            // TODO: we need a better solution to this
            BlockEditor.commitRemap(hg, &editor.remaps, block_inline.head);
        }

        try BlockEditor.updateBlock(hg, &editor, inst);
    }
}

fn processInst(b: *BlockEditor, inst: Hir.Index, inner_blocks: *std.ArrayListUnmanaged(Hir.Index)) !void {
    const hg = b.hg;
    switch (hg.insts.items(.tag)[inst]) {
        .fn_decl => try fnDecl(b, inst, inner_blocks),
        .int => _ = try integer(b, inst),
        .float => _ = try float(b, inst),
        .coerce => try coerce(b, inst),
        .cmp_eq, .cmp_ne, .cmp_gt, .cmp_ge, .cmp_lt, .cmp_le => try binaryCmp(b, inst),
        .branch_single => try branchSingle(b, inst, inner_blocks),
        .branch_double => try branchDouble(b, inst, inner_blocks),
        .loop => try loop(b, inst, inner_blocks),
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

// fn binaryArithOp(b: *BlockEditor, inst: Hir.Index) !void {
//     const hg = b.hg;
//     const data = b.hir.insts.items(.data)[inst];
//     const binary = b.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
//     const lref = try hg.resolveRef(b, binary.lref);
//     const rref = try hg.resolveRef(b, binary.rref);
//     const lty = try hg.resolveType(b, lref);
//     const rty = try hg.resolveType(b, rref);

//     if (lty.kind() == .comptime_uint or lty.kind() == .comptime_sint) {
//         if (rty.kind() == .comptime_uint or rty.kind() == .comptime_sint) {
//             return b.analyzeComptimeArithmetic(b, inst);
//         }
//     }

//     const dest_ty = try coercion.binaryCoerceTo(lty, rty);
//     const lval = try coercion.coerce(b, lref, dest_ty);
//     const rval = try coercion.coerce(b, rref, dest_ty);

//     try b.addInst(.{
//         .tag = switch (b.hir.insts.items(.tag)[inst]) {
//             .add => .add,
//             .sub => .sub,
//             .mul => .mul,
//             .div => .div,
//             .mod => .mod,
//             else => return error.NotImplemented,
//         },
//         .data = .{ .bin_op = .{ .lref = lval, .rref = rval } },
//     });
// }

// this function performs simple constant folding of compile time arithmetic.
// this is necessary because constants produce `comptime_int` and `comptime_float`
// values until explicitly coerced into fixed width types. arithmetic performed inline
// before this coercion happens must be performed at compile time.
//
// this is performed by emulating the bitwise computation of each operation
// on a u64 value and reporting overflows. eventually, bigints will be supported
// fn analyzeComptimeArithmetic(b: *BlockEditor, inst: Hir.Index) !Hir.Ref {
//     const hg = b.hg;
//     const data = hg.insts.items(.data)[inst];
//     const binary = hg.extraData(data.pl_node.pl, Hir.Inst.Binary);
//     const lref = try b.resolveRef(b, binary.lref);
//     const rref = try b.resolveRef(b, binary.rref);
//     const lty = try b.resolveType(b, lref);
//     const rty = try b.resolveType(b, rref);

//     const lval = b.refToInt(lref);
//     const rval = b.refToInt(rref);
//     const lsign = lty.intSign();
//     const rsign = rty.intSign();

//     // if both values are unsigned, we perform unsigned operations
//     // otherwise signed. TODO: revise this?
//     const result = switch (hg.insts.items(.tag)[inst]) {
//         .add => if (!lsign and !rsign) try alu.uadd(lval, rval) else try alu.sadd(lval, rval),
//         .sub => try alu.sadd(lval, alu.negate(rval)),
//         .mul => if (!lsign and !rsign) try alu.umul(lval, rval) else try alu.smul(lval, rval),
//         .div => if (!lsign and !rsign) try alu.udiv(lval, rval) else try alu.sdiv(lval, rval),
//         .mod => return error.NotImplemented,
//         .lsl => alu.lsl(lval, rval),
//         .lsr => try alu.lsr(lval, rval),
//         .asl => if (rsign) try alu.asr(lval, alu.negate(rval)) else try alu.asl(lval, rval),
//         .asr => if (rsign) try alu.asl(lval, alu.negate(rval)) else try alu.asr(lval, rval),
//         else => unreachable,
//     };

//     return switch (hg.insts.items(.tag)[inst]) {
//         .add, .mul, .div => if (!lsign and !rsign) try addUnsignedValue(b, result) else try addSignedValue(b, result),
//         .sub => try addSignedValue(b, result),
//         .lsl, .lsr => try addUnsignedValue(b, result),
//         .asl, .asr => if (!lsign) try addUnsignedValue(b, result) else try addSignedValue(b, result),
//         .mod => return error.NotImplemented,
//         else => unreachable,
//     };
// }

fn binaryCmp(b: *BlockEditor, inst: Hir.Index) !void {
    const hg = b.hg;
    const data = hg.insts.items(.data)[inst];
    const binary = hg.extraData(data.pl_node.pl, Hir.Inst.Binary);

    const lty = hg.resolveType(binary.lref);
    const rty = hg.resolveType(binary.rref);
    const lkind = lty.kind();
    const rkind = rty.kind();

    switch (lkind) {
        .uint => {
            // uints can be be compared to other uints (of any size) and comptime_uints
            // if both types are sized uints, the smaller one will be zero extended to
            // the larger size, and then an unsigned comparison will be used
            // if the other type is a comptime_uint, it will be coerced to a concrete
            // type of the right size
            switch (rkind) {
                .uint => {
                    var cmp_lref = binary.lref;
                    var cmp_rref = binary.rref;

                    if (lty.basic.width < rty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initInt(rty.basic.width, false));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_lref,
                            .ty = dest_ty,
                        });
                        cmp_lref = indexToRef(try b.addInst(.{
                            .tag = .zext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    } else if (rty.basic.width < lty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initInt(lty.basic.width, false));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_rref,
                            .ty = dest_ty,
                        });
                        cmp_rref = indexToRef(try b.addInst(.{
                            .tag = .zext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    }

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_ugt,
                        .cmp_ge => .icmp_uge,
                        .cmp_lt => .icmp_ult,
                        .cmp_le => .icmp_ule,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .comptime_uint => {
                    const rval: u64 = hg.refToInt(binary.rref);
                    if (rval > lty.maxInt()) {
                        return error.Truncated;
                    }
                    const cmp_lref = binary.lref;
                    const cmp_rref = indexToRef(try b.addIntConstant(lty, rval, data.pl_node.node));

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_ugt,
                        .cmp_ge => .icmp_uge,
                        .cmp_lt => .icmp_ult,
                        .cmp_le => .icmp_ule,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .sint => return error.Truncated, // TODO: should emit error
                else => unreachable, // TODO: should emit error
            }
        },
        .sint => {
            // sints can be be compared to other sints (of any size) and comptime_sints
            // if both types are sized sints, the smaller one will be sign extended to
            // the larger size, and then an signed comparison will be used
            // if the other type is a comptime_sint, it will be coerced to a concrete
            // type of the right size
            switch (rkind) {
                .sint => {
                    var cmp_lref = binary.lref;
                    var cmp_rref = binary.rref;

                    if (lty.basic.width < rty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initInt(rty.basic.width, true));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_lref,
                            .ty = dest_ty,
                        });
                        cmp_lref = indexToRef(try b.addInst(.{
                            .tag = .sext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    } else if (rty.basic.width < lty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initInt(lty.basic.width, true));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_rref,
                            .ty = dest_ty,
                        });
                        cmp_rref = indexToRef(try b.addInst(.{
                            .tag = .sext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    }

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_sgt,
                        .cmp_ge => .icmp_sge,
                        .cmp_lt => .icmp_slt,
                        .cmp_le => .icmp_sle,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .comptime_sint => {
                    const rval: i64 = @bitCast(hg.refToInt(binary.rref));
                    if (rval < lty.minInt() or rval > lty.maxInt()) {
                        return error.Truncated;
                    }
                    const cmp_lref = binary.lref;
                    const cmp_rref = indexToRef(try b.addIntConstant(lty, @bitCast(rval), data.pl_node.node));

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_sgt,
                        .cmp_ge => .icmp_sge,
                        .cmp_lt => .icmp_slt,
                        .cmp_le => .icmp_sle,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .uint => return error.Truncated, // TODO: should emit error
                else => unreachable, // TODO: should emit error
            }
        },
        .comptime_uint => {
            // comptime_uints can be be compared to uints (of any size) and comptime_uints
            // if the right side is a sized unit, the left side will be coerced
            // if the right side is a comptime_uint, the comparison will be folded
            // and a constant bool emitted
            switch (rkind) {
                .uint => {
                    const lval: u64 = hg.refToInt(binary.lref);
                    if (lval > rty.maxInt()) {
                        return error.Truncated;
                    }
                    const cmp_lref = indexToRef(try b.addIntConstant(rty, lval, data.pl_node.node));
                    const cmp_rref = binary.rref;

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_ugt,
                        .cmp_ge => .icmp_uge,
                        .cmp_lt => .icmp_ult,
                        .cmp_le => .icmp_ule,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .comptime_uint => {
                    const lval: u64 = hg.refToInt(binary.lref);
                    const rval: u64 = hg.refToInt(binary.rref);

                    const result: bool = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => lval == rval,
                        .cmp_ne => lval != rval,
                        .cmp_gt => lval > rval,
                        .cmp_ge => lval >= rval,
                        .cmp_lt => lval < rval,
                        .cmp_le => lval <= rval,
                        else => unreachable,
                    };

                    const result_int: u64 = if (result) 1 else 0;
                    const dest_ty = Type.initInt(1, false);
                    const result_inst = try b.addIntConstant(dest_ty, result_int, data.pl_node.node);

                    try b.addRemap(inst, indexToRef(result_inst));
                },
                .sint => return error.Truncated, // TODO: should emit error
                else => unreachable, // TODO: should emit error
            }
        },
        .float => {
            // floats can be be compared to other floats (of any size) and comptime_floats
            // if both types are sized floats, the smaller one will be extended to
            // the larger size, and then an floating point comparison will be used
            // if the other type is a comptime_float, it will be coerced to a concrete
            // type of the right size
            switch (rkind) {
                .float => {
                    var cmp_lref = binary.lref;
                    var cmp_rref = binary.rref;

                    if (lty.basic.width < rty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initFloat(rty.basic.width));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_lref,
                            .ty = dest_ty,
                        });
                        cmp_lref = indexToRef(try b.addInst(.{
                            .tag = .fpext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    } else if (rty.basic.width < lty.basic.width) {
                        const dest_ty = try b.typeToRef(Type.initFloat(lty.basic.width));
                        const pl = try b.hg.addExtra(Hir.Inst.Extend{
                            .val = cmp_rref,
                            .ty = dest_ty,
                        });
                        cmp_rref = indexToRef(try b.addInst(.{
                            .tag = .fpext,
                            .data = .{ .pl_node = .{ .pl = pl, .node = data.pl_node.node } },
                        }));
                    }

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .comptime_float => {
                    const rval: f64 = hg.refToFloat(binary.rref);
                    const cmp_lref = binary.lref;
                    const cmp_rref = indexToRef(try b.addFloatConstant(lty, rval, data.pl_node.node));

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                else => unreachable, // TODO: should emit error
            }
        },
        .comptime_float => {
            // comptime_floats can be be compared to floats (of any size) and comptime_floats
            // if the right side is a sized float, the left side will be coerced
            // if the right side is a comptime_float, the comparison will be folded
            // and a constant bool emitted
            switch (rkind) {
                .float => {
                    const lval: f64 = hg.refToFloat(binary.lref);
                    const cmp_lref = indexToRef(try b.addFloatConstant(rty, lval, data.pl_node.node));
                    const cmp_rref = binary.rref;

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(cmp_lref, cmp_rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_cmp));
                },
                .comptime_float => {
                    const lval: f64 = hg.refToFloat(binary.lref);
                    const rval: f64 = hg.refToFloat(binary.rref);

                    const result: bool = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => lval == rval,
                        .cmp_ne => lval != rval,
                        .cmp_gt => lval > rval,
                        .cmp_ge => lval >= rval,
                        .cmp_lt => lval < rval,
                        .cmp_le => lval <= rval,
                        else => unreachable,
                    };

                    const result_int: u64 = if (result) 1 else 0;
                    const dest_ty = Type.initInt(1, false);
                    const result_inst = try b.addIntConstant(dest_ty, result_int, data.pl_node.node);

                    try b.addRemap(inst, indexToRef(result_inst));
                },
                else => unreachable, // TODO: should emit error
            }
        },
        else => unreachable,
    }
}

fn branchSingle(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    const hg = b.hg;
    const pl = hg.insts.items(.data)[inst].pl_node.pl;
    const branch_single = hg.extraData(pl, Hir.Inst.BranchSingle);

    try inner.append(hg.arena, branch_single.exec_true);
    try b.linkInst(inst);
}

fn branchDouble(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    const hg = b.hg;
    const pl = hg.insts.items(.data)[inst].pl_node.pl;
    const branch_double = hg.extraData(pl, Hir.Inst.BranchDouble);

    try inner.append(hg.arena, branch_double.exec_true);
    try inner.append(hg.arena, branch_double.exec_false);
    try b.linkInst(inst);
}

fn loop(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    const hg = b.hg;
    const pl = hg.insts.items(.data)[inst].pl_node.pl;
    const loop_data = hg.extraData(pl, Hir.Inst.Loop);

    try inner.append(hg.arena, loop_data.condition);
    try inner.append(hg.arena, loop_data.body);
    try b.linkInst(inst);
}
