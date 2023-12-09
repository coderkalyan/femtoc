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
const Type = @import("type.zig").Type;
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
        .add, .sub, .mul, .div, .mod => try binaryArithOp(b, inst),
        .cmp_eq, .cmp_ne, .cmp_gt, .cmp_ge, .cmp_lt, .cmp_le => try binaryCmp(b, inst),
        .branch_single => try branchSingle(b, inst, inner_blocks),
        .branch_double => try branchDouble(b, inst, inner_blocks),
        .loop => try loop(b, inst, inner_blocks),
        .block => try block(b, inst, inner_blocks),
        .call => try call(b, inst),
        else => try b.linkInst(inst),
    }
}

fn fnDecl(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    const hg = b.hg;
    const fn_data = hg.insts.items(.data)[inst].pl_node;
    const fn_decl = hg.extraData(fn_data.pl, Hir.Inst.FnDecl);

    // analyze the parameters, generating their types and a list of parameter instructions
    const num_params = fn_decl.params_end - fn_decl.params_start;
    const param_types = try hg.gpa.alloc(Type, num_params);
    const params = try hg.gpa.alloc(Hir.Index, num_params);
    std.mem.copy(Hir.Index, params, hg.extra.items[fn_decl.params_start..fn_decl.params_end]);
    for (params, 0..) |param, i| {
        const param_pl = hg.insts.items(.data)[param].pl_node.pl;
        const param_data = hg.extraData(param_pl, Hir.Inst.Param);
        param_types[i] = hg.resolveType(param_data.ty);
    }

    // build the type for this function from the parameter and return types
    const return_type = hg.resolveType(fn_decl.return_type);
    const fn_type_inner = try hg.gpa.create(Type.Function);
    fn_type_inner.* = .{ .param_types = param_types, .return_type = return_type };
    const fn_type: Type = .{ .extended = &fn_type_inner.base };

    const function = try hg.gpa.create(Value.Function);
    function.* = .{ .params = params, .body = fn_decl.body };
    const fn_value: Value = .{ .extended = &function.base };

    try inner.append(hg.arena, function.body);

    const constant = try b.addConstant(fn_type, fn_value, fn_data.node);
    try b.addRemap(inst, indexToRef(constant));
}

fn integer(b: *BlockEditor, inst: Hir.Index) !Hir.Index {
    const data = b.hg.insts.items(.data)[inst];
    // TODO: maybe try to store the node
    const constant = try b.addIntConstant(Type.Common.comptime_uint, data.int, undefined);
    try b.addRemap(inst, indexToRef(constant));

    return constant;
}

fn float(b: *BlockEditor, inst: Hir.Index) !Hir.Index {
    const data = b.hg.insts.items(.data)[inst];
    // TODO: maybe try to store the node
    const constant = try b.addFloatConstant(Type.Common.comptime_float, data.float, undefined);
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

fn binaryArithOp(b: *BlockEditor, inst: Hir.Index) !void {
    const hg = b.hg;
    const tag = hg.insts.items(.tag)[inst];
    const data = hg.insts.items(.data)[inst];
    const binary = hg.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const op_token = hg.tree.mainToken(data.pl_node.node);

    const lty = hg.resolveType(binary.lref);
    const rty = hg.resolveType(binary.rref);
    const lkind = lty.kind();
    const rkind = rty.kind();

    // pending a discussion on arithmetic overflow semantics,
    // femto only allows addition between variables of the same sign
    // comptime literals coerce to the fixed type of the other value
    // if both are comptime literals, the arithmetic is evaluated at
    // compile time (constant folding)
    switch (lkind) {
        .uint => {
            switch (rkind) {
                .uint => {
                    const bits = @max(lty.basic.width, rty.basic.width);
                    const dest_ty = Type.initLiteral(.uint, bits);
                    const lref = try coercion.coerce(b, binary.lref, dest_ty);
                    const rref = try coercion.coerce(b, binary.rref, dest_ty);
                    const new_arith = try b.addBinary(lref, rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_arith));
                },
                .sint => {
                    try hg.errors.append(hg.gpa, .{
                        .tag = .binary_diffsign,
                        .token = op_token,
                    });
                    return error.HandledUserError;
                },
                .comptime_uint, .comptime_sint => {
                    const lref = binary.lref;
                    const rref = try coercion.coerce(b, binary.rref, lty);
                    const new_arith = try b.addBinary(lref, rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_arith));
                },
                else => unreachable, // TODO: should emit error
            }
        },
        .sint => {
            switch (rkind) {
                .uint => {
                    try hg.errors.append(hg.gpa, .{
                        .tag = .binary_diffsign,
                        .token = op_token,
                    });
                    return error.HandledUserError;
                },
                .sint => {
                    const bits = @max(lty.basic.width, rty.basic.width);
                    const dest_ty = Type.initLiteral(.sint, bits);
                    const lref = try coercion.coerce(b, binary.lref, dest_ty);
                    const rref = try coercion.coerce(b, binary.rref, dest_ty);
                    const new_arith = try b.addBinary(lref, rref, tag, data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_arith));
                },
                .comptime_uint, .comptime_sint => {
                    const lref = binary.lref;
                    const rref = try coercion.coerce(b, binary.rref, lty);

                    const new_arith = try b.addBinary(lref, rref, hg.insts.items(.tag)[inst], data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_arith));
                },
                else => unreachable, // TODO: should emit error
            }
        },
        .comptime_uint, .comptime_sint => {
            switch (rkind) {
                .uint, .sint => {
                    const lref = try coercion.coerce(b, binary.lref, rty);
                    const rref = binary.rref;

                    const new_arith = try b.addBinary(lref, rref, hg.insts.items(.tag)[inst], data.pl_node.node);
                    try b.addRemap(inst, indexToRef(new_arith));
                },
                .comptime_uint, .comptime_sint => {
                    unreachable; // TODO: constant folding
                },
                else => unreachable, // TODO: should emit error
            }
        },
        else => unreachable,
    }

    // if (lty.kind() == .comptime_uint or lty.kind() == .comptime_sint) {
    //     if (rty.kind() == .comptime_uint or rty.kind() == .comptime_sint) {
    //         return b.analyzeComptimeArithmetic(b, inst);
    //     }
    // }
}

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
                        cmp_lref = try coercion.coerce(b, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coercion.coerce(b, cmp_rref, lty);
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
                .sint => return error.Truncated, // TODO: should emit error
                .comptime_uint, .comptime_sint => {
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coercion.coerce(b, binary.rref, lty);

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
                .uint => return error.Truncated, // TODO: should emit error
                .sint => {
                    var cmp_lref = binary.lref;
                    var cmp_rref = binary.rref;

                    if (lty.basic.width < rty.basic.width) {
                        cmp_lref = try coercion.coerce(b, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coercion.coerce(b, cmp_rref, lty);
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
                .comptime_uint, .comptime_sint => {
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coercion.coerce(b, binary.rref, lty);

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
                    const cmp_lref = try coercion.coerce(b, binary.lref, rty);
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
                    const dest_ty = Type.Common.u1_type;
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
                        cmp_lref = try coercion.coerce(b, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coercion.coerce(b, cmp_rref, lty);
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
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coercion.coerce(b, binary.rref, lty);

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
                    const cmp_lref = try coercion.coerce(b, binary.lref, rty);
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
                    const dest_ty = Type.Common.u1_type;
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

fn block(b: *BlockEditor, inst: Hir.Index, inner: *std.ArrayListUnmanaged(u32)) !void {
    try inner.append(b.hg.arena, inst);
    try b.linkInst(inst);
}

fn call(b: *BlockEditor, inst: Hir.Index) !void {
    const hg = b.hg;
    const data = hg.insts.items(.data)[inst].pl_node;
    const call_data = hg.extraData(data.pl, Hir.Inst.Call);
    const addr_token = hg.tree.mainToken(data.node);

    // TODO: should we try to coerce this? not sure yet
    const addr_type = hg.resolveType(call_data.addr);
    if (addr_type.kind() != .function) {
        // you can only call a function
        try hg.errors.append(hg.gpa, .{
            .tag = .call_nonfunc,
            .token = addr_token,
        });
        return error.HandledUserError;
    }

    const func_type = addr_type.extended.cast(Type.Function).?;
    if (func_type.param_types.len != call_data.args_len) {
        // incorrect number of arguments to function call
        try hg.errors.append(hg.gpa, .{
            .tag = .call_argcount,
            .token = addr_token, // TODO: maybe something better we can use here?
        });
        return error.HandledUserError;
    }

    const src_args = hg.extra.items[data.pl + 2 .. data.pl + 2 + call_data.args_len];
    const dest_args = try hg.arena.alloc(u32, call_data.args_len);
    defer hg.arena.free(dest_args);
    for (src_args, func_type.param_types, 0..) |src_arg, param_type, i| {
        // identify the type of the ith parameter of the function type
        // and coerce the src_arg to that type
        const dest_arg = try coercion.coerce(b, @enumFromInt(src_arg), param_type);
        dest_args[i] = @intFromEnum(dest_arg);
    }

    const new_call = try b.addCall(call_data.addr, dest_args, data.node);
    try b.addRemap(inst, indexToRef(new_call));
}
