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
// const coercion = @import("coercion.zig");
const Coercion = @import("Coercion.zig");

const BlockAnalysis = struct {
    hg: *HirGen,
    // copy of block we are analyzing, used to update references
    src_block: *BlockEditor,
    // where all new instructions go (replaced or copied)
    b: *BlockEditor,
    block_index: Hir.Index,
};

const Error = error{
    OutOfMemory,
    InvalidCoercion,
    NotImplemented,
    Truncated,
    HandledUserError,
};

pub fn executePass(hg: *HirGen, module_index: Hir.Index) !void {
    const module = hg.get(module_index, .module);

    var inner_blocks = std.ArrayListUnmanaged(u32){};
    defer inner_blocks.deinit(hg.arena);

    var extra_index: u32 = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module.pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];
        try analyze(hg, false, inst);
    }

    // second pass
    extra_index = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module.pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        const block_data = hg.get(inst, .block);
        const slice = hg.block_slices.items[block_data.head];
        for (slice) |block_inst| {
            try hg.explore(block_inst, analyze, .{ hg, true });
        }
    }
}

fn analyze(hg: *HirGen, comptime explore: bool, block: Hir.Index) Error!void {
    const block_data = hg.get(block, .block);
    const slice = hg.block_slices.items[block_data.head];
    var src_block = try BlockEditor.clone(hg, slice);
    var b = try BlockEditor.init(hg);
    var analysis = BlockAnalysis{
        .hg = hg,
        .src_block = &src_block,
        .b = &b,
        .block_index = block,
    };

    for (slice, 0..) |inst, i| {
        _ = i;
        switch (hg.insts.items(.tag)[inst]) {
            .int => try integer(&analysis, inst),
            .float => try float(&analysis, inst),
            .coerce => try coerce(&analysis, inst),
            .fn_decl => try fnDecl(&analysis, inst),
            .call => try call(&analysis, inst),
            .branch_single,
            .branch_double,
            .loop,
            .block,
            .yield_node,
            .yield_implicit,
            .yield_inline,
            .global_mut,
            // just re-link these
            => try b.linkInst(inst),
            .pointer_ty => try pointerTy(&analysis, inst),
            .cmp_eq,
            .cmp_ne,
            .cmp_gt,
            .cmp_ge,
            .cmp_lt,
            .cmp_le,
            => try binaryCmp(&analysis, inst),
            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            => |tag| try binaryArithOp(&analysis, inst, tag),
            // else => {},
            // TODO: remove else clause and switch explicitly
            // to avoid copying bad instructions
            else => try b.linkInst(inst),
        }

        if (explore) try hg.explore(inst, analyze, .{ hg, explore });
    }

    try BlockEditor.updateBlock(hg, analysis.b, block);
}

fn fnDecl(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const fn_decl = hg.get(inst, .fn_decl);

    // analyze the parameters, generating their types and a list of parameter instructions
    const num_params = fn_decl.params_end - fn_decl.params_start;
    const param_types = try hg.gpa.alloc(Type, num_params);
    const params = try hg.gpa.alloc(Hir.Index, num_params);
    std.mem.copy(Hir.Index, params, hg.extra.items[fn_decl.params_start..fn_decl.params_end]);
    for (params, 0..) |param, i| {
        const param_pl = hg.insts.items(.data)[param].pl_node.pl;
        const param_data = hg.extraData(param_pl, Hir.Inst.Param);
        param_types[i] = try hg.resolveType(param_data.ty);
    }

    // build the type for this function from the parameter and return types
    const return_type = try hg.resolveType(fn_decl.return_type);
    const fn_type_inner = try hg.gpa.create(Type.Function);
    fn_type_inner.* = .{ .param_types = param_types, .return_type = return_type };
    const fn_type: Type = .{ .extended = &fn_type_inner.base };

    const function = try hg.gpa.create(Value.Function);
    function.* = .{ .params = params, .body = fn_decl.body };
    const fn_value: Value = .{ .extended = &function.base };

    const constant = try b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = fn_type }),
        .val = try b.addValue(fn_value),
        .node = fn_decl.node,
    });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn integer(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .int);
    const value = try b.addIntValue(Type.Common.comptime_uint, data.int);
    const ty = try b.add(.ty, .{ .ty = Type.Common.comptime_uint });
    // TODO: maybe try to store the node
    const constant = try b.add(.constant, .{ .ty = ty, .val = value, .node = undefined });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn float(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .float);
    const value = try b.addFloatValue(data.float);
    const ty = try b.add(.ty, .{ .ty = Type.Common.comptime_float });
    // TODO: maybe try to store the node
    const constant = try b.add(.constant, .{ .ty = ty, .val = value, .node = undefined });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn coerce(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .coerce);
    const src_type = try hg.resolveType(data.val);
    const dest_type = try hg.resolveType(data.ty);
    if (src_type.eql(dest_type)) {
        try analysis.src_block.replaceAllUsesWith(inst, data.val);
        return;
    }

    var coercion = Coercion{
        .src_block = analysis.src_block,
        .b = b,
        .gpa = b.hg.gpa,
        .coerce_inst = inst,
        .src = data.val,
        .src_type = src_type,
        .dest_type = dest_type,
    };
    _ = try coercion.coerce();
}

pub fn coerceInnerImplicit(analysis: *BlockAnalysis, src: Hir.Index, dest_type: Type) !Hir.Index {
    const hg = analysis.hg;
    const b = analysis.b;
    const src_type = try hg.resolveType(src);
    if (src_type.eql(dest_type)) return src;

    var coercion = Coercion{
        .src_block = analysis.src_block,
        .b = b,
        .gpa = b.hg.gpa,
        .coerce_inst = null,
        .src = src,
        .src_type = src_type,
        .dest_type = dest_type,
    };
    return coercion.coerce();
}

fn binaryArithOp(analysis: *BlockAnalysis, inst: Hir.Index, comptime tag: Hir.Inst.Tag) !void {
    const b = analysis.b;
    const hg = analysis.hg;
    const binary = hg.get(inst, tag);
    const op_token = hg.tree.mainToken(binary.node);

    const lty = try hg.resolveType(binary.lref);
    const rty = try hg.resolveType(binary.rref);
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
                    const lref = try coerceInnerImplicit(analysis, binary.lref, dest_ty);
                    const rref = try coerceInnerImplicit(analysis, binary.rref, dest_ty);
                    const new_arith = try b.addBinary(tag, lref, rref, binary.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_arith);
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
                    const rref = try coerceInnerImplicit(analysis, binary.rref, lty);
                    const new_arith = try b.addBinary(tag, lref, rref, binary.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_arith);
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
                    const lref = try coerceInnerImplicit(analysis, binary.lref, dest_ty);
                    const rref = try coerceInnerImplicit(analysis, binary.rref, dest_ty);
                    const new_arith = try b.addBinary(tag, lref, rref, binary.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_arith);
                },
                .comptime_uint, .comptime_sint => {
                    const lref = binary.lref;
                    const rref = try coerceInnerImplicit(analysis, binary.rref, lty);

                    const new_arith = try b.addBinary(tag, lref, rref, binary.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_arith);
                },
                else => unreachable, // TODO: should emit error
            }
        },
        .comptime_uint, .comptime_sint => {
            switch (rkind) {
                .uint, .sint => {
                    const lref = try coerceInnerImplicit(analysis, binary.lref, rty);
                    const rref = binary.rref;

                    const new_arith = try b.addBinary(tag, lref, rref, binary.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_arith);
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

fn binaryCmp(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const b = analysis.b;
    const hg = analysis.hg;
    const data = hg.insts.items(.data)[inst];
    const binary = hg.extraData(data.pl_node.pl, Hir.Inst.Binary);

    const lty = try hg.resolveType(binary.lref);
    const rty = try hg.resolveType(binary.rref);
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
                        cmp_lref = try coerceInnerImplicit(analysis, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coerceInnerImplicit(analysis, cmp_rref, lty);
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
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
                },
                .sint => return error.Truncated, // TODO: should emit error
                .comptime_uint, .comptime_sint => {
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coerceInnerImplicit(analysis, binary.rref, lty);

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_ugt,
                        .cmp_ge => .icmp_uge,
                        .cmp_lt => .icmp_ult,
                        .cmp_le => .icmp_ule,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
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
                        cmp_lref = try coerceInnerImplicit(analysis, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coerceInnerImplicit(analysis, cmp_rref, lty);
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
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
                },
                .comptime_uint, .comptime_sint => {
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coerceInnerImplicit(analysis, binary.rref, lty);

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq => .icmp_eq,
                        .cmp_ne => .icmp_ne,
                        .cmp_gt => .icmp_sgt,
                        .cmp_ge => .icmp_sge,
                        .cmp_lt => .icmp_slt,
                        .cmp_le => .icmp_sle,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
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
                    const cmp_lref = try coerceInnerImplicit(analysis, binary.lref, rty);
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
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
                },
                .comptime_uint => {
                    const lval: u64 = hg.instToInt(binary.lref);
                    const rval: u64 = hg.instToInt(binary.rref);

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
                    try analysis.src_block.replaceAllUsesWith(inst, result_inst);
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
                        cmp_lref = try coerceInnerImplicit(analysis, cmp_lref, rty);
                    } else if (rty.basic.width < lty.basic.width) {
                        cmp_rref = try coerceInnerImplicit(analysis, cmp_rref, lty);
                    }

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
                },
                .comptime_float => {
                    const cmp_lref = binary.lref;
                    const cmp_rref = try coerceInnerImplicit(analysis, binary.rref, lty);

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
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
                    const cmp_lref = try coerceInnerImplicit(analysis, binary.lref, rty);
                    const cmp_rref = binary.rref;

                    const tag: Hir.Inst.Tag = switch (hg.insts.items(.tag)[inst]) {
                        .cmp_eq, .cmp_ne => unreachable, // TODO: generate float comparison error
                        .cmp_gt => .fcmp_gt,
                        .cmp_ge => .fcmp_ge,
                        .cmp_lt => .fcmp_lt,
                        .cmp_le => .fcmp_le,
                        else => unreachable,
                    };
                    const new_cmp = try b.addBinary(tag, cmp_lref, cmp_rref, data.pl_node.node);
                    try analysis.src_block.replaceAllUsesWith(inst, new_cmp);
                },
                .comptime_float => {
                    const lval: f64 = hg.instToFloat(binary.lref);
                    const rval: f64 = hg.instToFloat(binary.rref);

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
                    try analysis.src_block.replaceAllUsesWith(inst, result_inst);
                },
                else => unreachable, // TODO: should emit error
            }
        },
        else => unreachable,
    }
}

fn call(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const call_data = hg.get(inst, .call);
    const addr_token = hg.tree.mainToken(call_data.node);

    // TODO: should we try to coerce this? not sure yet
    const addr_type = try hg.resolveType(call_data.ptr);
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

    const src_args = hg.extra.items[call_data.pl + 2 .. call_data.pl + 2 + call_data.args_len];
    for (src_args, func_type.param_types, 0..) |src_arg, param_type, i| {
        // identify the type of the ith parameter of the function type
        // and coerce the src_arg to that type
        _ = i;
        // TODO: get rid of this
        _ = try coerceInnerImplicit(analysis, src_arg, param_type);
    }

    // const dest_args = try hg.arena.alloc(u32, call_data.args_len);
    const updated_data = hg.get(inst, .call);
    const dest_args = hg.extra.items[updated_data.pl + 2 .. updated_data.pl + 2 + updated_data.args_len];
    // defer hg.arena.free(dest_args);

    const new_call = try b.addCall(call_data.ptr, dest_args, call_data.node);
    try analysis.src_block.replaceAllUsesWith(inst, new_call);
}

fn pointerTy(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.insts.items(.data)[inst].un_node;
    const pointee = try hg.resolveType(data.operand);

    const inner = try hg.gpa.create(Type.Pointer);
    inner.* = .{ .pointee = pointee };
    const pointer: Type = .{ .extended = &inner.base };

    const new_type = try b.add(.ty, .{ .ty = pointer });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}
