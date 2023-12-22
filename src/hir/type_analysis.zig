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
    return_type: ?Type,
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
        try analyze(hg, false, null, inst);
    }

    // second pass
    extra_index = 0;
    while (extra_index < module.len * 2) : (extra_index += 2) {
        const base = module.pl + 1;
        const inst = hg.extra.items[base + extra_index + 1];

        // try hg.explore(inst, analyze, .{ hg, true, null });
        try analyze(hg, true, null, inst);
        // const block_data = hg.get(inst, .block);
        // const slice = hg.block_slices.items[block_data.head];
        // for (slice) |block_inst| {
        //     try hg.explore(block_inst, analyze, .{ hg, true, null });
        // }
    }
}

fn analyze(hg: *HirGen, comptime explore: bool, parent: ?*BlockAnalysis, block: Hir.Index) Error!void {
    const block_data = hg.get(block, .block);
    const slice = hg.block_slices.items[block_data.head];
    var src_block = try BlockEditor.clone(hg, slice);
    var b = try BlockEditor.init(hg);
    var analysis = BlockAnalysis{
        .hg = hg,
        .src_block = &src_block,
        .b = &b,
        .block_index = block,
        .return_type = if (parent) |p| p.return_type else null,
    };

    // processes each instruction, either transforming it or leaving it alone
    for (slice) |inst| {
        switch (hg.insts.items(.tag)[inst]) {
            // integer and float literals are turned into comptime_*** constants
            .int => try integer(&analysis, inst),
            .float => try float(&analysis, inst),
            // TODO: not sure why this isn't working, i guess we have bugs
            // with none as a general datatype
            // .none => try none(&analysis, inst),
            // coerces a source value to a destination type, replaced either by
            // a nop (already correct type), or safe casting logic (like extend)
            .coerce => try coerce(&analysis, inst),
            // for binary arithmetic, we make sure both operand types are the
            // same or coercable, coerce implicitly to the destination type,
            // and emit a typed signed/unsigned/float arithmetic op
            // if both operations are comptime known, the value is comptime
            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            => |tag| try binaryArithOp(&analysis, inst, tag),
            // similar to above, check, coerce, and emit typed instruction
            // comptime known values are emitted at comptime
            .cmp_eq,
            .cmp_ne,
            .cmp_gt,
            .cmp_ge,
            .cmp_lt,
            .cmp_le,
            => try binaryCmp(&analysis, inst),
            // make sure the correct number of arguments are passed in,
            // and we are calling a function and not some other junk
            .call => try call(&analysis, inst),
            .field_ref => try fieldRef(&analysis, inst),
            .field_val => try fieldVal(&analysis, inst),
            .index_ref => try indexRef(&analysis, inst),
            .index_val => try indexVal(&analysis, inst),
            .fn_decl => try fnDecl(&analysis, inst),
            .branch_single,
            .branch_double,
            .loop,
            .block,
            .yield_node,
            .yield_implicit,
            .yield_inline,
            .global_handle,
            .global_set_init,
            .global_set_mutable,
            .global_set_type,
            // just re-link these
            => try b.linkInst(inst),
            // builders for generating types from other types - computed
            // and inlined into a new ty instruction
            .function_type => try createFunctionType(&analysis, inst),
            .pointer_type => try createPointerType(&analysis, inst),
            .many_pointer_type => try createUnsafePointerType(&analysis, inst),
            .slice_type => try createSliceType(&analysis, inst),
            .array_type => try createArrayType(&analysis, inst),
            // used for coercion logic when the initial ast lowering
            // isn't sure what the type of something is
            // when resolved, yields the type of the operand
            .type_of => try typeOf(&analysis, inst),
            // extracts a parameter type by index from a function type
            .param_type => try paramTypeOf(&analysis, inst),
            // extracts the element type from an array or slice type
            .element_type => try elementTypeOf(&analysis, inst),
            // yields the return type of the current context
            // only valid within a function body (not in a global)
            .ret_type => try retType(&analysis, inst),
            // TODO: remove else clause and switch explicitly
            // to avoid copying bad instructions
            else => try b.linkInst(inst),
        }

        if (hg.insts.items(.tag)[inst] == .constant) {
            const data = hg.get(inst, .constant);
            const ty = try hg.resolveType(data.ty);
            if (ty.kind() == .function) {
                const function_type = ty.extended.cast(Type.Function).?;
                analysis.return_type = function_type.return_type;
            }
        }

        if (explore) try hg.explore(inst, analyze, .{ hg, explore, &analysis });
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

    const function_value = handle: {
        const value = try Value.createFunction(hg.pool.arena, params, fn_decl.body);
        break :handle try hg.pool.internValue(value);
    };

    std.debug.print("adding function: handle = {}\n", .{function_value});
    const constant = try b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = fn_type }),
        .val = function_value,
        .node = fn_decl.node,
    });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn integer(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .int);
    const value = try hg.pool.internValue(.{ .integer = data.int });
    const ty = try b.add(.ty, .{ .ty = Type.Common.comptime_uint });
    // TODO: maybe try to store the node
    const constant = try b.add(.constant, .{ .ty = ty, .val = value, .node = undefined });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn float(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .float);
    const value = try hg.pool.internValue(.{ .float = @bitCast(data.float) });
    const ty = try b.add(.ty, .{ .ty = Type.Common.comptime_float });
    // TODO: maybe try to store the node
    const constant = try b.add(.constant, .{ .ty = ty, .val = value, .node = undefined });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn none(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const constant = try b.add(.constant, .{
        .ty = try b.add(.ty, .{ .ty = Type.Common.void_type }),
        .val = try hg.pool.internValue(.{ .none = 0 }),
        .node = undefined,
    });
    try analysis.src_block.replaceAllUsesWith(inst, constant);
}

fn coerce(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .coerce);
    std.debug.print("coercing %{} (operand %{})\n", .{ inst, data.val });
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
        .dest_type_ref = data.ty,
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
        .dest_type_ref = try b.addType(dest_type),
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
    if (func_type.param_types.len != (call_data.args_end - call_data.args_start)) {
        // incorrect number of arguments to function call
        try hg.errors.append(hg.gpa, .{
            .tag = .call_argcount,
            .token = addr_token, // TODO: maybe something better we can use here?
        });
        return error.HandledUserError;
    }

    try b.linkInst(inst);
}

// replaces a pointer_type instruction with an inline ty instruction
// by constructing a pointer to the operand type
fn createPointerType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .pointer_type);
    const pointee = try hg.resolveType(data.operand);
    const pointer = try Type.Pointer.init(hg.gpa, pointee);
    const new_type = try b.add(.ty, .{ .ty = pointer });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

// replaces a array_type instruction with an inline ty instruction
// by constructing a array to the operand type with the correct count
fn createArrayType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .array_type);
    const element = try hg.resolveType(data.element_type);
    const count = hg.instToInt(data.count);
    const array = try Type.Array.init(hg.gpa, element, @intCast(count));
    const new_type = try b.add(.ty, .{ .ty = array });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

// replaces a many_pointer_type instruction with an inline ty instruction
// by constructing a pointer to the operand type
fn createUnsafePointerType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .many_pointer_type);
    const pointee = try hg.resolveType(data.operand);
    const pointer = try Type.ManyPointer.init(hg.gpa, pointee);
    const new_type = try b.add(.ty, .{ .ty = pointer });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

// replaces a slice_type instruction with an inline ty instruction
// by constructing a slice to the operand type
fn createSliceType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .slice_type);
    const element = try hg.resolveType(data.operand);
    const slice = try Type.Slice.init(hg.gpa, element);
    const new_type = try b.add(.ty, .{ .ty = slice });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn createFunctionType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .create_function_type);
    const return_type = try hg.resolveType(data.return_type);

    const param_types = try hg.gpa.alloc(Type, data.params_end - data.params_start);
    var extra_index: u32 = data.params_start;
    var i: u32 = 0;
    while (extra_index < data.params_end) : (extra_index += 1) {
        const param_type = hg.extra.items[extra_index];
        param_types[i] = try hg.resolveType(param_type);
        i += 1;
    }

    const inner = try hg.gpa.create(Type.Function);
    inner.* = .{ .return_type = return_type, .param_types = param_types };
    const function_type: Type = .{ .extended = &inner.base };

    const new_type = try b.add(.ty, .{ .ty = function_type });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn paramTypeOf(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .param_type);
    const ptr_type = (try hg.resolveType(data.ptr)).extended.cast(Type.Function).?;
    const param_type = ptr_type.param_types[data.param_index];

    const new_type = try b.add(.ty, .{ .ty = param_type });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn elementTypeOf(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .element_type);
    const operand_type = try hg.resolveType(data.operand);
    const element_type = switch (operand_type.kind()) {
        .array => operand_type.extended.cast(Type.Array).?.element,
        .slice => operand_type.extended.cast(Type.Slice).?.element,
        .pointer => ty: {
            const inner_type = operand_type.extended.cast(Type.Pointer).?.pointee;
            break :ty switch (inner_type.kind()) {
                .array => inner_type.extended.cast(Type.Array).?.element,
                .slice => inner_type.extended.cast(Type.Slice).?.element,
                else => unreachable,
            };
        },
        else => unreachable,
    };

    const new_type = try b.add(.ty, .{ .ty = element_type });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn typeOf(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .type_of);
    const type_of = try hg.resolveType(data.operand);

    const new_type = try b.add(.ty, .{ .ty = type_of });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn retType(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const b = analysis.b;
    const new_type = try b.add(.ty, .{ .ty = analysis.return_type.? });
    try analysis.src_block.replaceAllUsesWith(inst, new_type);
}

fn fieldRef(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .field_ref);
    // TODO: use interner value instead
    const field_token = hg.tree.mainToken(data.node) + 1;
    const field_string = hg.tree.tokenString(field_token);

    switch (hg.insts.items(.tag)[data.operand]) {
        // reading a field from a pointer by ref - emit a ref
        .push, .alloca => {
            const pointer_type = (try hg.resolveType(data.operand)).extended.cast(Type.Pointer).?;
            const src_type = pointer_type.pointee;
            // replace the generic field reference by a specific one - builtin, slice, array, structure
            switch (src_type.kind()) {
                // slices only have two runtime fields - ptr and len
                .slice => {
                    if (std.mem.eql(u8, field_string, "ptr")) {
                        const access = try b.add(.slice_ptr_ref, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else if (std.mem.eql(u8, field_string, "len")) {
                        const access = try b.add(.slice_len_ref, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else {
                        std.log.err("field access: no such field {s} for type slice", .{field_string});
                        unreachable;
                    }
                },
                .array, .structure => unreachable, // unimplemented
                else => unreachable,
            }
        },
        // reading a field from a value by ref - emit a val + alloc + store
        else => {
            const src_type = try hg.resolveType(data.operand);
            // replace the generic field reference by a specific one - builtin, slice, array, structure
            switch (src_type.kind()) {
                // slices only have two runtime fields - ptr and len
                .slice => {
                    if (std.mem.eql(u8, field_string, "ptr")) {
                        const access = try b.add(.slice_ptr_val, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        const ptr_type = try Type.Pointer.init(hg.gpa, src_type.extended.cast(Type.Slice).?.element);
                        const slot_type = try Type.Pointer.init(hg.gpa, ptr_type);
                        const push = try b.add(.push, .{
                            .ty = slot_type,
                            .operand = access,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, push);
                    } else if (std.mem.eql(u8, field_string, "len")) {
                        const access = try b.add(.slice_len_val, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        const slot_type = try Type.Pointer.init(hg.gpa, Type.Common.u64_type);
                        const push = try b.add(.push, .{
                            .ty = slot_type,
                            .operand = access,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, push);
                    } else {
                        std.log.err("field access: no such field {s} for type slice", .{field_string});
                        unreachable;
                    }
                },
                .array, .structure => unreachable, // unimplemented
                else => unreachable,
            }
        },
    }
}

fn fieldVal(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .field_val);
    // TODO: use interner value instead
    const field_token = hg.tree.mainToken(data.node) + 1;
    const field_string = hg.tree.tokenString(field_token);

    switch (hg.insts.items(.tag)[data.operand]) {
        // reading a field from a pointer by value - emit a ref + load
        .push, .alloca => {
            const pointer_type = (try hg.resolveType(data.operand)).extended.cast(Type.Pointer).?;
            const src_type = pointer_type.pointee;
            // replace the generic field reference by a specific one - builtin, slice, array, structure
            switch (src_type.kind()) {
                // slices only have two runtime fields - ptr and len
                .slice => {
                    if (std.mem.eql(u8, field_string, "ptr")) {
                        const ref = try b.add(.slice_ptr_ref, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        const access = try b.add(.load, .{
                            .operand = ref,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else if (std.mem.eql(u8, field_string, "len")) {
                        const ref = try b.add(.slice_len_ref, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        const access = try b.add(.load, .{
                            .operand = ref,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else {
                        std.log.err("field access: no such field {s} for type slice", .{field_string});
                        unreachable;
                    }
                },
                .array, .structure => unreachable, // unimplemented
                else => unreachable,
            }
        },
        // reading a field from a value by value - emit a val
        else => {
            const src_type = try hg.resolveType(data.operand);
            // replace the generic field reference by a specific one - builtin, slice, array, structure
            switch (src_type.kind()) {
                // slices only have two runtime fields - ptr and len
                .slice => {
                    if (std.mem.eql(u8, field_string, "ptr")) {
                        const access = try b.add(.slice_ptr_val, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else if (std.mem.eql(u8, field_string, "len")) {
                        const access = try b.add(.slice_len_val, .{
                            .operand = data.operand,
                            .node = data.node,
                        });
                        try analysis.src_block.replaceAllUsesWith(inst, access);
                    } else {
                        std.log.err("field access: no such field {s} for type slice", .{field_string});
                        unreachable;
                    }
                },
                .array, .structure => unreachable, // unimplemented
                else => unreachable,
            }
        },
    }
}

fn indexVal(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .index_val);

    switch (hg.insts.items(.tag)[data.array]) {
        // reading an index from a pointer by value - emit a ref + load
        .push, .alloca => {
            const pointer_type = (try hg.resolveType(data.array)).extended.cast(Type.Pointer).?;
            const src_type = pointer_type.pointee;
            switch (src_type.kind()) {
                .array => {
                    const ref = try b.add(.index_ref, .{
                        .array = data.array,
                        .index = data.index,
                        .node = data.node,
                    });
                    const access = try b.add(.load, .{
                        .operand = ref,
                        .node = data.node,
                    });
                    try analysis.src_block.replaceAllUsesWith(inst, access);
                },
                .slice => unreachable, // unimplemented
                else => unreachable,
            }
        },
        // reading a field from a value by value - emit a val
        else => {
            const src_type = try hg.resolveType(data.array);
            switch (src_type.kind()) {
                .array => {
                    const access = try b.add(.index_val, .{
                        .array = data.array,
                        .index = data.index,
                        .node = data.node,
                    });
                    try analysis.src_block.replaceAllUsesWith(inst, access);
                },
                .slice => unreachable, // unimplemented
                else => unreachable,
            }
        },
    }
}

fn indexRef(analysis: *BlockAnalysis, inst: Hir.Index) !void {
    const hg = analysis.hg;
    const b = analysis.b;
    const data = hg.get(inst, .index_ref);

    switch (hg.insts.items(.tag)[data.array]) {
        // reading an index from a pointer by ref - emit a ref
        .push, .alloca => {
            const pointer_type = (try hg.resolveType(data.array)).extended.cast(Type.Pointer).?;
            const src_type = pointer_type.pointee;
            switch (src_type.kind()) {
                .array => {
                    const access = try b.add(.index_ref, .{
                        .array = data.array,
                        .index = data.index,
                        .node = data.node,
                    });
                    try analysis.src_block.replaceAllUsesWith(inst, access);
                },
                .slice => unreachable, // unimplemented
                else => unreachable,
            }
        },
        // reading a field from a value by value - emit a val
        else => {
            const src_type = try hg.resolveType(data.array);
            switch (src_type.kind()) {
                .array => {
                    const access = try b.add(.index_val, .{
                        .array = data.array,
                        .index = data.index,
                        .node = data.node,
                    });
                    const slot_type = try Type.Pointer.init(hg.gpa, src_type.extended.cast(Type.Array).?.element);
                    const push = try b.add(.push, .{
                        .ty = slot_type,
                        .operand = access,
                        .node = data.node,
                    });
                    try analysis.src_block.replaceAllUsesWith(inst, push);
                },
                .slice => unreachable, // unimplemented
                else => unreachable,
            }
        },
    }
}
