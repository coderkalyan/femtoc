const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const TypedValue = @import("typing.zig").TypedValue;
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");

const Allocator = std.mem.Allocator;
const Analyzer = @This();
const Value = Mir.Value;

const Error = Allocator.Error || Mir.Error || error {
    InvalidRef,
    TypeError,
    TypeMismatch,
    InvalidOperation,
};

mg: *MirGen,
map: MirMap,
hir: *const Hir,
instructions: std.MultiArrayList(Mir.Inst),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(Value),
scratch: std.ArrayListUnmanaged(u32),
gpa: Allocator,
arena: Allocator,

const Block = struct {
    parent: ?*Block,
    analyzer: *Analyzer,
    instructions: std.ArrayListUnmanaged(Mir.Index),

    pub fn addInst(b: *Block, inst: Mir.Inst) !Mir.Index {
        const analyzer = b.analyzer;
        const index = @intCast(Mir.Index, analyzer.instructions.len);
        try analyzer.instructions.append(analyzer.gpa, inst);
        try b.instructions.append(analyzer.arena, index);
        return index;
    }

    pub fn addExtra(b: *Block, extra: anytype) !u32 {
        const analyzer = b.analyzer;
        if (@TypeOf(extra) == u32) {
            analyzer.extra.append(analyzer.gpa, extra);
            return analyzer.extra.items.len - 1;
        } else {
            const fields = std.meta.fields(@TypeOf(extra));
            try analyzer.extra.ensureUnusedCapacity(analyzer.gpa, fields.len);
            const len = @intCast(u32, analyzer.extra.items.len);
            inline for (fields) |field| {
                if (field.field_type == u32) {
                    analyzer.extra.appendAssumeCapacity(@field(extra, field.name));
                } else if (field.field_type == Mir.Ref) {
                    analyzer.extra.appendAssumeCapacity(@enumToInt(@field(extra, field.name)));
                } else {
                    unreachable;
                }
            }
            return len;
        }
    }

    pub fn addValue(b: *Block, val: Value) !u32 {
        const analyzer = b.analyzer;
        const len = @intCast(u32, analyzer.values.items.len);
        try analyzer.values.append(analyzer.gpa, val);
        return len;
    }

    pub fn addConstant(b: *Block, ty: Type, val: Value) !u32 {
        const value = try b.addValue(val);
        const index = try b.addInst(.{
            .tag = .constant,
            .data = .{
                .ty_pl = .{
                    .ty = ty,
                    .pl = value,
                }
            }
        });
        return index;
    }
};

pub fn analyzeBody(analyzer: *Analyzer, inst: Hir.Index) !Mir {
    var block = Block {
        .parent = null,
        .analyzer = analyzer,
        .instructions = .{},
    };
    _ = try analyzer.analyzeBlock(&block, inst);

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
    };
}

pub fn analyzeBlock(analyzer: *Analyzer, block: *Block, inst: Hir.Index) !u32 {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const block_data = analyzer.hir.extraData(pl, Hir.Inst.Block);

    const scratch_top = analyzer.scratch.items.len;
    defer analyzer.scratch.shrinkRetainingCapacity(scratch_top);

    const hir_insts = analyzer.hir.extra_data[pl + 1..pl + 1 + block_data.len];
    try analyzer.map.ensureSliceCapacity(analyzer.arena, hir_insts);
    for (hir_insts) |index| {
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .alloc => try analyzer.alloc(block, index),
            .load => try analyzer.load(block, index),
            .load_inline => try analyzer.loadInline(block, index),
            .store => try analyzer.store(block, index),
            .validate_ty => try analyzer.validateTy(block, index),
            .add, .sub, .mul, .div, .mod => try analyzer.binaryArithOp(block, index),
            .ret_node => try analyzer.retNode(block, index),
            .ret_implicit => try analyzer.retImplicit(block, index),
            .branch_single => try analyzer.branchSingle(block, index),
            // .branch_double => try analyzer.branchDouble(block, index),
            else => Mir.indexToRef(0),
        };
        analyzer.map.putAssumeCapacity(index, ref);
        std.debug.print("{}\n", .{ref});
        if (Mir.refToIndex(ref)) |mir_index| try analyzer.scratch.append(analyzer.gpa, mir_index);
    }
    for (hir_insts) |index| analyzer.map.remove(index);

    const insts = analyzer.scratch.items[scratch_top..];
    const index = try block.addInst(.{
        .tag = .block,
        .data = .{
            .pl = try block.addExtra(Mir.Inst.Block { .insts_len = @intCast(u32, insts.len) })
        },
    });
    try analyzer.extra.appendSlice(analyzer.gpa, insts);
    std.debug.print("{any}\n", .{insts});

    return index;
}

pub fn resolveTy(analyzer: *Analyzer, b: *Block, ref: Mir.Ref) !Type {
    if (Mir.refToIndex(ref)) |index| {
        const data = analyzer.instructions.items(.data)[index];
        return switch (analyzer.instructions.items(.tag)[index]) {
            .constant => data.ty_pl.ty,
            .add, .sub, .mul, .div, .mod => analyzer.resolveTy(b, data.bin_op.lref),
            .eq, .neq, .geq,
            .leq, .gt, .lt => analyzer.resolveTy(b, data.bin_op.lref),
            .alloc => data.ty,
            else => error.NotImplemented,
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => .{ .tag = .comptime_int },
            .void_val => .{ .tag = .void },
            else => error.NotImplemented,
        };
    }
}

pub fn integer(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    _ = analyzer;
    const data = b.analyzer.hir.insts.items(.data)[inst];
    const index = try b.addConstant(Type.initTag(.comptime_int), .{ .int = data.int });
    return Mir.indexToRef(index);
}

fn float(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = b.analyzer.hir.insts.items(.data)[inst];
    _ = analyzer;
    const index = try b.addConstant(Type.initTag(.comptime_float), .{ .float = data.float });
    return Mir.indexToRef(index);
}

fn loadInline(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    _ = b;
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.hir.resolution_map.get(data.pl_node.pl).?;
    return analyzer.map.resolveRef(ref);
}

fn load(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addInst(.{
        .tag = .load,
        .data = .{ .un_op = analyzer.map.resolveRef(data.un_node.operand) },
    });
    return Mir.indexToRef(index);
}

fn store(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const store_data = analyzer.mg.hir.extraData(data.pl_node.pl, Hir.Inst.Store);
    const addr = analyzer.map.resolveRef(Hir.Inst.indexToRef(store_data.addr));
    const val = analyzer.map.resolveRef(store_data.val);
    const index = try b.addInst(.{
        .tag = .store,
        .data = .{ .bin_op = .{ .lref = addr, .rref = val } },
    });
    return Mir.indexToRef(index);
}

fn alloc(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_node.operand);
    const ty = try analyzer.resolveTy(b, ref);
    const index = try b.addInst(.{
        .tag = .alloc,
        .data = .{ .ty = ty },
    });
    return Mir.indexToRef(index);
}

fn validateTy(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const validate_ty = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.ValidateTy);
    const ref = analyzer.map.resolveRef(validate_ty.ref);
    const found_ty = try analyzer.resolveTy(b, ref);
    const expected_ty = validate_ty.ty.toType();

    if (found_ty.tag == expected_ty.tag) {
        return ref;
    } else {
        // std.debug.print("ty: {} {}\n", .{expected_ty.tag, found_ty.tag});
        // std.debug.print("val: {}\n", .{});
        switch (found_ty.tag) {
            .comptime_int => {
                const int_value: u64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].int;
                } else switch (ref) {
                    .zero_val => return Mir.Ref.zero_val,
                    .one_val => return Mir.Ref.one_val,
                    else => return error.InvalidRef,
                };
                const index = try b.addConstant(expected_ty, .{ .int = int_value });
                return Mir.indexToRef(index);
            },
            .comptime_float => {
                const float_value: f64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].float;
                } else {
                    return error.InvalidRef;
                };
                const index = try b.addConstant(expected_ty, .{ .float = float_value });
                return Mir.indexToRef(index);
            },
            else => return error.TypeError,
        }
    }
}

fn binaryArithOp(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);

    switch (lty.tag) {
        .comptime_int => {
            switch (rty.tag) {
                .comptime_int => {
                    const lval = analyzer.intConstantValue(lref);
                    const rval = analyzer.intConstantValue(rref);
                    const val = .{
                        .int = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => lval + rval,
                            .sub => lval - rval,
                            .mul => lval * rval,
                            .div => lval / rval,
                            .mod => lval % rval,
                            else => unreachable,
                        }
                    };
                    const index = try b.addConstant(Type.initTag(.comptime_int), val);
                    return Mir.indexToRef(index);
                },
                .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                    const lval: Mir.Ref = switch (lref) {
                        .zero_val => .zero_val,
                        .one_val => .one_val,
                        else => ref: {
                            const int = analyzer.intConstantValue(lref);
                            const index = try b.addConstant(rty, .{ .int = int });
                            break :ref Mir.indexToRef(index);
                        },
                    };
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => .mod,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lval, .rref = rref } },
                    });
                    return Mir.indexToRef(index);
                },
                else => return error.TypeMismatch,
            }
        },
        .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
            switch (rty.tag) {
                .comptime_int => {
                    const rval: Mir.Ref = switch (rref) {
                        .zero_val => .zero_val,
                        .one_val => .one_val,
                        else => ref: {
                            const int = analyzer.intConstantValue(rref);
                            const index = try b.addConstant(lty, .{ .int = int });
                            break :ref Mir.indexToRef(index);
                        },
                    };
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => .mod,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lref, .rref = rval } },
                    });
                    return Mir.indexToRef(index);
                },
                .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => .mod,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lref, .rref = rref } },
                    });
                    return Mir.indexToRef(index);
                },
                else => return error.TypeMismatch,
            }
        },
        .comptime_float => {
            switch (rty.tag) {
                .comptime_float => {
                    const lval = analyzer.floatConstantValue(lref);
                    const rval = analyzer.floatConstantValue(rref);
                    const val = .{
                        .float = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => lval + rval,
                            .sub => lval - rval,
                            .mul => lval * rval,
                            .div => lval / rval,
                            .mod => return error.InvalidOperation,
                            else => unreachable,
                        }
                    };
                    const index = try b.addConstant(Type.initTag(.comptime_float), val);
                    return Mir.indexToRef(index);
                },
                .f32, .f64 => {
                    const lval = analyzer.floatConstantValue(lref);
                    const value = try b.addConstant(rty, .{ .float = lval });
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => return error.InvalidOperation,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = Mir.indexToRef(value), .rref = rref } },
                    });
                    return Mir.indexToRef(index);
                },
                else => return error.TypeMismatch,
            }
        },
        .f32, .f64 => {
            switch (rty.tag) {
                .comptime_float => {
                    const rval = analyzer.floatConstantValue(rref);
                    const value = try b.addConstant(lty, .{ .float = rval });
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => return error.InvalidOperation,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lref, .rref = Mir.indexToRef(value) } },
                    });
                    return Mir.indexToRef(index);
                },
                .f32, .f64 => {
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => return error.InvalidOperation,
                            else => unreachable,
                        },
                        .data = .{ .bin_op = .{ .lref = lref, .rref = rref } },
                    });
                    return Mir.indexToRef(index);
                },
                else => return error.TypeMismatch,
            }
        },
        else => return error.TypeMismatch,
    }
}

fn intConstantValue(analyzer: *Analyzer, ref: Mir.Ref) u64 {
    const index = Mir.refToIndex(ref).?;
    const pl = analyzer.instructions.items(.data)[index].ty_pl.pl;
    return analyzer.values.items[pl].int;
}

fn floatConstantValue(analyzer: *Analyzer, ref: Mir.Ref) f64 {
    const index = Mir.refToIndex(ref).?;
    const pl = analyzer.instructions.items(.data)[index].ty_pl.pl;
    return analyzer.values.items[pl].float;
}

fn retNode(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_node.operand);
    const index = try b.addInst(.{
        .tag = .ret,
        .data = .{ .un_op = ref },
    });
    return Mir.indexToRef(index);
}

fn retImplicit(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_tok.operand);
    const index = try b.addInst(.{
        .tag = .ret,
        .data = .{ .un_op = ref },
    });
    return Mir.indexToRef(index);
}

fn branchSingle(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const branch_single = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.BranchSingle);
    const condition = analyzer.map.resolveRef(branch_single.condition);
    const exec_true = try analyzer.analyzeBlock(b, branch_single.exec_true);
    const exec_false = try b.addInst(.{
        .tag = .block,
        .data = .{ .pl = try b.addExtra(Mir.Inst.Block { .insts_len = 0 }) },
    });
    const condbr = try b.addExtra(Mir.Inst.CondBr {
        .exec_true = exec_true,
        .exec_false = exec_false,
    });

    const index = try b.addInst(.{
        .tag = .condbr,
        .data = .{ .op_pl = .{ .op = condition, .pl = condbr } },
    });
    return Mir.indexToRef(index);
}
