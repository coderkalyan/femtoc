const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const TypedValue = @import("typing.zig").TypedValue;
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");
const Interner = @import("interner.zig").Interner;

const Allocator = std.mem.Allocator;
const Analyzer = @This();
const Value = Mir.Value;

const Error = Allocator.Error || Mir.Error || error {
    InvalidRef,
    TypeError,
    TypeMismatch,
    InvalidOperation,
    CodeError,
    Overflow,
};

mg: *MirGen,
map: MirMap,
hir: *const Hir,
instructions: std.MultiArrayList(Mir.Inst),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(Value),
scratch: std.ArrayListUnmanaged(u32),
errors: std.MultiArrayList(Mir.UserError),
interner: *const Interner,
gpa: Allocator,
arena: Allocator,

pub fn addExtra(analyzer: *Analyzer, extra: anytype) !u32 {
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

pub const Block = struct {
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

    pub fn reserveInst(b: *Block, tag: Mir.Inst.Tag) !Mir.Index {
        const analyzer = b.analyzer;
        const index = @intCast(Mir.Index, analyzer.instructions.len);
        try analyzer.instructions.resize(analyzer.gpa, index + 1);
        analyzer.instructions.items(.tag)[index] = tag;
        try b.instructions.append(analyzer.arena, index);
        return @intCast(Mir.Index, index);
    }

    pub fn setInst(b: *Block, index: u32, inst: Mir.Inst) void {
        const analyzer = b.analyzer;
        analyzer.instructions.set(index, inst);
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

pub fn addBlock(block: *Block) !u32 {
    const analyzer = block.analyzer;
    const insts = block.instructions.items;
    const index = try analyzer.addExtra(Mir.Inst.Block {
        .insts_len = @intCast(u32, insts.len),
    });
    try analyzer.extra.appendSlice(analyzer.gpa, insts);

    return block.addInst(.{
        .tag = .block,
        .data = .{ .pl = index },
    });
}

pub fn analyzeModule(analyzer: *Analyzer, inst: Hir.Index) !void {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const module = analyzer.hir.extraData(pl, Hir.Inst.Module);

    var block = try analyzer.arena.create(Block);
    defer analyzer.arena.destroy(block);
    block.* = Block {
        .parent = null,
        .analyzer = analyzer,
        .instructions = .{},
    };

    const hir_insts = analyzer.hir.extra_data[pl + 1..pl + 1 + module.len];
    try analyzer.map.ensureSliceCapacity(analyzer.arena, hir_insts);
    for (hir_insts) |index| {
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .alloc => try analyzer.alloc(block, index),
            .load => try analyzer.load(block, index),
            .load_inline => try analyzer.loadInline(index),
            .store => try analyzer.store(block, index),
            .validate_ty => try analyzer.validateTy(block, index),
            .add, .sub, .mul, .div, .mod => try analyzer.binaryArithOp(block, index),
            .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge,
            .cmp_lt, .cmp_gt => try analyzer.binaryCmp(block, index),
            .fn_decl => try analyzer.fnDecl(block, index),
            else => Mir.indexToRef(0),
        };
        analyzer.map.putAssumeCapacity(index, ref);
    }
}

pub fn analyzeFunction(analyzer: *Analyzer, inst: Hir.Index) !Mir {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const fn_decl = analyzer.hir.extraData(pl, Hir.Inst.FnDecl);

    var block = try analyzer.arena.create(Block);
    block.* = Block {
        .parent = null,
        .analyzer = analyzer,
        .instructions = .{},
    };

    var extra_index = fn_decl.params_start;
    while (extra_index < fn_decl.params_end) : (extra_index += 1) {
        const param_index = analyzer.hir.extra_data[extra_index];
        const ref = try analyzer.param(block, param_index);
        try analyzer.map.ensureSliceCapacity(analyzer.arena, &.{param_index});
        analyzer.map.putAssumeCapacity(param_index, ref);
    }
    _ = try analyzer.analyzeBlock(block, fn_decl.body);

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = analyzer.interner,
    };
}

pub fn analyzeBlock(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !u32 {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const block_data = analyzer.hir.extraData(pl, Hir.Inst.Block);

    const block = b;

    const hir_insts = analyzer.hir.extra_data[pl + 1..pl + 1 + block_data.len];
    try analyzer.map.ensureSliceCapacity(analyzer.arena, hir_insts);
    for (hir_insts) |index| {
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .alloc => try analyzer.alloc(block, index),
            .load => try analyzer.load(block, index),
            .load_inline => try analyzer.loadInline(index),
            .store => try analyzer.store(block, index),
            .validate_ty => try analyzer.validateTy(block, index),
            .add, .sub, .mul, .div, .mod => try analyzer.binaryArithOp(block, index),
            .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge,
            .cmp_lt, .cmp_gt => try analyzer.binaryCmp(block, index),
            .ret_node => try analyzer.retNode(block, index),
            .ret_implicit => try analyzer.retImplicit(block, index),
            .yield_node => try analyzer.yieldNode(block, index),
            .yield_implicit => try analyzer.yieldImplicit(block, index),
            .branch_single => try analyzer.branchSingle(block, index),
            .branch_double => try analyzer.branchDouble(block, index),
            .loop => try analyzer.loop(block, index),
            .fn_decl => try analyzer.fnDecl(block, index),
            else => Mir.indexToRef(0),
        };
        analyzer.map.putAssumeCapacity(index, ref);
    }
    for (hir_insts) |index| analyzer.map.remove(index);

    const index = try addBlock(block);
    return index;
}

pub fn resolveTy(analyzer: *Analyzer, b: *Block, ref: Mir.Ref) !Type {
    if (Mir.refToIndex(ref)) |index| {
        const data = analyzer.instructions.items(.data)[index];
        return switch (analyzer.instructions.items(.tag)[index]) {
            .constant => data.ty_pl.ty,
            .add, .sub, .mul, .div, .mod => analyzer.resolveTy(b, data.bin_op.lref),
            .cmp_eq, .cmp_ne,
            .cmp_ge, .cmp_le, .cmp_gt, .cmp_lt => analyzer.resolveTy(b, data.bin_op.lref),
            .alloc => data.ty,
            .load => analyzer.resolveTy(b, data.un_op),
            .store => analyzer.resolveTy(b, data.un_op),
            .param => data.ty_pl.ty,
            .call => analyzer.resolveTy(b, data.op_pl.op),
            else => {
                std.debug.print("{}\n", .{analyzer.instructions.items(.tag)[index]});
                return error.NotImplemented;
            },
        };
    } else {
        return switch (ref) {
            .zero_val, .one_val => .{ .tag = .comptime_uint },
            .void_val => .{ .tag = .void },
            else => error.NotImplemented,
        };
    }
}

pub fn param(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const data = analyzer.hir.extraData(pl, Hir.Inst.Param);
    // TODO: proper ref to type
    // const ty = switch (analyzer.map.resolveRef(data.ty)) {

    // };
    const ty = switch (data.ty) {
        .i8_ty => Type.initTag(.i8),
        .u8_ty => Type.initTag(.u8),
        .i16_ty => Type.initTag(.i16),
        .u16_ty => Type.initTag(.u16),
        .i32_ty => Type.initTag(.i32),
        .u32_ty => Type.initTag(.u32),
        .i64_ty => Type.initTag(.i64),
        .u64_ty => Type.initTag(.u64),
        else => return error.TypeError,
    };
    const index = try b.addInst(.{
        .tag = .param,
        .data = .{ .ty_pl = .{ .ty = ty, .pl = data.name } },
    });
    return Mir.indexToRef(index);
}

pub fn integer(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addConstant(Type.initTag(.comptime_uint), .{ .uint = data.int });
    return Mir.indexToRef(index);
}

fn float(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addConstant(Type.initTag(.comptime_float), .{ .float = data.float });
    return Mir.indexToRef(index);
}

fn loadInline(analyzer: *Analyzer, inst: Hir.Index) !Mir.Ref {
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
    _ = try b.addInst(.{
        .tag = .store,
        .data = .{ .bin_op = .{ .lref = Mir.indexToRef(index), .rref = ref } },
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
        switch (found_ty.tag) {
            .comptime_uint => {
                const int_value: u64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].uint;
                } else val: {
                    switch (ref) {
                        .zero_val => break :val 0,
                        .one_val => break :val 1,
                        else => return error.InvalidRef,
                    }
                };
                const index = switch (expected_ty.tag) {
                    .u1, .u8, .u16, .u32, .u64 => index: {
                        break :index try b.addConstant(expected_ty, .{ .uint = int_value });
                    },
                    .i8, .i16, .i32, .i64 => index: {
                        break :index try b.addConstant(expected_ty, .{ .sint = @intCast(i64, int_value) });
                    },
                    else => unreachable,
                };
                return Mir.indexToRef(index);
            },
            .comptime_sint => {
                const int_value: i64 = if (Mir.refToIndex(ref)) |value_index| val: {
                    const pl = analyzer.instructions.items(.data)[value_index].ty_pl.pl;
                    break :val analyzer.values.items[pl].sint;
                } else switch (ref) {
                    .zero_val => return Mir.Ref.zero_val,
                    .one_val => return Mir.Ref.one_val,
                    else => return error.InvalidRef,
                };
                const index = try b.addConstant(expected_ty, .{ .sint = int_value });
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

    if (lty.compareTag(.comptime_uint) or lty.compareTag(.comptime_sint)) {
        if (rty.compareTag(.comptime_uint) or rty.compareTag(.comptime_sint)) {
            return analyzer.analyzeComptimeArithmetic(b, inst);
        }
    }

    switch (lty.tag) {
        .comptime_uint => {
            switch (rty.tag) {
                .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                    const lval = try analyzer.coerceUnsignedInt(b, data.pl_node.pl, rty, lref);
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => .mod,
                            else => return error.NotImplemented,
                        },
                        .data = .{ .bin_op = .{ .lref = lval, .rref = rref } },
                    });
                    return Mir.indexToRef(index);
                },
                else => return error.TypeMismatch,
            }
        },
        .comptime_sint => {
            switch (rty.tag) {
                .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
                    const lval = try analyzer.coerceSignedInt(b, data.pl_node.pl, rty, lref);
                    const index = try b.addInst(.{
                        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
                            .add => .add,
                            .sub => .sub,
                            .mul => .mul,
                            .div => .div,
                            .mod => .mod,
                            else => return error.NotImplemented,
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
                .comptime_uint => {
                    const rval = try analyzer.coerceUnsignedInt(b, data.pl_node.pl, lty, rref);
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
                .comptime_sint => {
                    const rval = try analyzer.coerceSignedInt(b, data.pl_node.pl, lty, rref);
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
                    const lval = analyzer.refToFloat(lref);
                    const rval = analyzer.refToFloat(rref);
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
                    const lval = analyzer.refToFloat(lref);
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
                    const rval = analyzer.refToFloat(rref);
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

fn binaryCmp(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    std.debug.print("cmp\n", .{});
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);

    if (lty.isComptimeInteger() and rty.isComptimeInteger()) {
        // TODO: we shouldn't be demoting
        const lval = try analyzer.demoteMaybeUnsignedInt(b, lref, data.pl_node.pl);
        const rval = try analyzer.demoteMaybeUnsignedInt(b, rref, data.pl_node.pl);
        const result = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .cmp_eq => lval == rval,
            .cmp_ne => lval != rval,
            .cmp_le => lval <= rval,
            .cmp_ge => lval >= rval,
            .cmp_lt => lval < rval,
            .cmp_gt => lval > rval,
            else => unreachable,
        };

        return analyzer.unsignedIntToRef(b, @boolToInt(result));
    }

    if (lty.compareTag(.comptime_float) and rty.compareTag(.comptime_float)) {
        const lval = analyzer.refToFloat(lref);
        const rval = analyzer.refToFloat(rref);
        const result = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .cmp_eq => lval == rval,
            .cmp_ne => lval != rval,
            .cmp_le => lval <= rval,
            .cmp_ge => lval >= rval,
            .cmp_lt => lval < rval,
            .cmp_gt => lval > rval,
            else => unreachable,
        };

        return analyzer.unsignedIntToRef(b, @boolToInt(result));
    }

    // TODO: coercion
    // switch (lty.tag) {
    //     .i8, .u8, .i16, .u16, .i32, .u32, .i64, .u64 => {
    std.debug.print("cmp inst\n", .{});
    const index = try b.addInst(.{
        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .cmp_eq => .cmp_eq,
            .cmp_ne => .cmp_ne,
            .cmp_le => .cmp_le,
            .cmp_ge => .cmp_ge,
            .cmp_lt => .cmp_lt,
            .cmp_gt => .cmp_gt,
            else => unreachable,
        },
        .data = .{ .bin_op = .{ .lref = lref, .rref = rref } },
    });
    return Mir.indexToRef(index);
    //     },
    //     .f32, .f64 => {

    //     },
    //     else => unreachable,
    // }
}

fn refToUnsignedInt(analyzer: *Analyzer, ref: Mir.Ref) u64 {
    switch (ref) {
        .zero_val => return 0,
        .one_val => return 1,
        else => {
            const index = Mir.refToIndex(ref).?;
            const pl = analyzer.instructions.items(.data)[index].ty_pl.pl;
            return analyzer.values.items[pl].uint;
        },
    }
}

fn refToSignedInt(analyzer: *Analyzer, ref: Mir.Ref) i64 {
    const index = Mir.refToIndex(ref).?;
    const pl = analyzer.instructions.items(.data)[index].ty_pl.pl;
    return analyzer.values.items[pl].sint;
}

fn refToFloat(analyzer: *Analyzer, ref: Mir.Ref) f64 {
    const index = Mir.refToIndex(ref).?;
    const pl = analyzer.instructions.items(.data)[index].ty_pl.pl;
    return analyzer.values.items[pl].float;
}

// this function attempts to catch the majority of compile time
// arithmetic undefined behaviors (overflows, underflows, truncations)
// during the simple "constant folding" style comptime_int simplification
// eventually, this will be replaced with architecture-specific ALU emulation
fn analyzeComptimeArithmetic(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);

    std.debug.assert(lty.isTag());
    std.debug.assert(rty.isTag());

    switch (analyzer.hir.insts.items(.tag)[inst]) {
        .add => {
            if ((lty.tag == .comptime_uint) and (rty.tag == .comptime_uint)) {
                const lval = analyzer.refToUnsignedInt(lref);
                const rval = analyzer.refToUnsignedInt(rref);
                if (std.math.maxInt(u64) - lval < rval) {
                    try analyzer.emitUserError(data.pl_node.pl, .uint_overflow);
                    unreachable;
                } else {
                    const result: u64 = lval + rval;
                    return analyzer.unsignedIntToRef(b, result);
                }
            } else if ((lty.tag == .comptime_sint) and (rty.tag == .comptime_sint)) {
                const lval = analyzer.refToSignedInt(lref);
                const rval = analyzer.refToSignedInt(rref);
                if (lval < std.math.minInt(i64) - rval) {
                    try analyzer.emitUserError(data.pl_node.pl, .sint_underflow);
                    unreachable;
                } else {
                    const result: i64 = lval + rval;
                    return analyzer.signedIntToRef(b, result);
                }
            } else {
                const lval = try analyzer.demoteMaybeUnsignedInt(b, lref, data.pl_node.pl);
                const rval = try analyzer.demoteMaybeUnsignedInt(b, rref, data.pl_node.pl);
                const result: i64 = lval + rval;
                return analyzer.signedIntToRef(b, result);
            }
        },
        .sub => {
            if ((lty.tag == .comptime_uint) and (rty.tag == .comptime_sint)) {
                const lval = analyzer.refToUnsignedInt(lref);
                const rval: u64 = @intCast(u64, try std.math.absInt(analyzer.refToSignedInt(rref)));
                if (lval > std.math.maxInt(u64) + rval) {
                    try analyzer.emitUserError(data.pl_node.pl, .uint_overflow);
                    unreachable;
                } else {
                    const result: u64 = lval + rval;
                    return analyzer.unsignedIntToRef(b, result);
                }
            } else if ((lty.tag == .comptime_sint) and (rty.tag == .comptime_uint)) {
                const lval = analyzer.refToSignedInt(lref);
                const rval = try analyzer.demoteUnsignedInt(rref, data.pl_node.pl);
                if (lval < std.math.minInt(i64) + rval) {
                    try analyzer.emitUserError(data.pl_node.pl, .sint_underflow);
                    unreachable;
                } else {
                    const result: i64 = lval - rval;
                    return analyzer.signedIntToRef(b, result);
                }
            } else {
                const lval = try analyzer.demoteMaybeUnsignedInt(b, lref, data.pl_node.pl);
                const rval = try analyzer.demoteMaybeUnsignedInt(b, rref, data.pl_node.pl);
                const result: i64 = lval - rval;
                return analyzer.signedIntToRef(b, result);
            }
        },
        .mul => {
            switch (lty.tag) {
                .comptime_uint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const lval = analyzer.refToUnsignedInt(lref);
                            const rval = analyzer.refToUnsignedInt(rref);
                            if ((rval != 0) and (lval > std.math.maxInt(u64) / rval)) {
                                try analyzer.emitUserError(data.pl_node.pl, .uint_overflow);
                                unreachable;
                            }
                            const result = lval * rval;
                            return analyzer.unsignedIntToRef(b, result);
                        },
                        .comptime_sint => {
                            const lval = try analyzer.demoteUnsignedInt(lref, data.pl_node.pl);
                            const rval = analyzer.refToSignedInt(rref);
                            if ((rval != 0) and (lval < std.math.minInt(u64) / rval)) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_underflow);
                                unreachable;
                            }
                            const result = lval * rval;
                            return analyzer.signedIntToRef(b, result);
                        },
                        else => unreachable,
                    }
                },
                .comptime_sint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const lval = analyzer.refToSignedInt(lref);
                            const rval = try analyzer.demoteUnsignedInt(rref, data.pl_node.pl);
                            if ((rval != 0) and (lval < std.math.minInt(u64) / rval)) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_underflow);
                                unreachable;
                            }
                            const result = lval * rval;
                            return analyzer.signedIntToRef(b, result);
                        },
                        .comptime_sint => {
                            const lval = analyzer.refToSignedInt(lref);
                            const rval = analyzer.refToSignedInt(rref);
                            if ((lval == -1) and (rval == std.math.minInt(i64))) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_flip_overflow);
                                unreachable;
                            }
                            if ((rval == -1) and (lval == std.math.minInt(i64))) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_flip_overflow);
                                unreachable;
                            }
                            const result = lval * rval;
                            return analyzer.signedIntToRef(b, result);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .div => {
            switch (lty.tag) {
                .comptime_uint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const lval = analyzer.refToUnsignedInt(lref);
                            const rval = analyzer.refToUnsignedInt(rref);
                            if (rval == 0) {
                                try analyzer.emitUserError(data.pl_node.pl, .divisor_zero);
                                unreachable;
                            }
                            return analyzer.unsignedIntToRef(b, lval / rval);
                        },
                        .comptime_sint => {
                            const lval = try analyzer.demoteUnsignedInt(lref, data.pl_node.pl);
                            const rval = analyzer.refToSignedInt(rref);
                            return analyzer.signedIntToRef(b, @divTrunc(lval, rval));
                        },
                        else => unreachable,
                    }
                },
                .comptime_sint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const lval = analyzer.refToSignedInt(lref);
                            const rval = try analyzer.demoteUnsignedInt(rref, data.pl_node.pl);
                            if (rval == 0) {
                                try analyzer.emitUserError(data.pl_node.pl, .divisor_zero);
                                unreachable;
                            }
                            return analyzer.signedIntToRef(b, @divTrunc(lval, rval));
                        },
                        .comptime_sint => {
                            const lval = analyzer.refToSignedInt(lref);
                            const rval = analyzer.refToSignedInt(rref);
                            if ((lval == -1) and (rval == std.math.minInt(i64))) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_flip_overflow);
                                unreachable;
                            }
                            if ((rval == -1) and (lval == std.math.minInt(i64))) {
                                try analyzer.emitUserError(data.pl_node.pl, .sint_flip_overflow);
                                unreachable;
                            }
                            return analyzer.signedIntToRef(b, @divTrunc(lval, rval));
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .mod => {
            switch (lty.tag) {
                .comptime_uint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const lval = analyzer.refToUnsignedInt(lref);
                            const rval = analyzer.refToUnsignedInt(rref);

                            if (analyzer.refToUnsignedInt(rref) == 0) {
                                try analyzer.emitUserError(data.pl_node.pl, .divisor_zero);
                                unreachable;
                            }

                            const result = lval % rval;
                            return analyzer.unsignedIntToRef(b, result);
                        },
                        .comptime_sint => {
                            const lval = try analyzer.demoteUnsignedInt(lref, data.pl_node.pl);
                            const rval = analyzer.refToSignedInt(rref);
                            const result = @mod(lval, rval);
                            return analyzer.signedIntToRef(b, result);
                        },
                        else => unreachable,
                    }
                },
                .comptime_sint => {
                    switch (rty.tag) {
                        .comptime_uint => {
                            const rval = try analyzer.demoteUnsignedInt(lref, data.pl_node.pl);

                            if (analyzer.refToUnsignedInt(rref) == 0) {
                                try analyzer.emitUserError(data.pl_node.pl, .divisor_zero);
                                unreachable;
                            }

                            const lval = analyzer.refToSignedInt(rref);
                            const result = @mod(lval, rval);
                            return analyzer.signedIntToRef(b, result);
                        },
                        .comptime_sint => {
                            const lval = analyzer.refToSignedInt(lref);
                            const rval = analyzer.refToSignedInt(rref);
                            const result = @mod(lval, rval);
                            return analyzer.signedIntToRef(b, result);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        .lsl, .lsr, .asl, .asr => return error.NotImplemented,
        else => unreachable,
    }
}

fn emitUserError(analyzer: *Analyzer, node: u32, tag: Mir.UserError.Tag) !void {
    try analyzer.errors.append(analyzer.gpa, .{
        .node = node,
        .tag = tag,
    });
    return error.CodeError;
}

fn demoteUnsignedInt(analyzer: *Analyzer, ref: Mir.Ref, node: u32) !i64 {
    const uint = analyzer.refToUnsignedInt(ref);
    if (uint > std.math.maxInt(i64)) {
        try analyzer.emitUserError(node, .uint_sign_cast_overflow);
        unreachable;
    } else {
        return @intCast(i64, uint);
    }
}

fn demoteMaybeUnsignedInt(analyzer: *Analyzer, b: *Block, ref: Mir.Ref, node: u32) !i64 {
    const ty = try analyzer.resolveTy(b, ref);
    return if (ty.tag == .comptime_uint) analyzer.demoteUnsignedInt(ref, node)
           else analyzer.refToSignedInt(ref);
}

fn signedIntToRef(analyzer: *Analyzer, b: *Block, sint: i64) !Mir.Ref {
    _ = analyzer;
    switch (sint) {
        0 => return .zero_val,
        1 => return .one_val,
        else => {
            if (sint > 0) {
                const uint = @intCast(u64, sint);
                const index = try b.addConstant(Type.initTag(.comptime_uint), .{ .uint = uint });
                return Mir.indexToRef(index);
            } else {
                const index = try b.addConstant(Type.initTag(.comptime_sint), .{ .sint = sint });
                return Mir.indexToRef(index);
            }
        }
    }
}

fn unsignedIntToRef(analyzer: *Analyzer, b: *Block, uint: u64) !Mir.Ref {
    _ = analyzer;
    switch (uint) {
        0 => return .zero_val,
        1 => return .one_val,
        else => {
            const index = try b.addConstant(Type.initTag(.comptime_uint), .{ .uint = uint });
            return Mir.indexToRef(index);
        }
    }
}

fn coerceUnsignedInt(analyzer: *Analyzer, b: *Block, node: u32, ty: Type, ref: Mir.Ref) !Mir.Ref {
    const val = analyzer.refToUnsignedInt(ref);
    if ((val >= 0) and (val <= ty.intMaxValue())) {
        const index = try b.addConstant(ty, switch (ty.tag) {
            .u8, .u16, .u32, .u64 => .{ .uint = val },
            .i8, .i16, .i32, .i64 => .{ .sint = @intCast(i64, val) },
            else => unreachable,
        });
        return Mir.indexToRef(index);
    } else {
        try analyzer.emitUserError(node, .coerce_overflow);
        unreachable;
    }
}

fn coerceSignedInt(analyzer: *Analyzer, b: *Block, node: u32, ty: Type, ref: Mir.Ref) !Mir.Ref {
    const val = analyzer.refToSignedInt(ref);
    if (val < ty.intMinValue()) {
        try analyzer.emitUserError(node, .coerce_underflow);
        unreachable;
    } else if (val > ty.intMaxValue()) {
        try analyzer.emitUserError(node, .coerce_overflow);
        unreachable;
    } else {
        const index = try b.addConstant(ty, .{ .sint = val });
        return Mir.indexToRef(index);
    }
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

fn yieldNode(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_node.operand);
    const index = try b.addInst(.{
        .tag = .yield,
        .data = .{ .un_op = ref },
    });
    return Mir.indexToRef(index);
}

fn yieldImplicit(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const ref = analyzer.map.resolveRef(data.un_tok.operand);
    const index = try b.addInst(.{
        .tag = .yield,
        .data = .{ .un_op = ref },
    });
    return Mir.indexToRef(index);
}

fn branchSingle(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const branch_single = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.BranchSingle);
    const condition = analyzer.map.resolveRef(branch_single.condition);

    const body = block: {
        const block = &Block {
            .parent = b,
            .analyzer = analyzer,
            .instructions = .{},
        };
        break :block try analyzer.analyzeBlock(block, branch_single.exec_true);
    };

    const index = try b.addInst(.{
        .tag = .branch_single,
        .data = .{ .op_pl = .{ .op = condition, .pl = body } },
    });
    return Mir.indexToRef(index);
}

fn branchDouble(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const branch_double = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.BranchDouble);
    const condition = analyzer.map.resolveRef(branch_double.condition);

    const exec_true = block: {
        const block = &Block {
            .parent = b,
            .analyzer = analyzer,
            .instructions = .{},
        };
        break :block try analyzer.analyzeBlock(block, branch_double.exec_true);
    };
    const exec_false = block: {
        const block = &Block {
            .parent = b,
            .analyzer = analyzer,
            .instructions = .{},
        };
        break :block try analyzer.analyzeBlock(block, branch_double.exec_false);
    };

    const condbr = try analyzer.addExtra(Mir.Inst.CondBr {
        .exec_true = exec_true,
        .exec_false = exec_false,
    });
    const index = try b.addInst(.{
        .tag = .branch_double,
        .data = .{ .op_pl = .{ .op = condition, .pl = condbr } },
    });
    return Mir.indexToRef(index);
}

fn loop(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const loop_data = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Loop);
    const condition = block: {
        const block = &Block {
            .parent = b,
            .analyzer = analyzer,
            .instructions = .{},
        };
        break :block try analyzer.analyzeBlock(block, loop_data.condition);
    };

    const body = block: {
        const block = &Block {
            .parent = b,
            .analyzer = analyzer,
            .instructions = .{},
        };
        break :block try analyzer.analyzeBlock(block, loop_data.body);
    };

    const extra_index = try analyzer.addExtra(Mir.Inst.Loop {
        .condition = condition,
        .body = body,
    });
    const index = try b.addInst(.{
        .tag = .loop,
        .data = .{ .pl = extra_index },
    });
    return Mir.indexToRef(index);
}

fn fnDecl(parent: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    _ = b;

    const mg = parent.mg;
    var arena = std.heap.ArenaAllocator.init(parent.gpa);
    defer arena.deinit();

    var analyzer = Analyzer {
        .mg = mg,
        .map = MirMap.init(&mg.map),
        .hir = parent.hir,
        .gpa = parent.gpa,
        .arena = arena.allocator(),
        .instructions = .{},
        .extra = .{},
        .values = .{},
        .scratch = .{},
        .errors = .{},
        .interner = parent.interner,
    };

    const mir = try analyzer.analyzeFunction(inst);
    try parent.mg.mir.append(parent.gpa, mir);

    return @intToEnum(Mir.Ref, 0); // TODO: change to real function reference
}
