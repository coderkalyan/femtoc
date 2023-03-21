const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Value = @import("value.zig").Value;
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");
const Interner = @import("interner.zig").Interner;
const alu = @import("alu.zig");
const coercion = @import("coercion.zig");

const Allocator = std.mem.Allocator;
const Analyzer = @This();

const Error = Allocator.Error || Mir.Error || coercion.Error || error {
    InvalidRef,
    TypeError,
    TypeMismatch,
    InvalidOperation,
    CodeError,
    Overflow,
    DivZero,
    UnspecificType,
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
                    .ty = try b.typeToRef(ty),
                    .pl = value,
                }
            }
        });
        return index;
    }

    pub fn addIntConstant(b: *Block, ty: Type, int: u64) !u32 {
        std.debug.assert(ty.basic.width <= 64);
        if (int == 0) return b.addConstant(ty, .{ .tag = .zero });
        if (int == 1) return b.addConstant(ty, .{ .tag = .one });
        if (ty.basic.width <= 32) {
            var payload = try b.analyzer.gpa.create(Value.Payload.U32);
            payload.* = .{ .int = @truncate(u32, int) };
            return b.addConstant(ty, .{ .payload = &payload.base });
        } else {
            var payload = try b.analyzer.gpa.create(Value.Payload.U64);
            payload.* = .{ .int = int };
            return b.addConstant(ty, .{ .payload = &payload.base });
        }
    }

    pub fn addFloatConstant(b: *Block, ty: Type, f: f64) !u32 {
        var payload = try b.analyzer.gpa.create(Value.Payload.F64);
        payload.* = .{ .float = f };
        return b.addConstant(ty, .{ .payload = &payload.base });
    }

    pub fn addType(b: *Block, ty: Type) !u32 {
        const index = try b.addInst(.{
            .tag = .ty,
            .data = .{ .ty = ty },
        });
        return index;
    }

    pub fn typeToRef(b: *Block, ty: Type) !Mir.Ref {
        return switch (ty.kind()) {
            .void => .void_ty,
            .uint => switch (ty.basic.width) {
                1 => .u1_ty,
                8 => .u8_ty,
                16 => .u16_ty,
                32 => .u32_ty,
                64 => .u64_ty,
                else => Mir.indexToRef(try b.addType(ty)),
            },
            .sint => switch (ty.basic.width) {
                8 => .i8_ty,
                16 => .i16_ty,
                32 => .i32_ty,
                64 => .i64_ty,
                else => Mir.indexToRef(try b.addType(ty)),
            },
            .float => switch (ty.basic.width) {
                32 => .f32_ty,
                64 => .f64_ty,
                else => error.NotImplemented,
            },
            else => return Mir.indexToRef(try b.addType(ty)),
        };
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

pub fn analyzeModule(analyzer: *Analyzer, inst: Hir.Index) !u32 {
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
            else => Mir.indexToRef(std.math.maxInt(u32) - @intCast(u32, @typeInfo(Mir.Ref).Enum.fields.len)),
        };
        analyzer.map.putAssumeCapacity(index, ref);
    }

    const index = try addBlock(block);
    return index;
}

pub fn analyzeFunction(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const fn_decl = analyzer.hir.extraData(pl, Hir.Inst.FnDecl);
    const mir = analyzer.getTempMir();

    var block = try analyzer.arena.create(Block);
    defer analyzer.arena.destroy(block);
    block.* = Block {
        .parent = b,
        .analyzer = analyzer,
        .instructions = .{},
    };

    const param_tys = try analyzer.gpa.alloc(Type, fn_decl.params_end - fn_decl.params_start);

    var extra_index = fn_decl.params_start;
    while (extra_index < fn_decl.params_end) : (extra_index += 1) {
        const param_index = analyzer.hir.extra_data[extra_index];
        const ref = try analyzer.param(block, param_index);
        try analyzer.map.ensureSliceCapacity(analyzer.arena, &.{param_index});
        analyzer.map.putAssumeCapacity(param_index, ref);
        param_tys[extra_index - fn_decl.params_start] = try analyzer.resolveTy(block, ref);
    }

    const block_index = try analyzer.analyzeBlock(block, fn_decl.body);
    const return_ty = mir.refToType(analyzer.map.resolveRef(fn_decl.return_ty));

    const function_ty = try Type.Function.init(analyzer.gpa, return_ty, param_tys);

    const mir_index = @intCast(u32, analyzer.mg.mir.items.len);
    const proto = try b.addInst(.{
        .tag = .proto,
        .data = .{ .ty_pl = .{ .ty = try b.typeToRef(function_ty), .pl = mir_index } },
    });
    _ = try block.addInst(.{
        .tag = .function,
        .data = .{ .bin_pl = .{ .l = proto, .r = block_index } },
    });

    return Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = analyzer.interner,
    };
}

pub fn analyzeBlock(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!u32 {
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
            .call => try analyzer.call(block, index),
            .dbg_value => try analyzer.dbgValue(block, index),
            .block => ref: {
                var inner = Block {
                    .parent = block,
                    .analyzer = analyzer,
                    .instructions = .{},
                };
                const block_index = try analyzer.analyzeBlock(&inner, index);
                try block.instructions.append(analyzer.gpa, block_index);
                break :ref Mir.indexToRef(block_index);
            },
            else => Mir.indexToRef(0),
        };
        analyzer.map.putAssumeCapacity(index, ref);
    }
    for (hir_insts) |index| analyzer.map.remove(index);

    const index = try addBlock(block);
    return index;
}

fn getTempMir(analyzer: *Analyzer) Mir {
    return Mir {
        .insts = analyzer.instructions.slice(),
        .extra = analyzer.extra.items,
        .values = analyzer.values.items,
        .interner = analyzer.interner,
    };
}

pub fn resolveTy(analyzer: *Analyzer, b: *Block, ref: Mir.Ref) !Type {
    _ = b;
    return analyzer.getTempMir().resolveTy(ref);
}

pub inline fn refToInt(analyzer: *Analyzer, ref: Mir.Ref) u64 {
    return analyzer.getTempMir().refToInt(ref);
}

pub inline fn refToFloat(analyzer: *Analyzer, ref: Mir.Ref) f64 {
    return analyzer.getTempMir().refToFloat(ref);
}

pub fn param(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const data = analyzer.hir.extraData(pl, Hir.Inst.Param);
    // TODO: proper ref to type
    const ty = switch (data.ty) {
        .void_ty => Type.initVoid(),
        .bool_ty => Type.initInt(1, false),
        .i8_ty => Type.initInt(8, true),
        .u8_ty => Type.initInt(8, false),
        .i16_ty => Type.initInt(16, true),
        .u16_ty => Type.initInt(16, false),
        .i32_ty => Type.initInt(32, true),
        .u32_ty => Type.initInt(32, false),
        .i64_ty => Type.initInt(64, true),
        .u64_ty => Type.initInt(64, false),
        .f32_ty => Type.initFloat(32),
        .f64_ty => Type.initFloat(64),
        else => return error.TypeError,
    };
    const index = try b.addInst(.{
        .tag = .param,
        .data = .{ .ty_pl = .{ .ty = try b.typeToRef(ty), .pl = data.name } },
    });
    return Mir.indexToRef(index);
}

pub fn integer(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addIntConstant(Type.initComptimeInt(false), data.int);
    return Mir.indexToRef(index);
}

fn float(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const index = try b.addFloatConstant(Type.initComptimeFloat(), data.float);
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
    const mir = analyzer.getTempMir();

    const src = analyzer.map.resolveRef(validate_ty.ref);
    const dest_ty = mir.refToType(analyzer.map.resolveRef(validate_ty.ty));
    return coercion.coerce(analyzer, b, src, dest_ty);
}

fn binaryArithOp(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);

    if (lty.kind() == .comptime_uint or lty.kind() == .comptime_sint) {
        if (rty.kind() == .comptime_uint or rty.kind() == .comptime_sint) {
            return analyzer.analyzeComptimeArithmetic(b, inst);
        }
    }

    const dest_ty = try coercion.binaryCoerceTo(lty, rty);
    const lval = try coercion.coerce(analyzer, b, lref, dest_ty);
    const rval = try coercion.coerce(analyzer, b, rref, dest_ty);

    const index = try b.addInst(.{
        .tag = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .add => .add,
            .sub => .sub,
            .mul => .mul,
            .div => .div,
            .mod => .mod,
            else => return error.NotImplemented,
        },
        .data = .{ .bin_op = .{ .lref = lval, .rref = rval } },
    });
    return Mir.indexToRef(index);
}

fn binaryCmp(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);
    const lkind = lty.kind();
    const rkind = rty.kind();

    // for now, we are using c++ semantics: if either is unsigned
    // everything is treated as unsigned
    if (lkind == .comptime_sint and rkind == .comptime_sint) {
        // only case where we do signed comparison
        const lval: i64 = @bitCast(i64, analyzer.refToInt(lref));
        const rval: i64 = @bitCast(i64, analyzer.refToInt(rref));
        const result = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .cmp_eq => lval == rval,
            .cmp_ne => lval != rval,
            .cmp_le => lval <= rval,
            .cmp_ge => lval >= rval,
            .cmp_lt => lval < rval,
            .cmp_gt => lval > rval,
            else => unreachable,
        };
        return addUnsignedValue(b, @boolToInt(result));
    } else if ((lkind == .comptime_uint or lkind == .comptime_sint) and (rkind == .comptime_uint or rkind == .comptime_sint)) {
        const lval: u64 = analyzer.refToInt(lref);
        const rval: u64 = analyzer.refToInt(rref);
        const result = switch (analyzer.hir.insts.items(.tag)[inst]) {
            .cmp_eq => lval == rval,
            .cmp_ne => lval != rval,
            .cmp_le => lval <= rval,
            .cmp_ge => lval >= rval,
            .cmp_lt => lval < rval,
            .cmp_gt => lval > rval,
            else => unreachable,
        };
        return addUnsignedValue(b, @boolToInt(result));
    } else if (lkind == .comptime_float and rkind == .comptime_float) {
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
        return addUnsignedValue(b, @boolToInt(result));
    }

    if ((lkind == .comptime_float or lkind == .float) and (rkind == .comptime_float or rkind == .float)) {
        const lwidth = lty.basic.width;
        const rwidth = rty.basic.width;
        const dest_ty = Type.initFloat(std.math.max(lwidth, rwidth));
        const lval = try coercion.coerce(analyzer, b, lref, dest_ty);
        const rval = try coercion.coerce(analyzer, b, rref, dest_ty);
        const hir_tag = analyzer.hir.insts.items(.tag)[inst];

        const tag: Mir.Inst.Tag = switch (hir_tag) {
            .cmp_eq => .cmp_eq,
            .cmp_ne => .cmp_ne,
            .cmp_le => .cmp_fle,
            .cmp_ge => .cmp_fge,
            .cmp_lt => .cmp_flt,
            .cmp_gt => .cmp_fgt,
            else => unreachable,
        };
        const index = try b.addInst(.{
            .tag = tag,
            .data = .{ .bin_op = .{ .lref = lval, .rref = rval } },
        });
        return Mir.indexToRef(index);
    } else if (lty.isInt() and rty.isInt()) {
        const lsign = lty.intSign();
        const rsign = rty.intSign();
        const vals = if (lty.isComptimeInt()) vals: {
            const rwidth = rty.basic.width;
            const lval = try coercion.coerce(analyzer, b, lref, Type.initInt(rwidth, lsign));
            const rval = rref;
            break :vals .{ lval, rval };
        } else if (rty.isComptimeInt()) vals: {
            const lwidth = lty.basic.width;
            const lval = lref;
            const rval = try coercion.coerce(analyzer, b, rref, Type.initInt(lwidth, rsign));
            break :vals .{ lval, rval };
        } else vals: {
            const lwidth = lty.basic.width;
            const rwidth = rty.basic.width;
            const maxwidth = std.math.max(lwidth, rwidth);
            const lval = try coercion.coerce(analyzer, b, lref, Type.initInt(maxwidth, lsign));
            const rval = try coercion.coerce(analyzer, b, rref, Type.initInt(maxwidth, rsign));
            break :vals .{ lval, rval };
        };
        const lval = vals[0];
        const rval = vals[1];

        const hir_tag = analyzer.hir.insts.items(.tag)[inst];
        const tag: Mir.Inst.Tag = if (lsign and rsign) switch (hir_tag) {
            .cmp_eq => .cmp_eq,
            .cmp_ne => .cmp_ne,
            .cmp_le => .cmp_sle,
            .cmp_ge => .cmp_sge,
            .cmp_lt => .cmp_slt,
            .cmp_gt => .cmp_sgt,
            else => unreachable,
        } else switch (hir_tag) {
            .cmp_eq => .cmp_eq,
            .cmp_ne => .cmp_ne,
            .cmp_le => .cmp_ule,
            .cmp_ge => .cmp_uge,
            .cmp_lt => .cmp_ult,
            .cmp_gt => .cmp_ugt,
            else => unreachable,
        };
        const index = try b.addInst(.{
            .tag = tag,
            .data = .{ .bin_op = .{ .lref = lval, .rref = rval } },
        });
        return Mir.indexToRef(index);
    } else return error.TypeMismatch;
}


fn addUnsignedValue(b: *Block, val: u64) !Mir.Ref {
    switch (val) {
        0 => return Mir.Ref.zero_val,
        1 => return Mir.Ref.one_val,
        else => {
            const index = try b.addIntConstant(Type.initComptimeInt(false), val);
            return Mir.indexToRef(index);
        }
    }
}

fn addSignedValue(b: *Block, val: u64) !Mir.Ref {
    switch (val) {
        0 => return Mir.Ref.zero_val,
        1 => return Mir.Ref.one_val,
        else => {
            const sign = alu.isNegative(val);
            const index = try b.addIntConstant(Type.initComptimeInt(sign), val);
            return Mir.indexToRef(index);
        }
    }
}

// this function performs simple constant folding of compile time arithmetic.
// this is necessary because constants produce `comptime_int` and `comptime_float`
// values until explicitly coerced into fixed width types. arithmetic performed inline
// before this coercion happens must be performed at compile time.
//
// this is performed by emulating the bitwise computation of each operation
// on a u64 value and reporting overflows. eventually, bigints will be supported
fn analyzeComptimeArithmetic(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = analyzer.map.resolveRef(binary.lref);
    const rref = analyzer.map.resolveRef(binary.rref);
    const lty = try analyzer.resolveTy(b, lref);
    const rty = try analyzer.resolveTy(b, rref);

    // std.debug.assert(lty.isTag());
    // std.debug.assert(rty.isTag());

    const lval = analyzer.refToInt(lref);
    const rval = analyzer.refToInt(rref);
    const lsign = lty.intSign();
    const rsign = rty.intSign();

    // if both values are unsigned, we perform unsigned operations
    // otherwise signed. TODO: revise this?
    const result = switch (analyzer.hir.insts.items(.tag)[inst]) {
        .add => if (!lsign and !rsign) try alu.uadd(lval, rval) else try alu.sadd(lval, rval),
        .sub => try alu.sadd(lval, alu.negate(rval)),
        .mul => if (!lsign and !rsign) try alu.umul(lval, rval) else try alu.smul(lval, rval),
        .div => if (!lsign and !rsign) try alu.udiv(lval, rval) else try alu.sdiv(lval, rval),
        .mod => return error.NotImplemented,
        .lsl => alu.lsl(lval, rval),
        .lsr => try alu.lsr(lval, rval),
        .asl => if (rsign) try alu.asr(lval, alu.negate(rval)) else try alu.asl(lval, rval),
        .asr => if (rsign) try alu.asl(lval, alu.negate(rval)) else try alu.asr(lval, rval),
        else => unreachable,
    };

    return switch (analyzer.hir.insts.items(.tag)[inst]) {
        .add,
        .mul,
        .div => if (!lsign and !rsign) try addUnsignedValue(b, result)
                else try addSignedValue(b, result),
        .sub => try addSignedValue(b, result),
        .lsl, .lsr => try addUnsignedValue(b, result),
        .asl,
        .asr => if (!lsign) try addUnsignedValue(b, result) else try addSignedValue(b, result),
        .mod => return error.NotImplemented,
        else => unreachable,
    };
}

fn emitUserError(analyzer: *Analyzer, node: u32, tag: Mir.UserError.Tag) !void {
    try analyzer.errors.append(analyzer.gpa, .{
        .node = node,
        .tag = tag,
    });
    return error.CodeError;
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

    // TODO: switch this to a bin_pl?
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

    const mir_index = @intCast(u32, parent.mg.mir.items.len);
    const mir = try analyzer.analyzeFunction(b, inst);
    try parent.mg.mir.append(parent.gpa, mir);

    return Mir.indexToRef(mir_index);
}

fn call(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const call_data = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Call);
    
    const scratch_top = analyzer.scratch.items.len;
    defer analyzer.scratch.shrinkRetainingCapacity(scratch_top);

    const addr = analyzer.map.resolveRef(call_data.addr);
    var arg_index: u32 = 0;
    while (arg_index < call_data.args_len) : (arg_index += 1) {
        const arg = analyzer.hir.extra_data[data.pl_node.pl + 2 + arg_index];
        const ref = analyzer.map.resolveRef(@intToEnum(Hir.Ref, arg));
        try analyzer.scratch.append(analyzer.arena, @enumToInt(ref));
    }

    const args = analyzer.scratch.items[scratch_top..];
    const extra_data = try analyzer.addExtra(Mir.Inst.Call {
        .args_len = @intCast(u32, args.len),
    });
    try analyzer.extra.appendSlice(analyzer.gpa, args);

    const index = try b.addInst(.{
        .tag = .call,
        .data = .{ .op_pl = .{ .op = addr, .pl = extra_data } },
    });
    return Mir.indexToRef(index);
}

fn dbgValue(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const dbg_value = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.DebugValue);

    const ref = analyzer.map.resolveRef(dbg_value.value);
    const index = try b.addInst(.{
        .tag = .dbg_value,
        .data = .{ .op_pl = .{ .op = ref, .pl = dbg_value.name } },
    });
    return Mir.indexToRef(index);
}
