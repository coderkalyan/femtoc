const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Type = @import("typing.zig").Type;
const Value = @import("value.zig").Value;
const Compilation = @import("Compilation.zig");
const Interner = @import("interner.zig").Interner;
const alu = @import("alu.zig");
const coercion = @import("coercion.zig");
const render = @import("render.zig");

const Allocator = std.mem.Allocator;
const Analyzer = @This();
const Decl = Compilation.Decl;

const Error = Allocator.Error || Mir.Error || coercion.Error || std.os.WriteError || @import("interner.zig").Error || error{
    InvalidRef,
    TypeError,
    TypeMismatch,
    InvalidOperation,
    CodeError,
    Overflow,
    DivZero,
    UnspecificType,
    RefNotFound,
};

comp: *Compilation,
hir: *const Hir,
map: std.AutoHashMapUnmanaged(Hir.Index, Mir.Ref),
instructions: std.MultiArrayList(Mir.Inst),
extra: std.ArrayListUnmanaged(u32),
values: std.ArrayListUnmanaged(Value),
scratch: std.ArrayListUnmanaged(u32),
errors: std.MultiArrayList(Mir.UserError),
interner: *const Interner,
gpa: Allocator,
arena: Allocator,

pub const Block = struct {
    parent: ?*Block,
    analyzer: *Analyzer,
    instructions: std.ArrayListUnmanaged(Mir.Index),

    pub fn addInst(b: *Block, inst: Mir.Inst) !Mir.Index {
        const analyzer = b.analyzer;
        const index: Mir.Index = @intCast(analyzer.instructions.len);
        try analyzer.instructions.append(analyzer.gpa, inst);
        try b.instructions.append(analyzer.arena, index);
        return index;
    }

    pub fn addValue(b: *Block, val: Value) !u32 {
        const analyzer = b.analyzer;
        const len: u32 = @intCast(analyzer.values.items.len);
        try analyzer.values.append(analyzer.gpa, val);
        return len;
    }

    pub fn addConstant(b: *Block, ty: Type, val: Value) !u32 {
        const value = try b.addValue(val);
        const index = try b.addInst(.{ .tag = .constant, .data = .{ .ty_pl = .{
            .ty = try b.typeToRef(ty),
            .pl = value,
        } } });
        return index;
    }

    pub fn addIntConstant(b: *Block, ty: Type, int: u64) !u32 {
        std.debug.assert(ty.basic.width <= 64);
        if (int == 0) return b.addConstant(ty, .{ .tag = .zero });
        if (int == 1) return b.addConstant(ty, .{ .tag = .one });
        if (ty.basic.kind == .comptime_uint or ty.basic.kind == .comptime_sint) {
            if (int <= std.math.maxInt(u32)) {
                var payload = try b.analyzer.gpa.create(Value.Payload.U32);
                payload.* = .{ .int = @truncate(int) };
                return b.addConstant(ty, .{ .payload = &payload.base });
            } else {
                var payload = try b.analyzer.gpa.create(Value.Payload.U64);
                payload.* = .{ .int = int };
                return b.addConstant(ty, .{ .payload = &payload.base });
            }
        } else if (ty.basic.width <= 32) {
            var payload = try b.analyzer.gpa.create(Value.Payload.U32);
            payload.* = .{ .int = @truncate(int) };
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

    // turns a type into a reference to be passed into an instruction
    // for many basic types, there are built in refs
    // special, derived, and aggregate types insert a type
    pub fn typeToRef(b: *Block, ty: Type) !Mir.Ref {
        return switch (ty.kind()) {
            .void => .void,
            .uint => switch (ty.basic.width) {
                1 => .u1,
                8 => .u8,
                16 => .u16,
                32 => .u32,
                64 => .u64,
                else => Mir.indexToRef(try b.addType(ty)),
            },
            .sint => switch (ty.basic.width) {
                8 => .i8,
                16 => .i16,
                32 => .i32,
                64 => .i64,
                else => Mir.indexToRef(try b.addType(ty)),
            },
            .float => switch (ty.basic.width) {
                32 => .f32,
                64 => .f64,
                else => error.NotImplemented,
            },
            .comptime_uint => .comptime_uint,
            .comptime_sint => .comptime_sint,
            .comptime_float => .comptime_float,
            else => return Mir.indexToRef(try b.addType(ty)),
        };
    }

    pub fn addParam(b: *Block, ty: Mir.Ref, name: u32) !Mir.Index {
        return b.addInst(.{
            .tag = .param,
            .data = .{ .ty_pl = .{ .ty = ty, .pl = name } },
        });
    }

    pub fn addStore(b: *Block, addr: Mir.Ref, val: Mir.Ref) !Mir.Index {
        return b.addInst(.{
            .tag = .store,
            .data = .{ .bin_op = .{ .lref = addr, .rref = val } },
        });
    }
};

fn resolveInst(analyzer: *Analyzer, b: *Block, hir_index: Hir.Index) !Mir.Ref {
    if (analyzer.map.get(hir_index)) |mapping| {
        // local ref
        return mapping;
    } else if (analyzer.comp.globals.get(hir_index)) |decl_index| {
        // global ref
        const inst = try b.addInst(.{
            .tag = .load_decl,
            .data = .{ .pl = decl_index },
        });
        return Mir.indexToRef(inst);
    } else {
        std.debug.print("unable to find index={}\n", .{hir_index});
        return error.RefNotFound;
    }
}

fn resolveRef(analyzer: *Analyzer, b: *Block, ref: Hir.Ref) !Mir.Ref {
    return switch (ref) {
        .zero_val => .zero_val,
        .one_val => .one_val,
        .void_val => .void_val,
        .btrue_val => .one_val,
        .bfalse_val => .zero_val,
        .u8_ty => .u8,
        .u16_ty => .u16,
        .u32_ty => .u32,
        .u64_ty => .u64,
        .i8_ty => .i8,
        .i16_ty => .i16,
        .i32_ty => .i32,
        .i64_ty => .i64,
        .f32_ty => .f32,
        .f64_ty => .f64,
        .bool_ty => .u1,
        .void_ty => .void,
        else => {
            const hir_index = Hir.Inst.refToIndex(ref).?;
            return analyzer.resolveInst(b, hir_index);
        },
    };
}

pub fn addBlock(block: *Block) !u32 {
    const analyzer = block.analyzer;
    const insts = block.instructions.items;
    const index = try analyzer.addExtra(Mir.Inst.Block{
        .insts_len = @intCast(insts.len),
    });
    try analyzer.extra.appendSlice(analyzer.gpa, insts);

    return block.addInst(.{
        .tag = .block,
        .data = .{ .pl = index },
    });
}

pub fn analyzeBlock(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!u32 {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const block_data = analyzer.hir.extraData(pl, Hir.Inst.Block);

    const block = b;

    var inst_counter: u32 = 0;
    var extra_index: u32 = pl + 2 + (2 * block_data.head);
    // const hir_insts = analyzer.hir.extra_data[pl + 1 .. pl + 1 + block_data.len];
    // for (hir_insts) |index| {
    while (inst_counter < block_data.len) : (inst_counter += 1) {
        const index = analyzer.hir.extra_data[extra_index];
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .push => try analyzer.alloc(block, index),
            .load => try analyzer.load(block, index),
            .load_inline => try analyzer.loadInline(block, index),
            .store => try analyzer.store(block, index),
            .coerce => try analyzer.coerce(block, index),
            .add, .sub, .mul, .div, .mod => try analyzer.binaryArithOp(block, index),
            .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge, .cmp_lt, .cmp_gt => try analyzer.binaryCmp(block, index),
            .ret_node => try analyzer.retNode(block, index),
            .ret_implicit => try analyzer.retImplicit(block, index),
            .yield_node => try analyzer.yieldNode(block, index),
            .yield_implicit => try analyzer.yieldImplicit(block, index),
            .branch_single => try analyzer.branchSingle(block, index),
            .branch_double => try analyzer.branchDouble(block, index),
            .loop => try analyzer.loop(block, index),
            // TODO: support nested functions (not much work but need to generate decls)
            // .fn_decl => try analyzer.fnDecl(block, index),
            .call => try analyzer.call(block, index),
            .dbg_value => try analyzer.dbgValue(block, index),
            .block => ref: {
                var inner = Block{
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
        try analyzer.map.put(analyzer.gpa, index, ref);
        if (inst_counter < block_data.len - 1) {
            extra_index = pl + 2 + (2 * analyzer.hir.extra_data[extra_index + 1]);
        }
    }
    // TODO: remove things from map

    return try addBlock(block);
}

pub fn analyzeInlineBlock(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const pl = analyzer.hir.insts.items(.data)[inst].pl_node.pl;
    const block_data = analyzer.hir.extraData(pl, Hir.Inst.Block);

    const block = b;
    var yield_ref: ?Mir.Ref = null;

    var inst_counter: u32 = 0;
    var extra_index: u32 = pl + 2 + (2 * block_data.head);
    // const hir_insts = analyzer.hir.extra_data[pl + 1 .. pl + 1 + block_data.len];
    // for (hir_insts) |index| {
    while (inst_counter < block_data.len) : (inst_counter += 1) {
        const index = analyzer.hir.extra_data[extra_index];
        const ref = switch (analyzer.hir.insts.items(.tag)[index]) {
            .int => try analyzer.integer(block, index),
            .float => try analyzer.float(block, index),
            .decl_const => try analyzer.declConst(block, index),
            .decl_mut => try analyzer.declMut(block, index),
            .decl_export => try analyzer.declExport(block, index),
            .load_inline => try analyzer.loadInline(block, index),
            .coerce => try analyzer.coerce(block, index),
            .add, .sub, .mul, .div, .mod => try analyzer.binaryArithOp(block, index),
            .cmp_eq, .cmp_ne, .cmp_le, .cmp_ge, .cmp_lt, .cmp_gt => try analyzer.binaryCmp(block, index),
            .fn_decl => try analyzer.fnProto(block, index),
            .yield_inline => {
                yield_ref = try analyzer.yieldInline(block, index);
                break;
            },
            else => unreachable,
        };
        try analyzer.map.put(analyzer.gpa, index, ref);
        if (inst_counter < block_data.len - 1) {
            extra_index = pl + 2 + (2 * analyzer.hir.extra_data[extra_index + 1]);
        }
    }

    // TODO: change if using inline block for non-globals
    const decl_inst = Mir.refToIndex(yield_ref.?).?;
    const decl_index = analyzer.instructions.items(.data)[decl_inst].pl;
    try analyzer.comp.globals.put(analyzer.gpa, inst, decl_index);

    inst_counter = 0;
    extra_index = pl + 2 + (2 * block_data.head);
    // for (hir_insts) |index| {
    while (inst_counter < block_data.len) : (inst_counter += 1) {
        const index = analyzer.hir.extra_data[extra_index];
        switch (analyzer.hir.insts.items(.tag)[index]) {
            .fn_decl => try analyzer.fnBody(block, index),
            else => {},
        }
        if (inst_counter < block_data.len - 1) {
            extra_index = pl + 2 + (2 * analyzer.hir.extra_data[extra_index + 1]);
        }
    }

    // TODO: remove things from map
    return yield_ref.?;
}

fn getTempMir(analyzer: *Analyzer) Mir {
    return Mir{
        .insts = analyzer.instructions.slice(),
        .extra = analyzer.extra.items,
        .values = analyzer.values.items,
        .interner = analyzer.interner,
        .comp = analyzer.comp,
    };
}

pub inline fn resolveType(analyzer: *Analyzer, b: *Block, ref: Mir.Ref) !Type {
    _ = b; // TODO: remove this
    return analyzer.getTempMir().resolveType(ref);
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
    // TODO: make sure ref is a type
    const ty = try analyzer.resolveRef(b, data.ty);
    return Mir.indexToRef(try b.addParam(ty, data.name));
}

fn coerce(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const coerce_data = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Coerce);
    const mir = analyzer.getTempMir();

    const src = try analyzer.resolveRef(b, coerce_data.val);
    const dest_ty = mir.resolveType(try analyzer.resolveRef(b, coerce_data.ty));
    return coercion.coerce(analyzer, b, src, dest_ty);
}

fn binaryArithOp(analyzer: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const binary = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Binary);
    const lref = try analyzer.resolveRef(b, binary.lref);
    const rref = try analyzer.resolveRef(b, binary.rref);
    const lty = try analyzer.resolveType(b, lref);
    const rty = try analyzer.resolveType(b, rref);

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
    const lref = try analyzer.resolveRef(b, binary.lref);
    const rref = try analyzer.resolveRef(b, binary.rref);
    const lty = try analyzer.resolveType(b, lref);
    const rty = try analyzer.resolveType(b, rref);

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
        .add, .mul, .div => if (!lsign and !rsign) try addUnsignedValue(b, result) else try addSignedValue(b, result),
        .sub => try addSignedValue(b, result),
        .lsl, .lsr => try addUnsignedValue(b, result),
        .asl, .asr => if (!lsign) try addUnsignedValue(b, result) else try addSignedValue(b, result),
        .mod => return error.NotImplemented,
        else => unreachable,
    };
}

fn fnProto(analyzer: *Analyzer, b: *Block, function_inst: Hir.Index) !Mir.Ref {
    const hir = analyzer.hir;
    const pl = hir.insts.items(.data)[function_inst].pl_node.pl;
    const fn_decl = hir.extraData(pl, Hir.Inst.FnDecl);
    const comp = analyzer.comp;

    const name = name: {
        var hash: [16]u8 = undefined;
        _ = try std.fmt.bufPrint(&hash, "{x}{x}", .{ fn_decl.hash_upper, fn_decl.hash_lower });
        break :name try std.mem.joinZ(analyzer.gpa, "_", &[_][]const u8{ "f", &hash });
    };

    const fn_type = try analyzer.initFunctionType(b, function_inst);

    const decl_index = try comp.allocateDecl();
    const decl_ptr = comp.declPtr(decl_index);

    const function_decl = try analyzer.gpa.create(Decl.Function);
    function_decl.* = .{ .decl = decl_index, .hir_inst = function_inst };
    const function_val = try analyzer.gpa.create(Value.Payload.Function);
    function_val.* = .{ .func = function_decl };

    decl_ptr.* = Decl{
        .name = name.ptr,
        .ty = fn_type,
        .val = .{ .payload = &function_val.base },
        .mut = false,
    };
    try comp.backend.updateDecl(decl_index);

    const load_decl = try b.addInst(.{
        .tag = .load_decl,
        .data = .{ .pl = decl_index },
    });
    return Mir.indexToRef(load_decl);
}

fn fnBody(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !void {
    const hir = analyzer.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const fn_decl = hir.extraData(pl, Hir.Inst.FnDecl);
    const comp = analyzer.comp;

    // const name = name: {
    //     var hash: [16]u8 = undefined;
    //     _ = try std.fmt.bufPrint(&hash, "{x}{x}", .{fn_decl.hash_upper, fn_decl.hash_lower});
    //     break :name try std.mem.joinZ(analyzer.gpa, "_", &[_][]const u8{ "f", &hash });
    // };

    // // this needs to be run before fnBody
    // const fn_type = try analyzer.initFunctionType(b, function_inst);

    // const decl_index = try comp.allocateDecl();
    const load_ref = try analyzer.resolveInst(b, inst);
    const load_inst = Mir.refToIndex(load_ref).?;
    const decl_index = analyzer.getTempMir().insts.items(.data)[load_inst].pl;
    const decl_ptr = comp.declPtr(decl_index);

    // const function_decl = try analyzer.gpa.create(Decl.Function);
    // function_decl.* = .{ .decl = decl_index, .hir_inst = function_inst };
    // const function_val = try analyzer.gpa.create(Value.Payload.Function);
    // function_val.* = .{ .func = function_decl };

    // decl_ptr.* = Decl {
    //     .name = name.ptr,
    //     .ty = fn_type,
    //     .val = .{ .payload = &function_val.base },
    // };
    // try comp.backend.updateDecl(decl_ptr);

    const mir = try analyzer.fnBodyInner(&fn_decl);
    if (comp.config.verbose_mir) {
        const out = std.io.getStdOut();
        var buffered_out = std.io.bufferedWriter(out.writer());
        var writer = buffered_out.writer();
        var mir_renderer = render.MirRenderer(2, @TypeOf(writer)).init(writer, &mir);
        try mir_renderer.render();
        try buffered_out.flush();
    }

    try comp.backend.generateBody(decl_ptr, &mir);

    // const load_decl = try b.addInst(.{
    //     .tag = .load_decl,
    //     .data = .{ .pl = decl_index },
    // });
    // return Mir.indexToRef(load_decl);
}

fn fnBodyInner(a: *Analyzer, fn_decl: *const Hir.Inst.FnDecl) !Mir {
    var arena = std.heap.ArenaAllocator.init(a.gpa);
    defer arena.deinit();

    var analyzer = Analyzer{
        .comp = a.comp,
        .gpa = a.gpa,
        .arena = arena.allocator(),
        .map = .{},
        .hir = a.hir,
        .instructions = .{},
        .extra = .{},
        .values = .{},
        .scratch = .{},
        .errors = .{},
        .interner = &a.hir.interner,
    };
    var block = Analyzer.Block{
        .parent = null,
        .analyzer = &analyzer,
        .instructions = .{},
    };

    // prepend params before analyzing the block
    var params = analyzer.hir.extra_data[fn_decl.params_start..fn_decl.params_end];
    for (params) |inst| {
        const ref = try analyzer.param(&block, inst);
        try analyzer.map.put(analyzer.gpa, inst, ref); // TODO: gpa or arena?
    }

    _ = try analyzer.analyzeBlock(&block, fn_decl.body);
    const mir = Mir{
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = try analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = try analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = &a.hir.interner,
        .comp = analyzer.comp,
    };
    return mir;
}

fn declConst(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const comp = analyzer.comp;
    const data = analyzer.hir.insts.items(.data)[inst].un_node;
    const mir = analyzer.getTempMir();

    const decl_index = try comp.allocateDecl();
    const decl_ptr = comp.declPtr(decl_index);
    const op = try analyzer.resolveRef(b, data.operand);
    decl_ptr.* = Decl{
        .name = "anon", // TODO: hash
        .ty = mir.resolveType(op),
        .val = mir.resolveValue(Mir.refToIndex(op).?),
        .mut = false,
    };
    try comp.backend.updateDecl(decl_index);

    const load_decl = try b.addInst(.{
        .tag = .load_decl,
        .data = .{ .pl = decl_index },
    });
    return Mir.indexToRef(load_decl);
}

fn declMut(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const comp = analyzer.comp;
    const data = analyzer.hir.insts.items(.data)[inst].un_node;
    const mir = analyzer.getTempMir();

    const decl_index = try comp.allocateDecl();
    const decl_ptr = comp.declPtr(decl_index);
    const op = try analyzer.resolveRef(b, data.operand);
    decl_ptr.* = Decl{
        .name = "anon", // TODO: hash
        .ty = mir.resolveType(op),
        .val = mir.resolveValue(Mir.refToIndex(op).?),
        .mut = true,
    };
    try comp.backend.updateDecl(decl_index);

    const load_decl = try b.addInst(.{
        .tag = .load_decl,
        .data = .{ .pl = decl_index },
    });
    return Mir.indexToRef(load_decl);
}

fn declExport(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const comp = analyzer.comp;
    const data = analyzer.hir.insts.items(.data)[inst].un_node;

    const ref = try analyzer.resolveRef(b, data.operand);
    const decl_index = Mir.refToIndex(ref).?;
    try comp.export_decls.put(comp.gpa, decl_index, {});
    try comp.backend.updateDecl(decl_index);

    return ref;
}
// fn fnDecl(parent: *Analyzer, b: *Block, inst: Hir.Index) Error!Mir.Ref {
//     if (true) unreachable;
//     var arena = std.heap.ArenaAllocator.init(parent.gpa);
//     defer arena.deinit();

//     var analyzer = Analyzer {
//         .comp = parent.comp,
//         // .mg = mg,
//         // .map = MirMap.init(&mg.map),
//         .map = .{},
//         .hir = parent.hir,
//         .gpa = parent.gpa,
//         .arena = arena.allocator(),
//         .instructions = .{},
//         .extra = .{},
//         .values = .{},
//         .scratch = .{},
//         .errors = .{},
//         .interner = parent.interner,
//     };

//     const mir_index = @intCast(u32, 0); //parent.comp.mir.items.len);
//     const mir = try analyzer.analyzeFunction(b, inst);
//     try parent.mg.mir.append(parent.gpa, mir);

//     return Mir.indexToRef(mir_index);
// }

fn call(analyzer: *Analyzer, b: *Block, inst: Hir.Index) !Mir.Ref {
    const data = analyzer.hir.insts.items(.data)[inst];
    const call_data = analyzer.hir.extraData(data.pl_node.pl, Hir.Inst.Call);

    const scratch_top = analyzer.scratch.items.len;
    defer analyzer.scratch.shrinkRetainingCapacity(scratch_top);

    const addr = try analyzer.resolveRef(b, call_data.addr);
    const function_type = (try analyzer.resolveType(b, addr)).extended.cast(Type.Function).?;
    const base = data.pl_node.pl + 2;
    const hir_args = analyzer.hir.extra_data[base .. base + call_data.args_len];
    for (hir_args, 0..) |arg, i| {
        const ref = try analyzer.resolveRef(b, @enumFromInt(arg));
        const res = try coercion.coerce(analyzer, b, ref, function_type.param_types[i]);
        try analyzer.scratch.append(analyzer.arena, @intFromEnum(res));
    }

    const args = analyzer.scratch.items[scratch_top..];
    const extra_data = try analyzer.addExtra(Mir.Inst.Call{
        .args_len = @intCast(args.len),
    });
    try analyzer.extra.appendSlice(analyzer.gpa, args);

    const index = try b.addInst(.{
        .tag = .call,
        .data = .{ .op_pl = .{ .op = addr, .pl = extra_data } },
    });
    return Mir.indexToRef(index);
}
