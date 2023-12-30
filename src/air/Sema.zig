const std = @import("std");
const Fir = @import("../fir/Fir.zig");
const Air = @import("Air.zig");
const Module = @import("Module.zig");
const InternPool = @import("../InternPool.zig");
const Coercion = @import("Coercion.zig");
const Type = @import("type.zig").Type;
const Decl = Air.Decl;
const Allocator = std.mem.Allocator;

const Sema = @This();
const GlobalMap = std.AutoHashMap(InternPool.StringIndex, InternPool.Index);

gpa: Allocator,
arena: Allocator,
fir: *const Fir,
pool: *InternPool,
insts: std.MultiArrayList(Air.Inst),
extra: std.ArrayListUnmanaged(u32),
// map: std.AutoHashMapUnmanaged(Fir.Index, Air.Ref),
inst_map: std.AutoHashMapUnmanaged(Fir.Index, Air.Index),
ip_map: std.AutoHashMapUnmanaged(Fir.Index, InternPool.Index),
return_type: ?InternPool.Index,
scratch: std.ArrayListUnmanaged(u32),
globals: *GlobalMap,
function: ?struct {
    inst: Fir.Index,
    body_index: InternPool.AirIndex,
},

pub const Block = struct {
    sema: *Sema,
    arena: Allocator,
    fir: *const Fir,
    insts: std.ArrayListUnmanaged(Air.Index),
    pool: *InternPool,
    is_comptime: bool = false,
    return_type: ?InternPool.Index,

    pub fn add(b: *Block, inst: Air.Inst) !Air.Index {
        const index = try b.sema.add(inst);
        try b.insts.append(b.arena, index);
        return index;
    }

    pub fn addConstant(b: *Block, key: InternPool.Key) !Air.Index {
        return b.sema.addConstant(key);
    }

    pub fn mapInst(b: *Block, fir_inst: Fir.Index, air_inst: Air.Index) !void {
        try b.sema.inst_map.put(b.arena, fir_inst, air_inst);
    }

    pub fn mapInterned(b: *Block, fir_inst: Fir.Index, ip_index: InternPool.Index) !void {
        try b.sema.ip_map.put(b.arena, fir_inst, ip_index);
    }

    pub fn resolveInst(b: *Block, fir_inst: Fir.Index) Air.Index {
        return b.sema.resolveInst(fir_inst);
    }

    pub fn resolveInterned(b: *Block, fir_inst: Fir.Index) InternPool.Index {
        return b.sema.resolveInterned(fir_inst);
    }
};

pub fn analyzeModule(gpa: Allocator, pool: *InternPool, fir: *const Fir) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var globals = GlobalMap.init(arena.allocator());
    defer globals.deinit();

    const module_pl = fir.insts.items(.data)[@intFromEnum(fir.module_index)].module.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, module_pl);
    const module_globals = fir.extraSlice(slice);

    // first pass - register all globals (forward declaration)
    for (module_globals) |global| {
        try registerGlobal(fir, pool, &globals, @enumFromInt(global));
    }

    // second pass - analyze the rest of the code
    for (module_globals) |global| {
        var sema: Sema = .{
            .gpa = gpa,
            .arena = arena.allocator(),
            .pool = pool,
            .fir = fir,
            .insts = .{},
            .extra = .{},
            .inst_map = .{},
            .ip_map = .{},
            .return_type = null,
            .scratch = .{},
            .globals = &globals,
            .function = null,
        };
        try sema.analyzeGlobal(@enumFromInt(global));

        if (sema.function) |_| {
            try sema.analyzeFunctionBody();
        }
    }
}

fn registerGlobal(fir: *const Fir, pool: *InternPool, globals: *GlobalMap, global_inst: Fir.Index) !void {
    const global = fir.insts.items(.data)[@intFromEnum(global_inst)].global;
    const decl_index = try pool.addOneDecl();
    const decl_ip_index = try pool.getOrPut(.{ .decl = decl_index });
    const decl = pool.decls.at(@intFromEnum(decl_index));
    decl.name = global.name;
    try globals.put(global.name, decl_ip_index);
}

fn analyzeGlobal(sema: *Sema, global_inst: Fir.Index) !void {
    const fir = sema.fir;
    const global = fir.insts.items(.data)[@intFromEnum(global_inst)].global;
    const block_pl = fir.insts.items(.data)[@intFromEnum(global.block)].block_inline.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);
    const decl_ip_index = sema.globals.get(global.name).?;

    var b: Block = .{
        .sema = sema,
        .arena = sema.arena,
        .fir = sema.fir,
        .insts = .{},
        .pool = sema.pool,
        .is_comptime = true,
        .return_type = sema.return_type,
    };

    for (insts) |i| {
        const inst: Fir.Index = @enumFromInt(i);
        switch (fir.insts.items(.tags)[i]) {
            .int => try integerLiteral(&b, inst),
            .float => try floatLiteral(&b, inst),
            .builtin_type => try builtinType(&b, inst),
            .bool_type => try boolType(&b, inst),
            .pointer_type => try pointerType(&b, inst),
            .many_pointer_type => try manyPointerType(&b, inst),
            .slice_type => try sliceType(&b, inst),
            .array_type => try arrayType(&b, inst),
            .function_type => try functionType(&b, inst),
            .type_of => try firTypeOf(&b, inst),
            .function => try firFunction(&b, inst),
            .global_handle => try globalHandle(&b, inst, decl_ip_index),
            .global_set_mutable => try globalSetMutable(&b, inst),
            .global_set_type => try globalSetType(&b, inst),
            .global_set_init => try globalSetInit(&b, inst),
            .global_set_linkage_external => try globalSetLinkageExternal(&b, inst),
            .yield_inline => break,
            else => |tag| {
                std.log.err("encountered invalid global instruction: {}\n", .{tag});
                unreachable;
            },
        }
    }
}

fn analyzeBodyBlock(sema: *Sema, block: Fir.Index) error{ NotImplemented, Truncated, InvalidCoercion, OutOfMemory, HandledUserError }!Air.Index {
    const fir = sema.fir;
    const block_pl = fir.insts.items(.data)[@intFromEnum(block)].block.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);

    var b: Block = .{
        .sema = sema,
        .arena = sema.arena,
        .fir = sema.fir,
        .pool = sema.pool,
        .insts = .{},
        .return_type = sema.return_type,
    };

    for (insts) |i| {
        const inst: Fir.Index = @enumFromInt(i);
        switch (fir.insts.items(.tags)[i]) {
            .int => try integerLiteral(&b, inst),
            .float => try floatLiteral(&b, inst),
            .none => try voidLiteral(&b, inst),
            .string => unreachable, // TODO
            .array => unreachable, // TODO
            .builtin_type => try builtinType(&b, inst),
            .bool_type => try boolType(&b, inst),
            .pointer_type => try pointerType(&b, inst),
            .many_pointer_type => try manyPointerType(&b, inst),
            .array_type => try arrayType(&b, inst),
            .slice_type => try sliceType(&b, inst),
            .function_type => try functionType(&b, inst),
            .return_type => try returnType(&b, inst),
            .param_type => try paramType(&b, inst),
            .element_type => try elementType(&b, inst),
            .type_of => try firTypeOf(&b, inst),

            inline .add,
            .sub,
            .mul,
            .div,
            .mod,
            .bitwise_or,
            .bitwise_and,
            .bitwise_xor,
            => |tag| try binaryArithOp(&b, inst, tag),
            .sl,
            .sr,
            => unreachable, // TODO
            inline .cmp_eq,
            .cmp_ne,
            .cmp_gt,
            .cmp_ge,
            .cmp_lt,
            .cmp_le,
            // => {}, // TODO
            => |tag| try binaryCmp(&b, inst, tag),
            .neg => try unaryNeg(&b, inst),
            .bitwise_inv => try bitwiseInv(&b, inst),

            .coerce => try firCoerce(&b, inst),
            .call => try call(&b, inst),
            .index_ref, .index_val => unreachable, // TODO
            .load_global => try loadGlobal(&b, inst),
            .push => try push(&b, inst),
            .load => try load(&b, inst),
            .store => try store(&b, inst),

            .branch_single => try branchSingle(&b, inst),
            .branch_double => try branchDouble(&b, inst),
            .loop => try loop(&b, inst),
            .return_node => try returnNode(&b, inst),
            .return_implicit => try returnImplicit(&b, inst),
            .yield_node => try yieldNode(&b, inst),
            .yield_implicit => try yieldImplicit(&b, inst),
            .yield_inline => unreachable,
            .@"break" => try firBreak(&b, inst),
            .@"continue" => try firContinue(&b, inst),
            .block => try firBlock(&b, inst),
            .block_inline => unreachable,

            .function => unreachable,
            .param => try firParam(&b, inst),
            .global_handle,
            .global_set_mutable,
            .global_set_type,
            .global_set_init,
            .global_set_linkage_external,
            .global,
            .module,
            => unreachable,
        }
    }

    const air_block = try sema.addBlockUnlinked(&b);
    try b.mapInst(block, air_block);
    return air_block;
}

pub fn addExtra(sema: *Sema, extra: anytype) !Air.ExtraIndex {
    const len: u32 = @intCast(sema.extra.items.len);
    const fields = std.meta.fields(@TypeOf(extra));
    try sema.extra.ensureUnusedCapacity(sema.gpa, fields.len);
    inline for (fields) |field| {
        switch (field.type) {
            inline else => {
                const num: u32 = @intFromEnum(@field(extra, field.name));
                sema.extra.appendAssumeCapacity(@bitCast(num));
            },
        }
    }
    return @enumFromInt(len);
}

pub fn extraData(sema: *Sema, comptime T: type, index: Air.ExtraIndex) T {
    var result: T = undefined;
    const fields = std.meta.fields(T);
    inline for (fields, 0..) |field, i| {
        switch (field.type) {
            inline else => @field(result, field.name) = @bitCast(sema.extra.items[index + i]),
        }
    }
    return result;
}

pub fn addSlice(sema: *Sema, slice: []const u32) !Air.ExtraIndex {
    const start: u32 = @intCast(sema.extra.items.len);
    try sema.extra.appendSlice(sema.gpa, slice);
    const end: u32 = @intCast(sema.extra.items.len);
    return sema.addExtra(Air.Inst.ExtraSlice{
        .start = @enumFromInt(start),
        .end = @enumFromInt(end),
    });
}

pub fn addBlockUnlinked(sema: *Sema, inner: *Block) !Air.Index {
    const scratch_top = sema.scratch.items.len;
    defer sema.scratch.shrinkRetainingCapacity(scratch_top);
    try sema.scratch.ensureUnusedCapacity(sema.arena, inner.insts.items.len);
    for (inner.insts.items) |inst| {
        sema.scratch.appendAssumeCapacity(@intFromEnum(inst));
    }
    const insts = sema.scratch.items[scratch_top..];
    const pl = try sema.addSlice(insts);
    return sema.add(.{ .block = .{ .insts = pl } });
}

fn resolveInst(sema: *Sema, fir_inst: Fir.Index) Air.Index {
    // TODO: globals
    return sema.inst_map.get(fir_inst).?;
}

fn resolveInterned(sema: *Sema, fir_inst: Fir.Index) InternPool.Index {
    // TODO: globals
    return sema.ip_map.get(fir_inst).?;
}

fn resolveDecl(sema: *Sema, inst: Air.Index) *Decl {
    std.debug.assert(sema.insts.items(.tags)[@intFromEnum(inst)] == .load_decl);
    const load_decl = sema.insts.items(.data)[@intFromEnum(inst)].load_decl;
    const decl_index = sema.pool.items.items(.data)[@intFromEnum(load_decl)];
    return sema.pool.decls.at(decl_index);
}

pub fn add(sema: *Sema, inst: Air.Inst) !Air.Index {
    const index: u32 = @intCast(sema.insts.len);
    try sema.insts.append(sema.gpa, inst);
    return @enumFromInt(index);
}

pub fn addConstant(sema: *Sema, key: InternPool.Key) !Air.Index {
    const ip_index = try sema.pool.getOrPut(key);
    return sema.add(.{ .constant = ip_index });
    // const raw: u32 = @intFromEnum(ip_index);
    // return @enumFromInt(raw & (1 << 31));
}

fn globalHandle(b: *Block, inst: Fir.Index, decl: InternPool.Index) !void {
    // const decl_index = b.pool.indexToKey(decl).decl;
    // const decl_data = b.pool.decls.at(@intFromEnum(decl_index));
    const air_inst = try b.add(.{ .load_decl = decl });
    try b.mapInst(inst, air_inst);
}

fn globalSetMutable(b: *Block, inst: Fir.Index) !void {
    const global_set_mutable = b.fir.get(inst);
    const decl = b.sema.resolveDecl(b.resolveInst(global_set_mutable.data.global_set_mutable));
    decl.mutable = true;
}

fn globalSetType(b: *Block, inst: Fir.Index) !void {
    const global_set_type = b.fir.get(inst);
    const data = global_set_type.data.global_set_type;
    const decl = b.sema.resolveDecl(b.resolveInst(data.handle));
    decl.ty = b.resolveInterned(data.ty);
}

fn globalSetInit(b: *Block, inst: Fir.Index) !void {
    const global_set_init = b.fir.get(inst);
    const data = global_set_init.data.global_set_init;
    const decl = b.sema.resolveDecl(b.resolveInst(data.handle));
    const air_inst = b.resolveInst(data.val);
    decl.initializer = b.sema.insts.items(.data)[@intFromEnum(air_inst)].constant;
}

fn globalSetLinkageExternal(b: *Block, inst: Fir.Index) !void {
    const global_set_linkage_external = b.fir.get(inst);
    const data = global_set_linkage_external.data.global_set_linkage_external;
    const decl = b.sema.resolveDecl(b.resolveInst(data));
    decl.linkage = .external;
}

fn integerLiteral(b: *Block, inst: Fir.Index) !void {
    const int = b.fir.get(inst);
    const air_inst = try b.addConstant(.{ .tv = .{
        .ty = try b.pool.getOrPut(.{ .ty = .{ .comptime_int = .{ .sign = .unsigned } } }),
        .val = .{ .integer = int.data.int },
    } });
    try b.mapInst(inst, air_inst);
}

fn floatLiteral(b: *Block, inst: Fir.Index) !void {
    const float = b.fir.get(inst);
    const air_inst = try b.addConstant(.{ .tv = .{
        .ty = try b.pool.getOrPut(.{ .ty = .{ .comptime_float = {} } }),
        .val = .{ .float = float.data.float },
    } });
    try b.mapInst(inst, air_inst);
}

fn voidLiteral(b: *Block, inst: Fir.Index) !void {
    const air_inst = try b.addConstant(.{ .tv = .{
        .ty = try b.pool.getOrPut(.{ .ty = .{ .void = {} } }),
        .val = .{ .none = {} },
    } });
    try b.mapInst(inst, air_inst);
}

const builtin_type_enum = enum {
    u8_type,
    u16_type,
    u32_type,
    u64_type,
    i8_type,
    i16_type,
    i32_type,
    i64_type,
    f32_type,
    f64_type,
    bool_type,
    void_type,
};

const builtin_types = std.ComptimeStringMap(builtin_type_enum, .{
    .{ "u8", .u8_type },
    .{ "u16", .u16_type },
    .{ "u32", .u32_type },
    .{ "u64", .u64_type },
    .{ "i8", .i8_type },
    .{ "i16", .i16_type },
    .{ "i32", .i32_type },
    .{ "i64", .i64_type },
    .{ "f32", .f32_type },
    .{ "f64", .f64_type },
    .{ "bool", .bool_type },
    .{ "void", .void_type },
});

fn builtinType(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const node = fir.locs[@intFromEnum(inst)].node;
    const main_token = fir.tree.mainToken(node);
    const type_str = fir.tree.tokenString(main_token);
    const builtin_type = builtin_types.get(type_str).?;
    const ip_index = switch (builtin_type) {
        .u8_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 8 } } }),
        .u16_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 16 } } }),
        .u32_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 32 } } }),
        .u64_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 64 } } }),
        .i8_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 8 } } }),
        .i16_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 16 } } }),
        .i32_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 32 } } }),
        .i64_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 64 } } }),
        .f32_type => try b.pool.getOrPut(.{ .ty = .{ .float = .{ .width = 32 } } }),
        .f64_type => try b.pool.getOrPut(.{ .ty = .{ .float = .{ .width = 64 } } }),
        .bool_type => try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 1 } } }),
        .void_type => try b.pool.getOrPut(.{ .ty = .{ .void = {} } }),
    };
    try b.mapInterned(inst, ip_index);
}

fn pointerType(b: *Block, inst: Fir.Index) !void {
    const pointer_type = b.fir.get(inst);
    const data = pointer_type.data.pointer_type;
    const pointee = b.resolveInterned(data.pointee);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = pointee } } });
    try b.mapInterned(inst, ip_index);
}

fn manyPointerType(b: *Block, inst: Fir.Index) !void {
    const many_pointer_type = b.fir.get(inst);
    const data = many_pointer_type.data.many_pointer_type;
    const pointee = b.resolveInterned(data.pointee);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .many_pointer = .{ .pointee = pointee } } });
    try b.mapInterned(inst, ip_index);
}

fn sliceType(b: *Block, inst: Fir.Index) !void {
    const slice_type = b.fir.get(inst);
    const data = slice_type.data.slice_type;
    const element = b.resolveInterned(data.element);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .slice = .{ .element = element } } });
    try b.mapInterned(inst, ip_index);
}

fn arrayType(b: *Block, inst: Fir.Index) !void {
    const array_type = b.fir.get(inst);
    const data = array_type.data.array_type;
    const element = b.resolveInterned(data.element);
    const count = count: {
        const ip_index = b.resolveInterned(data.element);
        const tv = b.pool.indexToKey(ip_index).tv;
        const ty = b.pool.indexToKey(tv.ty).ty;
        // TODO: emit error
        std.debug.assert(@as(std.meta.Tag(Type), ty) == .comptime_int);
        break :count tv.val.integer;
    };
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .array = .{ .element = element, .count = @intCast(count) } } });
    try b.mapInterned(inst, ip_index);
}

fn boolType(b: *Block, inst: Fir.Index) !void {
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 1 } } });
    try b.mapInterned(inst, ip_index);
}

fn functionType(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const function_type = fir.get(inst);
    const data = function_type.data.function_type;
    const return_type = b.resolveInterned(data.@"return");

    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.params);
    const params = fir.extraSlice(slice);
    const scratch_top = b.sema.scratch.items.len;
    defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
    try b.sema.scratch.ensureUnusedCapacity(b.arena, params.len);
    for (params) |param| {
        const ip_index = b.resolveInterned(@enumFromInt(param));
        b.sema.scratch.appendAssumeCapacity(@intFromEnum(ip_index));
    }

    const params_start: u32 = @intCast(b.pool.extra.items.len);
    try b.pool.extra.appendSlice(b.pool.gpa, b.sema.scratch.items[scratch_top..]);
    const params_end: u32 = @intCast(b.pool.extra.items.len);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .function = .{
        .params = .{ .start = params_start, .end = params_end },
        .@"return" = return_type,
    } } });
    try b.mapInterned(inst, ip_index);
}

fn firTypeOf(b: *Block, inst: Fir.Index) !void {
    const type_of = b.fir.get(inst);
    const operand = b.resolveInst(type_of.data.type_of);
    const ty = b.sema.tempAir().typeOf(operand);
    try b.mapInterned(inst, ty);
}

fn returnType(b: *Block, inst: Fir.Index) !void {
    const ty = b.return_type.?;
    try b.mapInterned(inst, ty);
}

fn paramType(b: *Block, inst: Fir.Index) !void {
    const param_type = b.fir.get(inst);
    const data = param_type.data.param_type;
    const function = b.resolveInst(data.function);
    const ty = b.sema.tempAir().typeOf(function);
    const function_type = b.pool.indexToKey(ty).ty.function;
    const params = b.pool.extra.items[function_type.params.start..function_type.params.end];
    try b.mapInterned(inst, @enumFromInt(params[data.index]));
}

fn elementType(b: *Block, inst: Fir.Index) !void {
    const element_type = b.fir.get(inst);
    const data = element_type.data.element_type;
    const parent = b.resolveInst(data.parent);
    const ty = b.sema.tempAir().typeOf(parent);
    const element = switch (b.pool.indexToKey(ty).ty) {
        .array => |array| array.element,
        .slice => |slice| slice.element,
        .many_pointer => |many_pointer| many_pointer.pointee,
        else => unreachable,
    };
    try b.mapInterned(inst, element);
}

fn firFunction(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const function = fir.get(inst);
    const data = function.data.function;
    const signature = b.resolveInterned(data.signature);
    // const function_type = b.pool.indexToKey(signature).ty.function;

    // const ip_index = try b.pool.putBody(signature, air);
    const air_index = try b.pool.addOneBody(signature);
    const ip_index = try b.pool.getOrPut(.{ .tv = .{
        .ty = signature,
        .val = .{ .body = air_index },
    } });
    const air_inst = try b.add(.{ .constant = ip_index });
    try b.mapInst(inst, air_inst);
    b.sema.function = .{
        .inst = inst,
        .body_index = air_index,
    };
}

fn analyzeFunctionBody(sema: *Sema) !void {
    const inst = sema.function.?.inst;
    const fir = sema.fir;
    const function = fir.get(inst);
    const data = function.data.function;
    const signature = sema.resolveInterned(data.signature);
    const function_type = sema.pool.indexToKey(signature).ty.function;

    var body_sema: Sema = .{
        .gpa = sema.gpa,
        .arena = sema.arena,
        .pool = sema.pool,
        .fir = fir,
        .insts = .{},
        .extra = .{},
        .inst_map = try sema.inst_map.clone(sema.arena),
        .ip_map = try sema.ip_map.clone(sema.arena),
        .return_type = function_type.@"return",
        .scratch = .{},
        .globals = sema.globals,
        .function = null,
    };
    const toplevel = try body_sema.analyzeBodyBlock(data.body);

    const air: Air = .{
        .insts = body_sema.insts.toOwnedSlice(),
        .extra = try body_sema.extra.toOwnedSlice(sema.gpa),
        .pool = body_sema.pool,
        .toplevel = toplevel,
    };
    const body = sema.pool.bodies.at(@intFromEnum(sema.function.?.body_index));
    body.* = air;
}

fn firCoerce(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const coerce = fir.get(inst);
    const data = coerce.data.coerce;

    const src = b.resolveInst(data.src);
    const dest_type = b.resolveInterned(data.ty);

    var info: Coercion = .{
        .b = b,
        .src = src,
        .dest_type = dest_type,
    };
    const air_inst = try info.coerce();
    try b.mapInst(inst, air_inst);
}

fn firParam(b: *Block, inst: Fir.Index) !void {
    const param = b.fir.get(inst);
    const data = param.data.param;
    const ty = b.resolveInterned(data.ty);

    const air_inst = try b.add(.{ .param = .{
        .name = data.name,
        .ty = ty,
    } });
    try b.mapInst(inst, air_inst);
}

fn push(b: *Block, inst: Fir.Index) !void {
    const push_inst = b.fir.get(inst);
    const data = push_inst.data.push;

    const operand = b.resolveInst(data);
    const ty = b.sema.tempAir().typeOf(operand);
    const pointer_type = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = ty } } });
    const alloc = try b.add(.{ .alloc = .{
        .slot_type = ty,
        .pointer_type = pointer_type,
    } });
    _ = try b.add(.{ .store = .{
        .ptr = alloc,
        .val = operand,
    } });
    try b.mapInst(inst, alloc);
}

fn load(b: *Block, inst: Fir.Index) !void {
    const load_inst = b.fir.get(inst);
    const data = load_inst.data.load;

    const ptr = b.resolveInst(data.ptr);
    const air_inst = try b.add(.{ .load = .{
        .ptr = ptr,
    } });
    try b.mapInst(inst, air_inst);
}

fn store(b: *Block, inst: Fir.Index) !void {
    const store_inst = b.fir.get(inst);
    const data = store_inst.data.store;

    const ptr = b.resolveInst(data.ptr);
    const val = b.resolveInst(data.val);
    const air_inst = try b.add(.{ .store = .{
        .ptr = ptr,
        .val = val,
    } });
    try b.mapInst(inst, air_inst);
}

fn call(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const call_inst = fir.get(inst);
    const data = call_inst.data.call;
    const function = b.resolveInst(data.function);
    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.args);
    const fir_args = fir.extraSlice(slice);

    const scratch_top = b.sema.scratch.items.len;
    defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
    try b.sema.scratch.ensureUnusedCapacity(b.arena, fir_args.len);
    for (fir_args) |fir_arg| {
        const arg = b.resolveInst(@enumFromInt(fir_arg));
        b.sema.scratch.appendAssumeCapacity(@intFromEnum(arg));
    }

    const args = b.sema.scratch.items[scratch_top..];
    const pl = try b.sema.addSlice(args);
    const air_inst = try b.add(.{ .call = .{
        .function = function,
        .args = pl,
    } });
    try b.mapInst(inst, air_inst);
}

fn loadGlobal(b: *Block, inst: Fir.Index) !void {
    const load_global = b.fir.get(inst);
    const data = load_global.data.load_global;

    const ip_index = b.sema.globals.get(data.name).?;
    const air_inst = try b.add(.{ .load_decl = ip_index });
    try b.mapInst(inst, air_inst);
}

pub fn coerceInnerImplicit(b: *Block, src: Air.Index, dest_type: Type) !Air.Index {
    var info: Coercion = .{
        .b = b,
        .src = src,
        .dest_type = try b.pool.getOrPut(.{ .ty = dest_type }),
    };
    return info.coerce();
}

fn binaryArithOp(b: *Block, inst: Fir.Index, comptime tag: std.meta.Tag(Fir.Inst.Data)) !void {
    const fir = b.fir;
    const binary = fir.get(inst);
    const data = @field(binary.data, @tagName(tag));
    // const op_token = b.tree.mainToken(binary.node);

    const temp_air = b.sema.tempAir();
    const l = b.resolveInst(data.l);
    const r = b.resolveInst(data.r);
    const lty_index = temp_air.typeOf(l);
    const rty_index = temp_air.typeOf(r);
    const lty = b.pool.indexToKey(lty_index).ty;
    const rty = b.pool.indexToKey(rty_index).ty;

    const air_tag: std.meta.Tag(Air.Inst) = switch (tag) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .mod => .mod,
        .bitwise_or => .bitwise_or,
        .bitwise_and => .bitwise_and,
        .bitwise_xor => .bitwise_xor,
        .sl, .sr => unreachable,
        else => undefined,
    };

    // pending a discussion on arithmetic overflow semantics,
    // femto only allows addition between variables of the same sign
    // comptime literals coerce to the fixed type of the other value
    // if both are comptime literals, the arithmetic is evaluated at
    // compile time (constant folding)
    switch (lty) {
        .int => |lty_int| switch (lty_int.sign) {
            .unsigned => switch (rty) {
                .int => |rty_int| switch (rty_int.sign) {
                    .unsigned => {
                        const bits = @max(lty_int.width, rty_int.width);
                        const dest_ty: Type = .{ .int = .{ .sign = .unsigned, .width = bits } };
                        const lref = try coerceInnerImplicit(b, l, dest_ty);
                        const rref = try coerceInnerImplicit(b, r, dest_ty);
                        const new_arith = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = lref, .r = rref }));
                        try b.mapInst(inst, new_arith);
                    },
                    .signed => {
                        // try hg.errors.append(hg.gpa, .{
                        //     .tag = .binary_diffsign,
                        //     .token = op_token,
                        // });
                        return error.HandledUserError;
                    },
                },
                .comptime_int => {
                    const lref = l;
                    const rref = try coerceInnerImplicit(b, r, lty);
                    const new_arith = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = lref, .r = rref }));
                    try b.mapInst(inst, new_arith);
                },
                else => unreachable, // TODO: should emit error
            },
            .signed => switch (rty) {
                .int => |rty_int| switch (rty_int.sign) {
                    .unsigned => {
                        // try hg.errors.append(hg.gpa, .{
                        //     .tag = .binary_diffsign,
                        //     .token = op_token,
                        // });
                        return error.HandledUserError;
                    },
                    .signed => {
                        const bits = @max(lty_int.width, rty_int.width);
                        const dest_ty: Type = .{ .int = .{ .sign = .signed, .width = bits } };
                        const lref = try coerceInnerImplicit(b, l, dest_ty);
                        const rref = try coerceInnerImplicit(b, r, dest_ty);
                        const new_arith = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = lref, .r = rref }));
                        try b.mapInst(inst, new_arith);
                    },
                },
                .comptime_int => {
                    const lref = l;
                    const rref = try coerceInnerImplicit(b, r, lty);

                    const new_arith = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = lref, .r = rref }));
                    try b.mapInst(inst, new_arith);
                },
                else => unreachable, // TODO: should emit error
            },
        },
        .comptime_int => {
            switch (rty) {
                .int => {
                    const lref = try coerceInnerImplicit(b, l, rty);
                    const rref = r;

                    const new_arith = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = lref, .r = rref }));
                    try b.mapInst(inst, new_arith);
                },
                .comptime_int => {
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

const ResolutionStrategy = enum {
    binary,
};

fn resolvePeerTypes(comptime strategy: ResolutionStrategy, types: anytype) ?Type {
    switch (strategy) {
        .binary => {
            // for now, we assume addition only works on ints and floats
            const lty = types[0];
            const rty = types[1];
            switch (lty) {
                .comptime_int => {
                    switch (rty) {
                        .comptime_int => {
                            if (lty.comptime_int.sign != rty.comptime_int.sign) return null;
                            return lty;
                        },
                        .int => return rty,
                        else => return null,
                    }
                },
                .int => {
                    switch (rty) {
                        .comptime_int => return lty,
                        .int => {
                            if (lty.int.sign != rty.int.sign) return null;
                            const width = @max(lty.int.width, rty.int.width);
                            return .{ .int = .{ .sign = lty.int.sign, .width = width } };
                        },
                        else => return null,
                    }
                },
                .comptime_float => switch (rty) {
                    .comptime_float => return lty,
                    .float => return rty,
                    else => return null,
                },
                .float => switch (rty) {
                    .comptime_float => return lty,
                    .float => {
                        const width = @max(lty.float.width, rty.float.width);
                        return .{ .float = .{ .width = width } };
                    },
                    else => return null,
                },
                else => return null,
            }
        },
    }
}

fn binaryCmp(b: *Block, inst: Fir.Index, comptime tag: std.meta.Tag(Fir.Inst.Data)) !void {
    const fir = b.fir;
    const binary = fir.get(inst);
    const data = @field(binary.data, @tagName(tag));

    const temp_air = b.sema.tempAir();
    var l = b.resolveInst(data.l);
    var r = b.resolveInst(data.r);
    const lty_index = temp_air.typeOf(l);
    const rty_index = temp_air.typeOf(r);
    const lty = b.pool.indexToKey(lty_index).ty;
    const rty = b.pool.indexToKey(rty_index).ty;

    const dest_type = resolvePeerTypes(.binary, .{ lty, rty }).?; // TODO: emit error if no type found
    l = try coerceInnerImplicit(b, l, dest_type);
    r = try coerceInnerImplicit(b, r, dest_type);

    switch (dest_type) {
        inline .comptime_int, .int => |int| switch (int.sign) {
            .unsigned => {
                const air_tag: std.meta.Tag(Air.Inst) = switch (tag) {
                    .cmp_eq => .icmp_eq,
                    .cmp_ne => .icmp_ne,
                    .cmp_gt => .icmp_ugt,
                    .cmp_ge => .icmp_uge,
                    .cmp_lt => .icmp_ult,
                    .cmp_le => .icmp_ule,
                    else => unreachable,
                };
                const air_inst = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = l, .r = r }));
                try b.mapInst(inst, air_inst);
            },
            .signed => {
                const air_tag: std.meta.Tag(Air.Inst) = switch (tag) {
                    .cmp_eq => .icmp_eq,
                    .cmp_ne => .icmp_ne,
                    .cmp_gt => .icmp_sgt,
                    .cmp_ge => .icmp_sge,
                    .cmp_lt => .icmp_slt,
                    .cmp_le => .icmp_sle,
                    else => unreachable,
                };
                const air_inst = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = l, .r = r }));
                try b.mapInst(inst, air_inst);
            },
        },
        .comptime_float, .float => {
            const air_tag: std.meta.Tag(Air.Inst) = switch (tag) {
                .cmp_gt => .fcmp_gt,
                .cmp_ge => .fcmp_ge,
                .cmp_lt => .fcmp_lt,
                .cmp_le => .fcmp_le,
                else => unreachable,
            };
            const air_inst = try b.add(@unionInit(Air.Inst, @tagName(air_tag), .{ .l = l, .r = r }));
            try b.mapInst(inst, air_inst);
        },
        else => unreachable,
    }

    switch (dest_type) {
        inline else => {},
    }
}

pub fn unaryNeg(b: *Block, inst: Fir.Index) !void {
    const neg = b.fir.get(inst);
    const data = neg.data.neg;

    const operand = b.resolveInst(data);
    const ty = b.sema.tempAir().typeOf(operand);
    const operand_type = b.pool.indexToKey(ty).ty;
    switch (operand_type) {
        .comptime_int => unreachable, // TODO
        // .comptime_int => |int| switch (int.sign) {
        //     .unsigned => {
        //         const src = b.insts.get(@intFromEnum(operand)).constant;
        //         const tv = b.pool.indexToKey(src).tv;
        //         const val = tv.val.integer;
        //         if (val > dest_type.maxInt()) {
        //             return error.Truncated;
        //         }
        //         return b.addConstant(.{ .tv = .{
        //             .ty = self.dest_type,
        //             .val = .{ .integer = val },
        //         } });
        //     },
        // }
        .int => |int| switch (int.sign) {
            .unsigned => unreachable, // TODO: emit error
            .signed => {
                const air_inst = try b.add(.{ .neg = operand });
                try b.mapInst(inst, air_inst);
            },
        },
        .float => {
            const air_inst = try b.add(.{ .neg = operand });
            try b.mapInst(inst, air_inst);
        },
        else => unreachable, // TODO: emit error
    }
}

pub fn bitwiseInv(b: *Block, inst: Fir.Index) !void {
    const neg = b.fir.get(inst);
    const data = neg.data.neg;

    const operand = b.resolveInst(data);
    // TODO: type checking
    const air_inst = try b.add(.{ .bitwise_inv = operand });
    try b.mapInst(inst, air_inst);
}

pub fn branchSingle(b: *Block, inst: Fir.Index) !void {
    const branch_single = b.fir.get(inst);
    const data = branch_single.data.branch_single;

    const cond = b.resolveInst(data.cond);
    const exec_true = try b.sema.analyzeBodyBlock(data.exec_true);
    const air_inst = try b.add(.{ .branch_single = .{
        .cond = cond,
        .exec_true = exec_true,
    } });
    try b.mapInst(inst, air_inst);
}

pub fn branchDouble(b: *Block, inst: Fir.Index) !void {
    const branch_double = b.fir.get(inst);
    const data = branch_double.data.branch_double;
    const branch = b.fir.extraData(Fir.Inst.BranchDouble, data.pl);

    const cond = b.resolveInst(data.cond);
    const exec_true = try b.sema.analyzeBodyBlock(branch.exec_true);
    const exec_false = try b.sema.analyzeBodyBlock(branch.exec_true);

    const pl = try b.sema.addExtra(Air.Inst.BranchDouble{
        .exec_true = exec_true,
        .exec_false = exec_false,
    });
    const air_inst = try b.add(.{ .branch_double = .{
        .cond = cond,
        .pl = pl,
    } });
    try b.mapInst(inst, air_inst);
}

pub fn loop(b: *Block, inst: Fir.Index) !void {
    const loop_inst = b.fir.get(inst);
    const data = loop_inst.data.loop;

    const cond = try b.sema.analyzeBodyBlock(data.cond);
    const body = try b.sema.analyzeBodyBlock(data.body);
    const air_inst = try b.add(.{ .loop = .{
        .cond = cond,
        .body = body,
    } });
    try b.mapInst(inst, air_inst);
}

pub fn firBlock(b: *Block, inst: Fir.Index) !void {
    const air_inst = try b.sema.analyzeBodyBlock(inst);
    try b.insts.append(b.arena, air_inst);
}

pub fn returnNode(b: *Block, inst: Fir.Index) !void {
    const return_node = b.fir.get(inst);
    const data = return_node.data.return_node;

    const operand = b.resolveInst(data);
    const air_inst = try b.add(.{ .@"return" = operand });
    try b.mapInst(inst, air_inst);
}

pub fn returnImplicit(b: *Block, inst: Fir.Index) !void {
    const return_implicit = b.fir.get(inst);
    const data = return_implicit.data.return_implicit;

    const operand = b.resolveInst(data);
    const air_inst = try b.add(.{ .@"return" = operand });
    try b.mapInst(inst, air_inst);
}

pub fn yieldNode(b: *Block, inst: Fir.Index) !void {
    const yield_node = b.fir.get(inst);
    const data = yield_node.data.yield_node;

    const operand = b.resolveInst(data);
    const air_inst = try b.add(.{ .yield = operand });
    try b.mapInst(inst, air_inst);
}

pub fn yieldImplicit(b: *Block, inst: Fir.Index) !void {
    const yield_implicit = b.fir.get(inst);
    const data = yield_implicit.data.yield_implicit;

    const operand = b.resolveInst(data);
    const air_inst = try b.add(.{ .yield = operand });
    try b.mapInst(inst, air_inst);
}

pub fn firContinue(b: *Block, inst: Fir.Index) !void {
    const air_inst = try b.add(.{ .@"continue" = {} });
    try b.mapInst(inst, air_inst);
}

pub fn firBreak(b: *Block, inst: Fir.Index) !void {
    const air_inst = try b.add(.{ .@"break" = {} });
    try b.mapInst(inst, air_inst);
}

pub fn tempAir(sema: *Sema) Air {
    return .{
        .insts = sema.insts.slice(),
        .extra = sema.extra.items,
        .pool = sema.pool,
        .toplevel = undefined,
    };
}
