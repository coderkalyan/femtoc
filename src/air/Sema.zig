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
        try sema.analyzeGlobal(undefined, @enumFromInt(global)); // TODO: get rid of this parameter

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

fn analyzeGlobal(sema: *Sema, decls: *std.ArrayList(InternPool.Index), global_inst: Fir.Index) !void {
    _ = decls;
    const fir = sema.fir;
    const global = fir.insts.items(.data)[@intFromEnum(global_inst)].global;
    const block_pl = fir.insts.items(.data)[@intFromEnum(global.block)].block_inline.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);
    // const decl_index = try sema.pool.addOneDecl();
    // const decl_ip_index = try sema.pool.getOrPut(.{ .decl = decl_index });
    const decl_ip_index = sema.globals.get(global.name).?;
    // const decl_index = sema.pool.indexToKey(decl_ip_index).decl;
    // const decl = sema.pool.decls.at(@intFromEnum(decl_index));
    // decl.name = global.name;

    for (insts) |i| {
        const inst: Fir.Index = @enumFromInt(i);
        switch (fir.insts.items(.tags)[i]) {
            .int => try sema.integerLiteral(inst),
            .float => try sema.floatLiteral(inst),
            .builtin_type => try sema.builtinType(inst),
            .bool_type => try sema.boolType(inst),
            .pointer_type => try sema.pointerType(inst),
            .many_pointer_type => try sema.manyPointerType(inst),
            .slice_type => try sema.sliceType(inst),
            .array_type => try sema.arrayType(inst),
            .function_type => try sema.functionType(inst),
            .type_of => try sema.firTypeOf(inst),
            .function => try sema.firFunction(inst),
            .global_handle => try sema.globalHandle(inst, decl_ip_index),
            .global_set_mutable => try sema.globalSetMutable(inst),
            .global_set_type => try sema.globalSetType(inst),
            .global_set_init => try sema.globalSetInit(inst),
            .global_set_linkage_external => try sema.globalSetLinkageExternal(inst),
            .yield_inline => break,
            else => |tag| {
                std.log.err("encountered invalid global instruction: {}\n", .{tag});
                unreachable;
            },
        }
    }
}

fn analyzeBodyBlock(sema: *Sema, block: Fir.Index) !void {
    const fir = sema.fir;
    const block_pl = fir.insts.items(.data)[@intFromEnum(block)].block.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);
    for (insts) |i| {
        const inst: Fir.Index = @enumFromInt(i);
        switch (fir.insts.items(.tags)[i]) {
            .int => try sema.integerLiteral(inst),
            .float => try sema.floatLiteral(inst),
            .builtin_type => try sema.builtinType(inst),
            .bool_type => try sema.boolType(inst),
            .pointer_type => try sema.pointerType(inst),
            .many_pointer_type => try sema.manyPointerType(inst),
            .slice_type => try sema.sliceType(inst),
            .array_type => try sema.arrayType(inst),
            .function_type => try sema.functionType(inst),
            .type_of => try sema.firTypeOf(inst),
            .param_type => try sema.paramType(inst),
            .coerce => try sema.firCoerce(inst),
            .param => try sema.firParam(inst),
            .push => try sema.push(inst),
            .load => try sema.load(inst),
            .store => try sema.store(inst),
            .return_type => try sema.returnType(inst),
            .call => try sema.call(inst),
            .load_global => try sema.loadGlobal(inst),
            else => {},
            // .none => unreachable, // TODO
            // .string => unreachable,
            // .array => unreachable,
        }
    }
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

fn globalHandle(sema: *Sema, inst: Fir.Index, decl: InternPool.Index) !void {
    // const decl_index = sema.pool.indexToKey(decl).decl;
    // const decl_data = sema.pool.decls.at(@intFromEnum(decl_index));
    const air_inst = try sema.add(.{ .load_decl = decl });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn globalSetMutable(sema: *Sema, inst: Fir.Index) !void {
    const global_set_mutable = sema.fir.get(inst);
    const decl = sema.resolveDecl(sema.resolveInst(global_set_mutable.data.global_set_mutable));
    decl.mutable = true;
}

fn globalSetType(sema: *Sema, inst: Fir.Index) !void {
    const global_set_type = sema.fir.get(inst);
    const data = global_set_type.data.global_set_type;
    const decl = sema.resolveDecl(sema.resolveInst(data.handle));
    decl.ty = sema.resolveInterned(data.ty);
}

fn globalSetInit(sema: *Sema, inst: Fir.Index) !void {
    const global_set_init = sema.fir.get(inst);
    const data = global_set_init.data.global_set_init;
    const decl = sema.resolveDecl(sema.resolveInst(data.handle));
    const air_inst = sema.resolveInst(data.val);
    decl.initializer = sema.insts.items(.data)[@intFromEnum(air_inst)].constant;
}

fn globalSetLinkageExternal(sema: *Sema, inst: Fir.Index) !void {
    const global_set_linkage_external = sema.fir.get(inst);
    const data = global_set_linkage_external.data.global_set_linkage_external;
    const decl = sema.resolveDecl(sema.resolveInst(data));
    decl.linkage = .external;
}

fn integerLiteral(sema: *Sema, inst: Fir.Index) !void {
    const int = sema.fir.get(inst);
    const air_inst = try sema.addConstant(.{ .tv = .{
        .ty = try sema.pool.getOrPut(.{ .ty = .{ .comptime_int = .{ .sign = .unsigned } } }),
        .val = .{ .integer = int.data.int },
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn floatLiteral(sema: *Sema, inst: Fir.Index) !void {
    const float = sema.fir.get(inst);
    const air_inst = try sema.addConstant(.{ .tv = .{
        .ty = try sema.pool.getOrPut(.{ .ty = .{ .comptime_float = {} } }),
        .val = .{ .float = float.data.float },
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
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

fn builtinType(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const node = fir.locs[@intFromEnum(inst)].node;
    const main_token = fir.tree.mainToken(node);
    const type_str = fir.tree.tokenString(main_token);
    const builtin_type = builtin_types.get(type_str).?;
    const air_ref = switch (builtin_type) {
        .u8_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 8 } } }),
        .u16_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 16 } } }),
        .u32_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 32 } } }),
        .u64_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 64 } } }),
        .i8_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 8 } } }),
        .i16_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 16 } } }),
        .i32_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 32 } } }),
        .i64_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .signed, .width = 64 } } }),
        .f32_type => try sema.pool.getOrPut(.{ .ty = .{ .float = .{ .width = 32 } } }),
        .f64_type => try sema.pool.getOrPut(.{ .ty = .{ .float = .{ .width = 64 } } }),
        .bool_type => try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 1 } } }),
        .void_type => try sema.pool.getOrPut(.{ .ty = .{ .void = {} } }),
    };
    // const air_inst = try sema.add(.{ .constant = ip_index });
    try sema.ip_map.put(sema.arena, inst, air_ref);
}

fn pointerType(sema: *Sema, inst: Fir.Index) !void {
    const pointer_type = sema.fir.get(inst);
    const data = pointer_type.data.pointer_type;
    const pointee = sema.resolveInterned(data.pointee);
    const ip_index = try sema.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = pointee } } });
    try sema.ip_map.put(sema.arena, inst, ip_index);
}

fn manyPointerType(sema: *Sema, inst: Fir.Index) !void {
    const many_pointer_type = sema.fir.get(inst);
    const data = many_pointer_type.data.many_pointer_type;
    const pointee = sema.resolveInterned(data.pointee);
    const ip_index = try sema.pool.getOrPut(.{ .ty = .{ .many_pointer = .{ .pointee = pointee } } });
    try sema.ip_map.put(sema.arena, inst, ip_index);
}

fn sliceType(sema: *Sema, inst: Fir.Index) !void {
    const slice_type = sema.fir.get(inst);
    const data = slice_type.data.slice_type;
    const element = sema.resolveInterned(data.element);
    const ip_index = try sema.pool.getOrPut(.{ .ty = .{ .slice = .{ .element = element } } });
    try sema.ip_map.put(sema.arena, inst, ip_index);
}

fn arrayType(sema: *Sema, inst: Fir.Index) !void {
    const array_type = sema.fir.get(inst);
    const data = array_type.data.array_type;
    const element = sema.resolveInterned(data.element);
    const count = count: {
        const ip_index = sema.resolveInterned(data.element);
        const tv = sema.pool.indexToKey(ip_index).tv;
        const ty = sema.pool.indexToKey(tv.ty).ty;
        // TODO: emit error
        std.debug.assert(@as(std.meta.Tag(Type), ty) == .comptime_int);
        break :count tv.val.integer;
    };
    const ip_index = try sema.pool.getOrPut(.{ .ty = .{ .array = .{ .element = element, .count = @intCast(count) } } });
    try sema.ip_map.put(sema.arena, inst, ip_index);
}

fn boolType(sema: *Sema, inst: Fir.Index) !void {
    const ip_index = try sema.pool.getOrPut(.{ .ty = .{ .int = .{ .sign = .unsigned, .width = 1 } } });
    try sema.ip_map.put(sema.arena, inst, ip_index);
}

fn functionType(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const function_type = fir.get(inst);
    const data = function_type.data.function_type;
    const return_type = sema.resolveInterned(data.@"return");

    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.params);
    const params = fir.extraSlice(slice);
    const scratch_top = sema.scratch.items.len;
    defer sema.scratch.shrinkRetainingCapacity(scratch_top);
    try sema.scratch.ensureUnusedCapacity(sema.arena, params.len);
    for (params) |param| {
        const ip_index = sema.resolveInterned(@enumFromInt(param));
        sema.scratch.appendAssumeCapacity(@intFromEnum(ip_index));
    }

    const params_start: u32 = @intCast(sema.pool.extra.items.len);
    try sema.pool.extra.appendSlice(sema.pool.gpa, sema.scratch.items[scratch_top..]);
    const params_end: u32 = @intCast(sema.pool.extra.items.len);
    const air_inst = try sema.pool.getOrPut(.{ .ty = .{ .function = .{
        .params = .{ .start = params_start, .end = params_end },
        .@"return" = return_type,
    } } });
    try sema.ip_map.put(sema.arena, inst, air_inst);
}

fn firTypeOf(sema: *Sema, inst: Fir.Index) !void {
    const type_of = sema.fir.get(inst);
    const operand = sema.resolveInst(type_of.data.type_of);
    const ty = sema.tempAir().typeOf(operand);
    try sema.ip_map.put(sema.arena, inst, ty);
}

fn returnType(sema: *Sema, inst: Fir.Index) !void {
    const ty = sema.return_type.?;
    try sema.ip_map.put(sema.arena, inst, ty);
}

fn paramType(sema: *Sema, inst: Fir.Index) !void {
    const param_type = sema.fir.get(inst);
    const data = param_type.data.param_type;
    const function = sema.resolveInst(data.function);
    const ty = sema.tempAir().typeOf(function);
    const function_type = sema.pool.indexToKey(ty).ty.function;
    const params = sema.pool.extra.items[function_type.params.start..function_type.params.end];
    try sema.ip_map.put(sema.arena, inst, @enumFromInt(params[data.index]));
}

fn firFunction(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const function = fir.get(inst);
    const data = function.data.function;
    const signature = sema.resolveInterned(data.signature);
    // const function_type = sema.pool.indexToKey(signature).ty.function;

    // const ip_index = try sema.pool.putBody(signature, air);
    const air_index = try sema.pool.addOneBody(signature);
    const ip_index = try sema.pool.getOrPut(.{ .tv = .{
        .ty = signature,
        .val = .{ .body = air_index },
    } });
    const air_inst = try sema.add(.{ .constant = ip_index });
    try sema.inst_map.put(sema.arena, inst, air_inst);
    sema.function = .{
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
    try body_sema.analyzeBodyBlock(data.body);

    const air: Air = .{
        .insts = body_sema.insts.toOwnedSlice(),
        .extra = try body_sema.extra.toOwnedSlice(sema.gpa),
        .pool = body_sema.pool,
    };
    const body = sema.pool.bodies.at(@intFromEnum(sema.function.?.body_index));
    body.* = air;
}

fn firCoerce(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const coerce = fir.get(inst);
    const data = coerce.data.coerce;

    const src = sema.resolveInst(data.src);
    const dest_type = sema.resolveInterned(data.ty);

    var info: Coercion = .{
        .sema = sema,
        .src = src,
        .dest_type = dest_type,
    };
    const air_inst = try info.coerce();
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn firParam(sema: *Sema, inst: Fir.Index) !void {
    const param = sema.fir.get(inst);
    const data = param.data.param;
    const ty = sema.resolveInterned(data.ty);

    const air_inst = try sema.add(.{ .param = .{
        .name = data.name,
        .ty = ty,
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn push(sema: *Sema, inst: Fir.Index) !void {
    const push_inst = sema.fir.get(inst);
    const data = push_inst.data.push;

    const operand = sema.resolveInst(data);
    const ty = sema.tempAir().typeOf(operand);
    const pointer_type = try sema.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = ty } } });
    const alloc = try sema.add(.{ .alloc = .{
        .slot_type = ty,
        .pointer_type = pointer_type,
    } });
    _ = try sema.add(.{ .store = .{
        .ptr = alloc,
        .val = operand,
    } });
    try sema.inst_map.put(sema.arena, inst, alloc);
}

fn load(sema: *Sema, inst: Fir.Index) !void {
    const load_inst = sema.fir.get(inst);
    const data = load_inst.data.load;

    const ptr = sema.resolveInst(data.ptr);
    const air_inst = try sema.add(.{ .load = .{
        .ptr = ptr,
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn store(sema: *Sema, inst: Fir.Index) !void {
    const store_inst = sema.fir.get(inst);
    const data = store_inst.data.store;

    const ptr = sema.resolveInst(data.ptr);
    const val = sema.resolveInst(data.val);
    const air_inst = try sema.add(.{ .store = .{
        .ptr = ptr,
        .val = val,
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn call(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const call_inst = fir.get(inst);
    const data = call_inst.data.call;
    const function = sema.resolveInst(data.function);
    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.args);
    const fir_args = fir.extraSlice(slice);

    const scratch_top = sema.scratch.items.len;
    defer sema.scratch.shrinkRetainingCapacity(scratch_top);
    try sema.scratch.ensureUnusedCapacity(sema.arena, fir_args.len);
    for (fir_args) |fir_arg| {
        const arg = sema.resolveInst(@enumFromInt(fir_arg));
        sema.scratch.appendAssumeCapacity(@intFromEnum(arg));
    }

    const args = sema.scratch.items[scratch_top..];
    const pl = try sema.addSlice(args);
    const air_inst = try sema.add(.{ .call = .{
        .function = function,
        .args = pl,
    } });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

fn loadGlobal(sema: *Sema, inst: Fir.Index) !void {
    const load_global = sema.fir.get(inst);
    const data = load_global.data.load_global;

    const ip_index = sema.globals.get(data.name).?;
    const air_inst = try sema.add(.{ .load_decl = ip_index });
    try sema.inst_map.put(sema.arena, inst, air_inst);
}

pub fn tempAir(sema: *Sema) Air {
    return .{
        .insts = sema.insts.slice(),
        .extra = sema.extra.items,
        .pool = sema.pool,
    };
}
