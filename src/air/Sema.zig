const std = @import("std");
const Fir = @import("../fir/Fir.zig");
const Air = @import("Air.zig");
const Module = @import("Module.zig");
const InternPool = @import("../InternPool.zig");
const Decl = @import("Decl.zig");
const Allocator = std.mem.Allocator;

const Sema = @This();

gpa: Allocator,
arena: Allocator,
fir: *const Fir,
pool: *InternPool,
insts: std.MultiArrayList(Air.Inst),
map: std.AutoHashMapUnmanaged(Fir.Index, Air.Ref),
scratch: std.ArrayListUnmanaged(u32),

pub fn analyzeModule(gpa: Allocator, pool: *InternPool, fir: *const Fir) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    // var decls = std.ArrayList(InternPool.Index).init(gpa);

    const module_pl = fir.insts.items(.data)[@intFromEnum(fir.module_index)].module.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, module_pl);
    const globals = fir.extraSlice(slice);
    for (globals) |global| {
        var sema: Sema = .{
            .gpa = gpa,
            .arena = arena.allocator(),
            .pool = pool,
            .fir = fir,
            .insts = .{},
            .map = .{},
            .scratch = .{},
        };
        try sema.analyzeGlobal(undefined, @enumFromInt(global)); // TODO: get rid of this parameter
    }
}

fn analyzeGlobal(sema: *Sema, decls: *std.ArrayList(InternPool.Index), global_inst: Fir.Index) !void {
    _ = decls;
    const fir = sema.fir;
    const global = fir.insts.items(.data)[@intFromEnum(global_inst)].global;
    const block_pl = fir.insts.items(.data)[@intFromEnum(global.block)].block_inline.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);
    const decl_index = try sema.pool.addOneDecl();
    const decl_ip_index = try sema.pool.getOrPut(.{ .decl = decl_index });
    // const decl = sema.pool.decls.at(decl_index);

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
            .global_handle => try sema.globalHandle(inst, decl_ip_index),
            .global_set_mutable => try sema.globalSetMutable(inst),
            .global_set_type => try sema.globalSetType(inst),
            .global_set_init => try sema.globalSetInit(inst),
            .global_set_linkage_external => try sema.globalSetLinkageExternal(inst),
            else => {},
        }
    }
}

fn analyzeBodyBlock(sema: *Sema, block: Fir.Index) !Air.Index {
    const fir = sema.fir;
    const block_pl = fir.insts.items(.data)[@intFromEnum(block)].block.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, block_pl);
    const insts = fir.extraSlice(slice);
    for (insts) |inst| {
        const i: u32 = @intFromEnum(inst);
        const data = fir.insts.items(.data)[i];
        switch (data) {
            .int => sema.integerLiteral(inst),
            .float => sema.floatLiteral(inst),
            .builtin_type => try sema.builtinType(inst),
            .bool_type => try sema.boolType(inst),
            .pointer_type => try sema.pointerType(inst),
            .many_pointer_type => try sema.manyPointerType(inst),
            .slice_type => try sema.sliceType(inst),
            .array_type => try sema.arrayType(inst),
            .function_type => try sema.functionType(inst),
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

fn resolveInst(sema: *Sema, fir_inst: Fir.Index) Air.Ref {
    // TODO: globals
    return sema.map.get(fir_inst).?;
}

fn resolveDecl(sema: *Sema, inst: Air.Ref) *Decl {
    std.debug.assert(sema.insts.items(.tags)[@intFromEnum(inst)] == .load_decl);
    const load_decl = sema.insts.items(.data)[@intFromEnum(inst)].load_decl;
    const decl_index = sema.pool.items.items(.data)[@intFromEnum(load_decl)];
    return sema.pool.decls.at(decl_index);
}

fn add(sema: *Sema, inst: Air.Inst) !Air.Ref {
    const index: u32 = @intCast(sema.insts.len);
    try sema.insts.append(sema.gpa, inst);
    return @enumFromInt(index);
}

fn addConstant(sema: *Sema, key: InternPool.Key) !Air.Ref {
    const ip_index = try sema.pool.getOrPut(key);
    const raw: u32 = @intFromEnum(ip_index);
    return @enumFromInt(raw & (1 << 31));
}

fn globalHandle(sema: *Sema, inst: Fir.Index, decl: InternPool.Index) !void {
    const air_inst = try sema.add(.{ .load_decl = decl });
    try sema.map.put(sema.arena, inst, air_inst);
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
    decl.ty = sema.resolveInst(data.ty).toInterned().?;
}

fn globalSetInit(sema: *Sema, inst: Fir.Index) !void {
    const global_set_init = sema.fir.get(inst);
    const data = global_set_init.data.global_set_init;
    const decl = sema.resolveDecl(sema.resolveInst(data.handle));
    decl.initializer = sema.resolveInst(data.val).toInterned().?;
}

fn globalSetLinkageExternal(sema: *Sema, inst: Fir.Index) !void {
    const global_set_linkage_external = sema.fir.get(inst);
    const data = global_set_linkage_external.data.global_set_linkage_external;
    const decl = sema.resolveDecl(sema.resolveInst(data));
    decl.linkage = .external;
}

fn integerLiteral(sema: *Sema, inst: Fir.Index) !void {
    const int = sema.fir.get(inst);
    const ip_index = try sema.pool.getOrPut(.{ .int = .{
        .ty = try sema.pool.getOrPut(.{ .ty = .{ .comptime_int = .{ .sign = .unsigned } } }),
        .value = int.data.int,
    } });
    const air_inst = try sema.add(.{ .constant = ip_index });
    try sema.map.put(sema.arena, inst, air_inst);
}

fn floatLiteral(sema: *Sema, inst: Fir.Index) !void {
    const float = sema.fir.get(inst);
    const ip_index = try sema.pool.getOrPut(.{ .float = .{
        .ty = try sema.pool.getOrPut(.{ .ty = .{ .comptime_float = {} } }),
        .value = float.data.float,
    } });
    const air_inst = try sema.add(.{ .constant = ip_index });
    try sema.map.put(sema.arena, inst, air_inst);
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
    const ip_index = switch (builtin_type) {
        .u8_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 8 } }),
        .u16_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 16 } }),
        .u32_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 32 } }),
        .u64_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 64 } }),
        .i8_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .signed, .width = 8 } }),
        .i16_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .signed, .width = 16 } }),
        .i32_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .signed, .width = 32 } }),
        .i64_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .signed, .width = 64 } }),
        .f32_type => try sema.pool.getOrPut(.{ .float_type = .{ .is_comptime = false, .width = 32 } }),
        .f64_type => try sema.pool.getOrPut(.{ .float_type = .{ .is_comptime = false, .width = 64 } }),
        .bool_type => try sema.pool.getOrPut(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 1 } }),
        .void_type => try sema.pool.getOrPut(.{ .void_type = .{} }),
    };
    const air_inst = try sema.add(.{ .constant = ip_index });
    try sema.map.put(sema.arena, inst, air_inst);
}

fn pointerType(sema: *Sema, inst: Fir.Index) !void {
    const pointer_type = sema.fir.get(inst);
    const data = pointer_type.data.pointer_type;
    const pointee = sema.resolveInst(data.pointee).toInterned().?;
    const air_ref = try sema.addConstant(.{ .pointer_type = .{ .pointee = pointee } });
    try sema.map.put(sema.arena, inst, air_ref);
}

fn manyPointerType(sema: *Sema, inst: Fir.Index) !void {
    const many_pointer_type = sema.fir.get(inst);
    const data = many_pointer_type.data.many_pointer_type;
    const pointee = sema.resolveInst(data.pointee).toInterned().?;
    const air_ref = try sema.addConstant(.{ .many_pointer_type = .{ .pointee = pointee } });
    try sema.map.put(sema.arena, inst, air_ref);
}

fn sliceType(sema: *Sema, inst: Fir.Index) !void {
    const slice_type = sema.fir.get(inst);
    const data = slice_type.data.slice_type;
    const element = sema.resolveInst(data.element).toInterned().?;
    const air_ref = try sema.addConstant(.{ .slice_type = .{ .element = element } });
    try sema.map.put(sema.arena, inst, air_ref);
}

fn arrayType(sema: *Sema, inst: Fir.Index) !void {
    const array_type = sema.fir.get(inst);
    const data = array_type.data.array_type;
    const element = sema.resolveInst(data.element).toInterned().?;
    const count_ip_index = sema.resolveInst(data.element).toInterned().?;
    const count = sema.pool.indexToKey(count_ip_index).int.value;
    const air_ref = try sema.addConstant(.{ .array_type = .{ .element = element, .count = @intCast(count) } });
    try sema.map.put(sema.arena, inst, air_ref);
}

fn boolType(sema: *Sema, inst: Fir.Index) !void {
    const air_ref = try sema.addConstant(.{ .int_type = .{ .is_comptime = false, .sign = .unsigned, .width = 1 } });
    try sema.map.put(sema.arena, inst, air_ref);
}

fn functionType(sema: *Sema, inst: Fir.Index) !void {
    const fir = sema.fir;
    const function_type = fir.get(inst);
    const data = function_type.data.function_type;
    const return_type = sema.resolveInst(data.@"return").toInterned().?;

    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.params);
    const params = fir.extraSlice(slice);
    const scratch_top = sema.scratch.items.len;
    defer sema.scratch.shrinkRetainingCapacity(scratch_top);
    try sema.scratch.ensureUnusedCapacity(sema.arena, params.len);
    for (params) |param| {
        const ip_index = sema.resolveInst(@enumFromInt(param)).toInterned().?;
        sema.scratch.appendAssumeCapacity(@intFromEnum(ip_index));
    }

    const params_start: u32 = @intCast(sema.pool.extra.items.len);
    try sema.pool.extra.appendSlice(sema.pool.gpa, sema.scratch.items[scratch_top..]);
    const params_end: u32 = @intCast(sema.pool.extra.items.len);
    const air_ref = try sema.addConstant(.{ .function_type = .{
        .params = .{ .start = params_start, .end = params_end },
        .@"return" = return_type,
    } });
    try sema.map.put(sema.arena, inst, air_ref);
}
