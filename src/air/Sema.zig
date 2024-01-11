const std = @import("std");
const Fir = @import("../fir/Fir.zig");
const Air = @import("Air.zig");
const InternPool = @import("../InternPool.zig");
const Coercion = @import("Coercion.zig");
const coercion = @import("coercion.zig");
const alu = @import("alu.zig");
const Type = @import("type.zig").Type;
const TypedValue = @import("TypedValue.zig");
const error_handler = @import("../error_handler.zig");
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
errors: *std.ArrayListUnmanaged(error_handler.SourceError),

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
        const index = try b.sema.addConstant(key);
        try b.insts.append(b.arena, index);
        return index;
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

    pub fn typeOf(b: *Block, inst: Air.Index) InternPool.Index {
        return b.sema.tempAir().typeOf(inst);
    }

    pub fn addIndexRef(b: *Block, base: Air.Index, index: Air.Index, element: InternPool.Index, mutable: bool) !Air.Index {
        const element_ptr = try b.pool.getOrPutType(.{ .ref = .{ .pointee = element, .mutable = mutable } });
        const pl = try b.sema.addExtra(Air.Inst.IndexRef{
            .base = base,
            .index = index,
            .ty = element_ptr,
        });
        return b.add(.{ .index_ref = .{ .pl = pl } });
    }

    pub fn addIndexVal(b: *Block, base: Air.Index, index: Air.Index) !Air.Index {
        return b.add(.{ .index_val = .{
            .base = base,
            .index = index,
        } });
    }
};

pub fn analyzeModule(gpa: Allocator, pool: *InternPool, fir: *const Fir) ![]error_handler.SourceError {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var globals = GlobalMap.init(arena.allocator());
    defer globals.deinit();

    const module_pl = fir.insts.items(.data)[@intFromEnum(fir.module_index)].module.insts;
    const slice = fir.extraData(Fir.Inst.ExtraSlice, module_pl);
    const module_globals = fir.extraSlice(slice);
    var errors: std.ArrayListUnmanaged(error_handler.SourceError) = .{};

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
            .errors = &errors,
        };
        sema.analyzeGlobal(@enumFromInt(global)) catch |err| if (err != error.HandledUserError) {
            return err;
        };

        if (sema.function) |_| {
            sema.analyzeFunctionBody() catch |err| if (err != error.HandledUserError) {
                return err;
            };
        }
    }

    return errors.toOwnedSlice(gpa);
}

fn registerGlobal(fir: *const Fir, pool: *InternPool, globals: *GlobalMap, global_inst: Fir.Index) !void {
    const global = fir.insts.items(.data)[@intFromEnum(global_inst)].global;
    const decl_index = try pool.addOneDecl();
    const decl_ip_index = try pool.getOrPut(.{ .decl = decl_index });
    const decl = pool.decls.at(@intFromEnum(decl_index));
    decl.* = .{
        .name = .{ .named = global.name },
        .ty = undefined, // must be set
        .initializer = null,
        .mutable = false,
        .linkage = .internal,
    };
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
            .bool => try boolLiteral(&b, inst),
            .none => try voidLiteral(&b, inst),
            .string => unreachable, // TODO
            .array => try array(&b, inst),
            .@"struct" => try firStruct(&b, inst),
            .builtin_type => try builtinType(&b, inst),
            .pointer_type => try pointerType(&b, inst),
            .many_pointer_type => try manyPointerType(&b, inst),
            .slice_type => try sliceType(&b, inst),
            .array_type => try arrayType(&b, inst),
            .function_type => try functionType(&b, inst),
            .struct_type => try structType(&b, inst),
            .type_of => try firTypeOf(&b, inst),
            .function => try firFunction(&b, inst),
            .coerce => try firCoerce(&b, inst),
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

fn analyzeBodyBlock(sema: *Sema, block: Fir.Index) error{ NotImplemented, Truncated, InvalidCoercion, OutOfMemory, HandledUserError, Overflow, DivZero }!Air.Index {
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
            .bool => try boolLiteral(&b, inst),
            .none => try voidLiteral(&b, inst),
            .string => try stringLiteral(&b, inst),
            .array => try array(&b, inst),
            .@"struct" => try firStruct(&b, inst),
            .builtin_type => try builtinType(&b, inst),
            .pointer_type => try pointerType(&b, inst),
            .many_pointer_type => try manyPointerType(&b, inst),
            .array_type => try arrayType(&b, inst),
            .slice_type => try sliceType(&b, inst),
            .function_type => try functionType(&b, inst),
            .struct_type => try structType(&b, inst),
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
            .logical_xor,
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
            => |tag| try binaryCmp(&b, inst, tag),
            .neg => try unaryNeg(&b, inst),
            .logical_not => try logicalNot(&b, inst),
            .bitwise_not => try bitwiseNot(&b, inst),

            .coerce => try firCoerce(&b, inst),
            .reftoptr => try reftoptr(&b, inst),
            .ptrtoref => try ptrtoref(&b, inst),
            .call => try call(&b, inst),
            .index_val => try indexVal(&b, inst),
            .index_ref => try indexRef(&b, inst),
            .slice => try firSlice(&b, inst),
            .field_val => try fieldVal(&b, inst),
            .field_ref => unreachable, // TODO
            // .field_ref => try fieldRef(&b, inst),

            .load_global => try loadGlobal(&b, inst),
            .push, .push_mut => try push(&b, inst),
            .load => try load(&b, inst),
            .load_lazy => try loadLazy(&b, inst),
            .store => try store(&b, inst),

            .branch_single => try branchSingle(&b, inst),
            .branch_double => try branchDouble(&b, inst),
            .loop_forever => try loopForever(&b, inst),
            .loop_while => try loopWhile(&b, inst),
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
    const air_inst = sema.inst_map.get(fir_inst).?;
    return air_inst;
}

fn resolveInterned(sema: *Sema, fir_inst: Fir.Index) InternPool.Index {
    // TODO: globals
    return sema.ip_map.get(fir_inst).?;
}

fn resolveDecl(sema: *Sema, inst: Air.Index) *Decl {
    std.debug.assert(sema.insts.items(.tags)[@intFromEnum(inst)] == .load_decl);
    const load_decl = sema.insts.items(.data)[@intFromEnum(inst)].load_decl;
    const decl_index = sema.pool.items.items(.data)[@intFromEnum(load_decl.ip_index)];
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
    const decl_index = b.pool.indexToKey(decl).decl;
    const decl_data = b.pool.decls.at(@intFromEnum(decl_index));
    const ty = decl_data.ty;
    const pointer_type = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = ty, .mutable = decl_data.mutable } } });
    const air_inst = try b.add(.{ .load_decl = .{
        .ip_index = decl,
        .ty = pointer_type,
    } });
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
        .ty = .comptime_uint_type,
        .val = .{ .integer = int.data.int },
    } });
    try b.mapInst(inst, air_inst);
}

fn floatLiteral(b: *Block, inst: Fir.Index) !void {
    const float = b.fir.get(inst);
    const air_inst = try b.addConstant(.{ .tv = .{
        .ty = .comptime_float_type,
        .val = .{ .float = float.data.float },
    } });
    try b.mapInst(inst, air_inst);
}

fn boolLiteral(b: *Block, inst: Fir.Index) !void {
    const bool_inst = b.fir.get(inst);
    const air_inst = try b.add(.{ .constant = if (bool_inst.data.bool) .bool_true else .bool_false });
    try b.mapInst(inst, air_inst);
}

fn voidLiteral(b: *Block, inst: Fir.Index) !void {
    const air_inst = try b.add(.{ .constant = .none });
    try b.mapInst(inst, air_inst);
}

fn stringLiteral(b: *Block, inst: Fir.Index) !void {
    const pool = b.pool;
    const string_inst = b.fir.get(inst);
    const literal = pool.getString(string_inst.data.string).?;
    const decl_index = try pool.addOneDecl();
    const decl_ip_index = try pool.getOrPut(.{ .decl = decl_index });
    const decl = pool.decls.at(@intFromEnum(decl_index));

    const buffer_type = try pool.getOrPutType(.{ .array = .{ .element = .u8_type, .count = @intCast(literal.len + 1) } });
    const buffer_tv = try pool.getOrPut(.{ .tv = .{
        .ty = buffer_type,
        .val = .{ .string = string_inst.data.string },
    } });
    decl.* = .{
        .name = .{ .unnamed = {} },
        .ty = buffer_type,
        .initializer = buffer_tv,
        .mutable = false,
        .linkage = .internal,
    };

    const ptr = try b.add(.{ .load_decl = .{
        .ip_index = decl_ip_index,
        .ty = buffer_type,
    } });
    const len = try b.add(.{ .constant = try pool.getOrPut(.{ .tv = .{
        .ty = .u64_type,
        .val = .{ .integer = literal.len },
    } }) });
    const slice_type = try b.pool.getOrPut(.{ .ty = .{
        .slice = .{ .element = .u8_type },
    } });
    const pl = try b.sema.addExtra(Air.Inst.SliceInit{
        .ptr = ptr,
        .len = len,
        .ty = slice_type,
    });

    const air_inst = try b.add(.{ .slice_init = .{ .pl = pl } });
    try b.mapInst(inst, air_inst);
}

fn array(b: *Block, inst: Fir.Index) !void {
    const pl = b.fir.get(inst).data.array;
    const slice = b.fir.extraData(Fir.Inst.ExtraSlice, pl);
    const fir_elements = b.fir.extraSlice(slice);

    const element_types = try b.arena.alloc(Type, fir_elements.len);
    const scratch_top = b.sema.scratch.items.len;
    defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
    try b.sema.scratch.ensureUnusedCapacity(b.arena, fir_elements.len);
    for (fir_elements, 0..) |fir_element, i| {
        const element = b.resolveInst(@enumFromInt(fir_element));
        element_types[i] = b.pool.indexToType(b.typeOf(element));
        b.sema.scratch.appendAssumeCapacity(@intFromEnum(element));
    }
    const dest_type = coercion.resolvePeerTypes(b.pool, element_types).?;

    var is_constant = true;
    const elements = b.sema.scratch.items[scratch_top..];
    for (elements, 0..) |inner, i| {
        // TODO: do this without emitting anything if possible
        const dest = try coerceInnerImplicit(b, @enumFromInt(inner), dest_type);
        elements[i] = @intFromEnum(dest);
        if (b.sema.insts.items(.tags)[@intFromEnum(dest)] != .constant) is_constant = false;
    }

    const dest_ip_index = try b.pool.getOrPut(.{ .ty = dest_type });
    const array_type = try b.pool.getOrPut(.{ .ty = .{
        .array = .{ .element = dest_ip_index, .count = @intCast(elements.len) },
    } });

    if (is_constant) {
        // upgrade the elements from constants to typed values, and emit an array tv
        for (elements, 0..) |constant, i| {
            const tv = b.sema.insts.get(constant).constant;
            elements[i] = @intFromEnum(tv);
        }

        const air_inst = try arrayConstantInner(b, array_type, elements);
        try b.mapInst(inst, air_inst);
    } else {
        const air_inst = try b.add(.{ .array_init = .{
            .ty = array_type,
            .elements = try b.sema.addSlice(elements),
        } });
        try b.mapInst(inst, air_inst);
    }
}

fn firStruct(b: *Block, inst: Fir.Index) !void {
    const data = b.fir.get(inst).data.@"struct";
    const slice = b.fir.extraData(Fir.Inst.ExtraSlice, data.fields);
    const fir_fields = b.fir.extraSlice(slice);

    const struct_ip_index = b.resolveInterned(data.ty);
    const struct_type = b.pool.indexToType(struct_ip_index).@"struct";
    const field_map_index = b.pool.indexToKey(struct_type.names).field_map;
    const field_map = b.pool.fields.at(@intFromEnum(field_map_index));
    const field_types = b.pool.extra.items[struct_type.fields.start..struct_type.fields.end];

    const scratch_top = b.sema.scratch.items.len;
    defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
    const fields = try b.sema.scratch.addManyAsSlice(b.arena, fir_fields.len);
    for (fir_fields) |fir_field| {
        const initializer = b.fir.extraData(Fir.Inst.StructFieldInitializer, @enumFromInt(fir_field));
        const slot_index = field_map.get(initializer.name).?; // TODO: error if unknown field name
        const inner = b.resolveInst(initializer.val);
        const field_type = b.pool.indexToType(@enumFromInt(field_types[slot_index]));
        const val = try coerceInnerImplicit(b, inner, field_type);
        fields[slot_index] = @intFromEnum(val);
    }

    const air_inst = try b.add(.{ .struct_init = .{
        .ty = struct_ip_index,
        .fields = try b.sema.addSlice(fields),
    } });
    try b.mapInst(inst, air_inst);
}

fn arrayConstantInner(b: *Block, ty: InternPool.Index, tvs: []u32) !Air.Index {
    const elements_start: u32 = @intCast(b.pool.extra.items.len);
    try b.pool.extra.appendSlice(b.pool.gpa, tvs);
    const elements_end: u32 = @intCast(b.pool.extra.items.len);
    return b.addConstant(.{ .tv = .{
        .ty = ty,
        .val = .{ .array = .{ .start = elements_start, .end = elements_end } },
    } });
}

const builtin_types = std.ComptimeStringMap(InternPool.Index, .{
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
    const ip_index = builtin_types.get(type_str).?;
    try b.mapInterned(inst, ip_index);
}

fn pointerType(b: *Block, inst: Fir.Index) !void {
    const pointer_type = b.fir.get(inst);
    const data = pointer_type.data.pointer_type;
    const pointee = b.resolveInterned(data.pointee);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = pointee, .mutable = data.mutable } } });
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
        const constant = b.resolveInst(data.count);
        // const ip_index = b.resolveInterned(data.count);
        const ip_index = b.sema.insts.items(.data)[@intFromEnum(constant)].constant;
        const tv = b.pool.indexToKey(ip_index).tv;
        const ty = b.pool.indexToKey(tv.ty).ty;
        // TODO: emit error
        std.debug.assert(@as(std.meta.Tag(Type), ty) == .comptime_int);
        break :count tv.val.integer;
    };
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .array = .{ .element = element, .count = @intCast(count) } } });
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

fn structType(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const struct_type = fir.get(inst);
    const data = struct_type.data.struct_type;

    const field_map_index = try b.pool.addOneFieldMap();
    const field_map = b.pool.fields.at(@intFromEnum(field_map_index));
    field_map.* = .{};
    const slice = fir.extraData(Fir.Inst.ExtraSlice, data.fields);
    const fields = fir.extraSlice(slice);
    const scratch_top = b.sema.scratch.items.len;
    defer b.sema.scratch.shrinkRetainingCapacity(scratch_top);
    try b.sema.scratch.ensureUnusedCapacity(b.arena, fields.len);
    for (fields, 0..) |field, i| {
        const fir_field = fir.extraData(Fir.Inst.StructField, @enumFromInt(field));
        const ip_index = b.resolveInterned(fir_field.ty);
        b.sema.scratch.appendAssumeCapacity(@intFromEnum(ip_index));
        try field_map.put(b.pool.gpa, fir_field.name, @intCast(i));
    }

    const fields_start: u32 = @intCast(b.pool.extra.items.len);
    try b.pool.extra.appendSlice(b.pool.gpa, b.sema.scratch.items[scratch_top..]);
    const fields_end: u32 = @intCast(b.pool.extra.items.len);
    const ip_index = try b.pool.getOrPut(.{ .ty = .{ .@"struct" = .{
        .fields = .{ .start = fields_start, .end = fields_end },
        .names = try b.pool.getOrPut(.{ .field_map = field_map_index }),
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
    const pointer_type = b.pool.indexToType(b.typeOf(function)).pointer;
    const function_type = b.pool.indexToType(pointer_type.pointee).function;
    const params = b.pool.extra.items[function_type.params.start..function_type.params.end];
    try b.mapInterned(inst, @enumFromInt(params[data.index]));
}

fn elementType(b: *Block, inst: Fir.Index) !void {
    const element_type = b.fir.get(inst);
    const data = element_type.data.element_type;
    const parent = b.resolveInst(data.parent);
    const ty = b.sema.tempAir().typeOf(parent);
    const element = switch (b.pool.indexToKey(ty).ty) {
        .array => |arr| arr.element,
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
        .errors = sema.errors,
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

fn reftoptr(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const reftoptr_inst = fir.get(inst);
    const data = reftoptr_inst.data.reftoptr;

    const operand = b.resolveInst(data);
    const operand_type = b.pool.indexToType(b.typeOf(operand)).ref;
    const ptr_type = try b.pool.getOrPutType(.{ .pointer = .{ .pointee = operand_type.pointee, .mutable = operand_type.mutable } });

    const air_inst = try b.add(.{ .reftoptr = .{
        .operand = operand,
        .ty = ptr_type,
    } });
    try b.mapInst(inst, air_inst);
}

fn ptrtoref(b: *Block, inst: Fir.Index) !void {
    const fir = b.fir;
    const ptrtoref_inst = fir.get(inst);
    const data = ptrtoref_inst.data.ptrtoref;

    const operand = b.resolveInst(data);
    const operand_type = b.pool.indexToType(b.typeOf(operand)).pointer;
    const ptr_type = try b.pool.getOrPutType(.{ .ref = .{ .pointee = operand_type.pointee, .mutable = operand_type.mutable } });

    const air_inst = try b.add(.{ .ptrtoref = .{
        .operand = operand,
        .ty = ptr_type,
    } });
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
    const sema = b.sema;
    const push_inst = b.fir.get(inst);
    var mutable: bool = undefined;
    var data: Fir.Index = undefined;
    switch (push_inst.data) {
        .push => |operand| {
            data = operand;
            mutable = false;
        },
        .push_mut => |operand| {
            data = operand;
            mutable = true;
        },
        else => unreachable,
    }

    const operand = b.resolveInst(data);
    const ty = b.typeOf(operand);
    if (b.pool.indexToType(ty).size(b.pool) == null) {
        // unsized types cannot be pushed
        try sema.errors.append(sema.gpa, .{
            .tag = .unsized_type_alloc,
            .token = sema.fir.tree.mainToken(push_inst.loc.node),
        });
        return error.HandledUserError;
    }

    const alloc = try pushInner(b, operand, mutable);
    try b.mapInst(inst, alloc);
}

fn pushInner(b: *Block, operand: Air.Index, mutable: bool) !Air.Index {
    const ty = b.typeOf(operand);
    const ref_type = try b.pool.getOrPut(.{ .ty = .{ .ref = .{ .pointee = ty, .mutable = mutable } } });
    const alloc = switch (mutable) {
        false => try b.add(.{ .alloc = .{
            .slot_type = ty,
            .pointer_type = ref_type,
        } }),
        true => try b.add(.{ .alloc_mut = .{
            .slot_type = ty,
            .pointer_type = ref_type,
        } }),
    };
    _ = try b.add(.{ .store = .{
        .ptr = alloc,
        .val = operand,
    } });
    return alloc;
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

fn loadLazy(b: *Block, inst: Fir.Index) !void {
    const load_inst = b.fir.get(inst);
    const data = load_inst.data.load_lazy;

    const ptr = b.resolveInst(data.ptr);
    const air_inst = try b.add(.{ .load_lazy = .{
        .ptr = ptr,
    } });
    try b.mapInst(inst, air_inst);
}

fn store(b: *Block, inst: Fir.Index) !void {
    const sema = b.sema;
    const store_inst = b.fir.get(inst);
    const data = store_inst.data.store;

    const ptr = b.resolveInst(data.ptr);
    const val_inner = b.resolveInst(data.val);
    const ref_type = b.pool.indexToType(b.typeOf(ptr)).ref;
    if (!ref_type.mutable) {
        try sema.errors.append(sema.gpa, .{
            .tag = .const_pointer_write,
            .token = sema.fir.tree.mainToken(store_inst.loc.node),
        });
        return error.HandledUserError;
    }
    const pointee_type = b.pool.indexToType(ref_type.pointee);
    const val = try coerceInnerImplicit(b, val_inner, pointee_type);

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
    const decl_index = b.pool.indexToKey(ip_index).decl;
    const decl = b.pool.decls.at(@intFromEnum(decl_index));
    const ty = decl.ty;
    const pointer_type = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = ty, .mutable = decl.mutable } } });
    const air_inst = try b.add(.{ .load_decl = .{
        .ip_index = ip_index,
        .ty = pointer_type,
    } });
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

fn binaryInner(b: *Block, l: Air.Index, r: Air.Index, comptime tag: std.meta.Tag(Air.Inst)) !Air.Index {
    // tries to comptime an operation, and if not possible, emits an instruction
    const ltag = b.sema.insts.items(.tags)[@intFromEnum(l)];
    const rtag = b.sema.insts.items(.tags)[@intFromEnum(r)];
    if (ltag != .constant or rtag != .constant) {
        // can't comptime, emit the instruction
        return b.add(@unionInit(Air.Inst, @tagName(tag), .{ .l = l, .r = r }));
    }

    // we *can* comptime, so dispatch the correct instruction to the generic alu
    const lconst = b.sema.insts.items(.data)[@intFromEnum(l)].constant;
    const rconst = b.sema.insts.items(.data)[@intFromEnum(r)].constant;
    const ltv = b.pool.indexToKey(lconst).tv;
    const rtv = b.pool.indexToKey(rconst).tv;
    // const lty = b.pool.indexToKey(ltv.ty).ty;
    // const rty = b.pool.indexToKey(rtv.ty).ty;

    // at this point, we expect both types to be the same
    // so we just switch on the left one
    const result = switch (tag) {
        .add => @addWithOverflow(ltv.val.integer, rtv.val.integer)[0],
        .sub => @subWithOverflow(ltv.val.integer, rtv.val.integer)[0],
        .mul => @mulWithOverflow(ltv.val.integer, rtv.val.integer)[0],
        .div => @divTrunc(ltv.val.integer, rtv.val.integer),
        .mod => @rem(ltv.val.integer, rtv.val.integer),
        .lsl => @shlWithOverflow(ltv.val.integer, rtv.val.integer)[0],
        .lsr => unreachable, // TODO
        else => unreachable,
    };
    // const result = switch (tag) {
    //     .add => switch (lty) {
    //         .int => |int| switch (int.sign) {
    //             .unsigned => try alu.uadd(ltv.val.integer, rtv.val.integer),
    //             .signed => try alu.sadd(ltv.val.integer, rtv.val.integer),
    //         },
    //         .float => unreachable, // big sadge, not implemented
    //         else => unreachable,
    //     },
    //     .sub => try alu.sadd(ltv.val.integer, alu.negate(rtv.val.integer)),
    //     .mul => switch (lty) {
    //         .int => |int| switch (int.sign) {
    //             .unsigned => try alu.umul(ltv.val.integer, rtv.val.integer),
    //             .signed => try alu.smul(ltv.val.integer, rtv.val.integer),
    //         },
    //         .float => unreachable, // big sadge, not implemented
    //         else => unreachable,
    //     },
    //     .div => switch (lty) {
    //         .int => |int| switch (int.sign) {
    //             .unsigned => try alu.udiv(ltv.val.integer, rtv.val.integer),
    //             .signed => try alu.sdiv(ltv.val.integer, rtv.val.integer),
    //         },
    //         .float => unreachable, // big sadge, not implemented
    //         else => unreachable,
    //     },
    //     .mod => unreachable, // not implemented
    //     // .lsl => try
    //     else => unreachable,
    // };

    return b.addConstant(.{ .tv = .{
        .ty = ltv.ty,
        .val = .{ .integer = result },
    } });
}

fn binaryArithOp(b: *Block, inst: Fir.Index, comptime tag: std.meta.Tag(Fir.Inst.Data)) !void {
    const sema = b.sema;
    const fir = b.fir;
    const binary = fir.get(inst);
    const data = @field(binary.data, @tagName(tag));

    const temp_air = b.sema.tempAir();
    var l = b.resolveInst(data.l);
    var r = b.resolveInst(data.r);
    const lty = b.pool.indexToKey(temp_air.typeOf(l)).ty;
    const rty = b.pool.indexToKey(temp_air.typeOf(r)).ty;

    const air_tag: std.meta.Tag(Air.Inst) = switch (tag) {
        .add => .add,
        .sub => .sub,
        .mul => .mul,
        .div => .div,
        .mod => .mod,
        .bitwise_or => .bitwise_or,
        .bitwise_and => .bitwise_and,
        .bitwise_xor => .bitwise_xor,
        .logical_xor => .bitwise_xor, // TODO: do we want to split this into a separate air instruction?
        .sl, .sr => unreachable,
        else => undefined,
    };

    const dest_type = coercion.resolvePeerTypes(b.pool, &.{ lty, rty }).?;
    switch (dest_type) {
        .comptime_int, .int, .comptime_float, .float, .bool => {}, // TODO: actually, we shouldn't allow these to mix
        else => {
            try sema.errors.append(sema.gpa, .{
                .tag = .operator_invalid,
                .token = sema.fir.tree.mainToken(binary.loc.node),
            });
            return error.HandledUserError;
        },
    }
    l = try coerceInnerImplicit(b, l, dest_type);
    r = try coerceInnerImplicit(b, r, dest_type);
    const new_arith = try binaryInner(b, l, r, air_tag);
    try b.mapInst(inst, new_arith);
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

    const dest_type = coercion.resolvePeerTypes(b.pool, &.{ lty, rty }).?; // TODO: emit error if no peer type found
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

pub fn logicalNot(b: *Block, inst: Fir.Index) !void {
    const not = b.fir.get(inst);
    const data = not.data.logical_not;

    const inner = b.resolveInst(data);
    const operand = try coerceInnerImplicit(b, inner, Type.bool_type);
    const air_inst = try b.add(.{ .logical_not = operand });
    try b.mapInst(inst, air_inst);
}

pub fn bitwiseNot(b: *Block, inst: Fir.Index) !void {
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
    const exec_false = try b.sema.analyzeBodyBlock(branch.exec_false);

    const pl = try b.sema.addExtra(Air.Inst.BranchDouble{
        .exec_true = exec_true,
        .exec_false = exec_false,
    });
    const air_branch = try b.add(.{ .branch_double = .{
        .cond = cond,
        .pl = pl,
    } });

    const true_type = b.pool.indexToType(b.typeOf(exec_true));
    const false_type = b.pool.indexToType(b.typeOf(exec_false));
    const dest_type = coercion.resolvePeerTypes(b.pool, &.{ true_type, false_type }).?;
    const air_coerce = try coerceInnerImplicit(b, air_branch, dest_type);
    try b.mapInst(inst, air_coerce);
}

pub fn loopForever(b: *Block, inst: Fir.Index) !void {
    const loop_inst = b.fir.get(inst);
    const data = loop_inst.data.loop_forever;

    const body = try b.sema.analyzeBodyBlock(data.body);
    const air_inst = try b.add(.{ .loop_forever = .{
        .body = body,
    } });
    try b.mapInst(inst, air_inst);
}

pub fn loopWhile(b: *Block, inst: Fir.Index) !void {
    const loop_inst = b.fir.get(inst);
    const data = loop_inst.data.loop_while;

    const cond = try b.sema.analyzeBodyBlock(data.cond);
    const body = try b.sema.analyzeBodyBlock(data.body);
    const air_inst = try b.add(.{ .loop_while = .{
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

fn indexVal(b: *Block, inst: Fir.Index) !void {
    const sema = b.sema;
    const index_val = b.fir.get(inst);
    const data = index_val.data.index_val;
    const base = b.resolveInst(data.base);
    const index_inner = b.resolveInst(data.index);
    const index = try coerceInnerImplicit(b, index_inner, Type.u64_type);

    var base_type = b.pool.indexToType(b.typeOf(base));
    var is_ref = false;
    var ref_mutable: bool = undefined;
    if (@as(Type.Tag, base_type) == .ref) {
        ref_mutable = base_type.ref.mutable;
        // fir wants ref semantics, so we have one level of indirection
        base_type = b.pool.indexToType(base_type.ref.pointee);
        is_ref = true;
    }

    switch (base_type) {
        // reading a field from a value by value - emit a val
        .array => |arr| {
            const access = if (is_ref) access: {
                // we want to avoid loading the entire aggregate (array) into a temporary,
                // so we instead get a reference to the element and then perform a load
                // this translates to a GEP in LLVM
                // TODO: this code path probably shouldn't exist anymore
                const ref = try b.addIndexRef(base, index, arr.element, ref_mutable);
                break :access try b.add(.{ .load = .{ .ptr = ref } });
            } else try b.addIndexVal(base, index);
            try b.mapInst(inst, access);
        },
        .slice => |slice| {
            // because of the extra level of indirection, we first load the
            // slice into a temporary, and then emit an index ref on that
            const val = if (is_ref) try b.add(.{ .load = .{ .ptr = base } }) else base;
            const ref = try b.addIndexVal(val, index);
            _ = slice;
            // const ptr = if (is_ref) {
            //     unreachable;
            // } else base: {
            //     const ty = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = slice.element, .mutable = false } } });
            //     break :base try b.add(.{ .slice_ptr_val = .{
            //         .base = base,
            //         .ty = ty,
            //     } });
            // };
            // const ref = if (is_ref) ref: {
            //     break :ref try b.addIndexRef(base, index, arr.element);
            // } else ref: {
            //     const val = try b.addIndexVal(base, index);
            //     break :ref try pushInner(b, val, false);
            // };
            try b.mapInst(inst, ref);
        },
        .many_pointer => |ptr| {
            const access = if (is_ref) access: {
                // we want to avoid loading the entire aggregate (array) into a temporary,
                // so we instead get a reference to the element and then perform a load
                // this translates to a GEP in LLVM
                const ref = try b.addIndexRef(base, index, ptr.pointee, ref_mutable);
                break :access try b.add(.{ .load = .{ .ptr = ref } });
            } else try b.addIndexVal(base, index);
            try b.mapInst(inst, access);
        },
        else => {
            try sema.errors.append(sema.gpa, .{
                .tag = .not_indexable,
                .token = sema.fir.tree.mainToken(index_val.loc.node),
            });
            return error.HandledUserError;
        },
    }
}

fn indexRef(b: *Block, inst: Fir.Index) !void {
    const sema = b.sema;
    const index_val = b.fir.get(inst);
    const data = index_val.data.index_ref;
    const base = b.resolveInst(data.base);
    const index_inner = b.resolveInst(data.index);
    const index = try coerceInnerImplicit(b, index_inner, Type.u64_type);

    var base_type = b.pool.indexToType(b.typeOf(base));
    var is_ref = false;
    var ref_mutable: bool = undefined;
    if (@as(Type.Tag, base_type) == .ref) {
        // fir wants ref semantics, so we have one level of indirection
        ref_mutable = base_type.ref.mutable;
        base_type = b.pool.indexToType(base_type.ref.pointee);
        is_ref = true;
    }

    switch (base_type) {
        .array => |arr| {
            const ref = if (is_ref) ref: {
                break :ref try b.addIndexRef(base, index, arr.element, ref_mutable);
            } else ref: {
                // TODO: should this case actually be used?
                const ptr = try pushInner(b, base, false);
                break :ref try b.addIndexRef(ptr, index, arr.element, true); // TODO: array mutable
                // const val = try b.addIndexVal(base, index);
                // break :ref try pushInner(b, val, false);
            };
            try b.mapInst(inst, ref);
        },
        .slice => |slice| {
            // because of the extra level of indirection, we first load the
            // slice into a temporary, and then emit an index ref on that
            const val = try b.add(.{ .load = .{ .ptr = base } });
            const ref = try b.addIndexRef(val, index, slice.element, true); // TODO: slice mutable
            try b.mapInst(inst, ref);
        },
        .many_pointer => |ptr| {
            const ref = if (is_ref) ref: {
                break :ref try b.addIndexRef(base, index, ptr.pointee, ref_mutable);
            } else ref: {
                const val = try b.addIndexVal(base, index);
                break :ref try pushInner(b, val, false);
            };
            try b.mapInst(inst, ref);
        },
        else => {
            try sema.errors.append(sema.gpa, .{
                .tag = .not_indexable,
                .token = sema.fir.tree.mainToken(index_val.loc.node),
            });
            return error.HandledUserError;
        },
    }
}

fn firSlice(b: *Block, inst: Fir.Index) !void {
    const slice_inst = b.fir.get(inst);
    const slice_data = slice_inst.data.slice;
    const data = b.fir.extraData(Fir.Inst.Slice, slice_data.pl);

    const base = b.resolveInst(data.base);
    const start_inner = b.resolveInst(data.start);
    const start = try coerceInnerImplicit(b, start_inner, Type.u64_type);
    const end_inner = b.resolveInst(data.end);
    const end = try coerceInnerImplicit(b, end_inner, Type.u64_type);

    var base_type = b.pool.indexToType(b.typeOf(base));
    var is_ref = false;
    if (@as(Type.Tag, base_type) == .ref) {
        // fir wants ref semantics, so we have one level of indirection
        base_type = b.pool.indexToType(base_type.ref.pointee);
        is_ref = true;
    }

    switch (base_type) {
        .array => |arr| {
            // we construct a slice using a gep (base + start) for the ptr
            // and a len (end - start + 1)
            // TODO: should this be mutable or not
            const ptr = ptr: {
                if (is_ref) {
                    const element_ptr = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = arr.element, .mutable = true } } });
                    break :ptr try b.addIndexRef(base, start, element_ptr, false); // TODO: mutable
                } else {
                    const alloc = try pushInner(b, base, false);
                    const element_ptr = try b.pool.getOrPut(.{ .ty = .{ .pointer = .{ .pointee = arr.element, .mutable = false } } });
                    break :ptr try b.addIndexRef(alloc, start, element_ptr, false);
                }
            };
            const len = len: {
                const sub = try b.add(.{ .sub = .{ .l = end, .r = start } });
                const one = try b.add(.{ .constant = .u64_one });
                const inc = try b.add(.{ .add = .{ .l = sub, .r = one } });
                break :len inc;
            };

            const slice_type = try b.pool.getOrPut(.{ .ty = .{
                .slice = .{ .element = arr.element },
            } });
            const pl = try b.sema.addExtra(Air.Inst.SliceInit{
                .ptr = ptr,
                .len = len,
                .ty = slice_type,
            });

            const ref = try b.add(.{ .slice_init = .{ .pl = pl } });
            try b.mapInst(inst, ref);
        },
        .slice => |slice| {
            const val = if (is_ref) try b.add(.{ .load = .{ .ptr = base } }) else base;
            const ptr = try b.addIndexRef(val, start, slice.element, false); // TODO: mutable
            const len = len: {
                const sub = try b.add(.{ .sub = .{ .l = end, .r = start } });
                const one = try b.add(.{ .constant = .u64_one });
                const inc = try b.add(.{ .add = .{ .l = sub, .r = one } });
                break :len inc;
            };

            const slice_type = try b.pool.getOrPut(.{ .ty = .{ .slice = slice } });
            const pl = try b.sema.addExtra(Air.Inst.SliceInit{
                .ptr = ptr,
                .len = len,
                .ty = slice_type,
            });

            const ref = try b.add(.{ .slice_init = .{ .pl = pl } });
            try b.mapInst(inst, ref);
        },
        .many_pointer => unreachable, // unimplemented
        else => unreachable, // TODO: emit error
    }
}

// fn fieldRef(analysis: *BlockAnalysis, inst: Hir.Index) !void {
//     const hg = analysis.hg;
//     const b = analysis.b;
//     const data = hg.get(inst, .field_ref);
//     // TODO: use interner value instead
//     const field_token = hg.tree.mainToken(data.node) + 1;
//     const field_string = hg.tree.tokenString(field_token);
//
//     switch (hg.insts.items(.tag)[data.operand]) {
//         // reading a field from a pointer by ref - emit a ref
//         .push, .alloca => {
//             const pointer_type = (try hg.resolveType(data.operand)).extended.cast(Type.Pointer).?;
//             const src_type = pointer_type.pointee;
//             // replace the generic field reference by a specific one - builtin, slice, array, structure
//             switch (src_type.kind()) {
//                 // slices only have two runtime fields - ptr and len
//                 .slice => {
//                     if (std.mem.eql(u8, field_string, "ptr")) {
//                         const access = try b.add(.slice_ptr_ref, .{
//                             .operand = data.operand,
//                             .node = data.node,
//                         });
//                         try analysis.src_block.replaceAllUsesWith(inst, access);
//                     } else if (std.mem.eql(u8, field_string, "len")) {
//                         const access = try b.add(.slice_len_ref, .{
//                             .operand = data.operand,
//                             .node = data.node,
//                         });
//                         try analysis.src_block.replaceAllUsesWith(inst, access);
//                     } else {
//                         std.log.err("field access: no such field {s} for type slice", .{field_string});
//                         unreachable;
//                     }
//                 },
//                 .array, .structure => unreachable, // unimplemented
//                 else => unreachable,
//             }
//         },
//         // reading a field from a value by ref - emit a val + alloc + store
//         else => {
//             const src_type = try hg.resolveType(data.operand);
//             // replace the generic field reference by a specific one - builtin, slice, array, structure
//             switch (src_type.kind()) {
//                 // slices only have two runtime fields - ptr and len
//                 .slice => {
//                     if (std.mem.eql(u8, field_string, "ptr")) {
//                         const access = try b.add(.slice_ptr_val, .{
//                             .operand = data.operand,
//                             .node = data.node,
//                         });
//                         const ptr_type = try Type.Pointer.init(hg.gpa, src_type.extended.cast(Type.Slice).?.element);
//                         const slot_type = try Type.Pointer.init(hg.gpa, ptr_type);
//                         const push = try b.add(.push, .{
//                             .ty = slot_type,
//                             .operand = access,
//                             .node = data.node,
//                         });
//                         try analysis.src_block.replaceAllUsesWith(inst, push);
//                     } else if (std.mem.eql(u8, field_string, "len")) {
//                         const access = try b.add(.slice_len_val, .{
//                             .operand = data.operand,
//                             .node = data.node,
//                         });
//                         const slot_type = try Type.Pointer.init(hg.gpa, Type.Common.u64_type);
//                         const push = try b.add(.push, .{
//                             .ty = slot_type,
//                             .operand = access,
//                             .node = data.node,
//                         });
//                         try analysis.src_block.replaceAllUsesWith(inst, push);
//                     } else {
//                         std.log.err("field access: no such field {s} for type slice", .{field_string});
//                         unreachable;
//                     }
//                 },
//                 .array, .structure => unreachable, // unimplemented
//                 else => unreachable,
//             }
//         },
//     }
// }

fn fieldVal(b: *Block, inst: Fir.Index) !void {
    const field_val = b.fir.get(inst);
    const data = field_val.data.field_val;
    var base = b.resolveInst(data.base);
    const field_string = b.pool.getString(data.field).?;

    var base_type = b.pool.indexToType(b.typeOf(base));
    var is_ref = false;
    if (@as(Type.Tag, base_type) == .ref) {
        // fir wants ref semantics, so we have one level of indirection
        base_type = b.pool.indexToType(base_type.ref.pointee);
        is_ref = true;
    }

    switch (base_type) {
        .slice => |slice| {
            // slices only have two runtime fields - ptr and len
            if (std.mem.eql(u8, field_string, "ptr")) {
                // TODO: should this be mutable or not
                const ty = try b.pool.getOrPut(.{ .ty = .{ .many_pointer = .{ .pointee = slice.element } } });
                if (is_ref) base = try b.add(.{ .load = .{ .ptr = base } });
                const access = try b.add(.{ .slice_ptr_val = .{
                    .base = base,
                    .ty = ty,
                } });
                try b.mapInst(inst, access);
            } else if (std.mem.eql(u8, field_string, "len")) {
                if (is_ref) base = try b.add(.{ .load = .{ .ptr = base } });
                const access = try b.add(.{ .slice_len_val = .{
                    .base = base,
                    .ty = .u64_type,
                } });
                try b.mapInst(inst, access);
            } else {
                std.log.err("field access: no such field {s} for type slice", .{field_string});
                unreachable;
            }
        },
        .array => |arr| {
            // arrays only have one comptime field - len
            if (std.mem.eql(u8, field_string, "len")) {
                const len = try b.addConstant(.{ .tv = .{
                    .ty = .u64_type,
                    .val = .{ .integer = arr.count },
                } });
                try b.mapInst(inst, len);
            } else {
                std.log.err("field access: no such field {s} for type array", .{field_string});
                unreachable;
            }
        },
        .@"struct" => |st| {
            // TODO: check that the field exists
            _ = st;
            // if (is_ref) {
            //     const ref = try b.add(.{ .field_ref = .{
            //         .base = base,
            //         .name = data.field,
            //     } })
            // }
            const access = try b.add(.{ .field_val = .{
                .base = base,
                .name = data.field,
            } });
            try b.mapInst(inst, access);
        },
        else => unreachable,
    }
}

pub fn tempAir(sema: *Sema) Air {
    return .{
        .insts = sema.insts.slice(),
        .extra = sema.extra.items,
        .pool = sema.pool,
        .toplevel = undefined,
    };
}
