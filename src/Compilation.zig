const std = @import("std");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");
const Analyzer = @import("Analyzer.zig");
const Type = @import("typing.zig").Type;
const Value = @import("value.zig").Value;
const Driver = @import("Driver.zig");
const llvm = @import("codegen/llvm/llvm.zig");
const DeclGen = @import("codegen/llvm/DeclGen.zig"); // TODO: encapsulate
const CodeGen = @import("codegen/llvm/CodeGen.zig"); // TODO: encapsulate
const render = @import("render.zig");

const Allocator = std.mem.Allocator;

pub const Decl = struct {
    name: [*:0]const u8,
    ty: Type,
    val: Value,

    pub const Index = u32;

    pub const U32 = struct {
        decl: Decl.Index,
        hir_inst: Hir.Index,
    };

    pub const Function = struct {
        decl: Decl.Index,
        hir_inst: Hir.Index,
    };
};


const Compilation = @This();
const InlineMap = std.AutoHashMapUnmanaged(Hir.Index, Decl.Index);

gpa: Allocator,
arena: Allocator,
config: *Driver.Configuration,
hir: *const Hir,

decls: std.SegmentedList(Decl, 0),
globals: std.AutoHashMapUnmanaged(Hir.Index, Decl.Index),
backend: *llvm.Backend,

pub fn compile(gpa: Allocator, hir: *const Hir, config: *Driver.Configuration) !void {
    var comp = try gpa.create(Compilation);
    defer gpa.destroy(comp);

    var backend = llvm.Backend.create(gpa, comp, "femto_main");
    defer backend.destroy();

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    comp.* = Compilation {
        .gpa = gpa,
        .arena = arena.allocator(),
        .config = config,
        .hir = hir,
        .decls = .{},
        .globals = .{},
        .backend = &backend,
    };

    const module_index = @intCast(u32, hir.insts.len - 1);
    try comp.compileModule(module_index);

    if (config.verbose_llvm_ir) backend.module.print();
    try backend.module.verify();

    if (config.emit_llvm) {
        if (config.stage == .assembly) {
            try backend.module.writeBitcode(config.output);
        } else {
            try backend.module.writeIR(config.output);
        }
    }
}

pub fn addDecl(comp: *Compilation, decl: Decl) !u32 {
    const index = comp.decls.count();
    var ptr = try comp.decls.addOne(comp.gpa);
    ptr.* = decl;

    return index;
}

pub fn allocateDecl(comp: *Compilation) !u32 {
    const count = @intCast(u32, comp.decls.count());
    _ = try comp.decls.addOne(comp.gpa);
    return count;
}

pub inline fn declPtr(comp: *Compilation, index: u32) *Decl {
    std.debug.assert(index < comp.decls.count());
    return comp.decls.at(index);
}

fn compileModule(comp: *Compilation, module_inst: Hir.Index) !void {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[module_inst].pl_node.pl;
    const module = hir.extraData(pl, Hir.Inst.Module);

    const ids = hir.extra_data[pl + 1..pl + 1 + module.len];
    const insts = hir.extra_data[pl + 1 + module.len..pl + 1 + 2 * module.len];
    for (insts) |inst| {
        const index = try comp.allocateDecl();
        try comp.globals.put(comp.gpa, inst, index);
    }

    // todo: zig 0.11 multi object for loops
    for (insts) |inst, i| {
        const decl_index = comp.globals.get(inst).?;
        const decl_ptr = comp.declPtr(decl_index);
        try comp.blockInline(inst, decl_index);
        const member_str = try hir.interner.get(ids[i]);
        // TODO: figure out memory lifetimes for previous and current name
        const name = try std.mem.joinZ(comp.gpa, "", &.{member_str});
        decl_ptr.name = name.ptr;
        try comp.backend.updateDecl(decl_ptr);
    }
}

pub fn refToType(ref: Hir.Ref) Type {
    if (Hir.Inst.refToIndex(ref)) |index| {
        _ = index;
        unreachable;
        // std.debug.assert(mir.insts.items(.tag)[index] == .ty);
        // return mir.insts.items(.data)[index].ty;
    } else {
        return switch (ref) {
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
            .void_ty => Type.initVoid(),
            .zero_val, .one_val, .btrue_val, .bfalse_val,
            .void_val => unreachable,
            _ => unreachable,
        };
    }
}

fn blockInline(comp: *Compilation, block_inst: Hir.Index, decl_index: Decl.Index) !void {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[block_inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Block);

    const insts = hir.extra_data[pl + 1..pl + 1 + data.len];
    for (insts) |inst| {
        const decl = switch (hir.insts.items(.tag)[inst]) {
            .fn_decl => try comp.fnDecl(inst, decl_index),
            .yield_inline => comp.yieldInline(inst),
            else => unreachable,
        };
        try comp.globals.put(comp.arena, inst, decl);
    }
}

fn yieldInline(comp: *Compilation, inst: Hir.Index) Decl.Index {
    const data = comp.hir.insts.items(.data)[inst];
    const index = Hir.Inst.refToIndex(data.un_node.operand).?; // TODO: non-index refs
    return comp.globals.get(index).?;
}

fn fnBody(comp: *Compilation, fn_decl: *const Hir.Inst.FnDecl) !Mir {
    var arena = std.heap.ArenaAllocator.init(comp.gpa);
    defer arena.deinit();

    var analyzer = Analyzer {
        .comp = comp,
        .gpa = comp.gpa,
        .arena = arena.allocator(),
        .map = .{},
        .hir = comp.hir,
        .instructions = .{},
        .extra = .{},
        .values = .{},
        .scratch = .{},
        .errors = .{},
        .interner = &comp.hir.interner,
    };
    var block = Analyzer.Block {
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
    const mir = Mir {
        .insts = analyzer.instructions.toOwnedSlice(),
        .extra = analyzer.extra.toOwnedSlice(analyzer.gpa),
        .values = analyzer.values.toOwnedSlice(analyzer.gpa),
        .interner = &comp.hir.interner,
        .comp = analyzer.comp,
    };
    return mir;
}

fn fnDecl(comp: *Compilation, function_inst: Hir.Index, decl_index: Decl.Index) !Decl.Index {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[function_inst].pl_node.pl;
    const fn_decl = hir.extraData(pl, Hir.Inst.FnDecl);

    const name = name: {
        var hash: [16]u8 = undefined;
        _ = try std.fmt.bufPrint(&hash, "{x}{x}", .{fn_decl.hash_upper, fn_decl.hash_lower});
        break :name try std.mem.joinZ(comp.gpa, "_", &[_][]const u8{ "f", &hash });
    };

    // this needs to be run before fnBody
    const fn_type = try comp.initFunctionType(function_inst);

    const function_decl = try comp.gpa.create(Decl.Function);
    function_decl.* = .{ .decl = decl_index, .hir_inst = function_inst };
    const function_val = try comp.gpa.create(Value.Payload.Function);
    function_val.* = .{ .func = function_decl };

    const decl = Decl {
        .name = name.ptr,
        .ty = fn_type,
        .val = .{ .payload = &function_val.base },
    };
    const decl_ptr = comp.declPtr(decl_index);
    decl_ptr.* = decl;

    const mir = try comp.fnBody(&fn_decl);
    if (comp.config.verbose_mir) {
        const out = std.io.getStdOut();
        var buffered_out = std.io.bufferedWriter(out.writer());
        var writer = buffered_out.writer();
        var mir_renderer = render.MirRenderer(2, @TypeOf(writer)).init(writer, &mir);
        try mir_renderer.render();
        try buffered_out.flush();
    }

    try comp.backend.updateDecl(decl_ptr);
    try comp.backend.generateBody(decl_ptr, &mir);

    return decl_index;
}

fn initFunctionType(comp: *Compilation, function_inst: Hir.Index) !Type {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[function_inst].pl_node.pl;
    const fn_decl = hir.extraData(pl, Hir.Inst.FnDecl);

    const param_types = try comp.gpa.alloc(Type, fn_decl.params_end - fn_decl.params_start);
    const params = hir.extra_data[fn_decl.params_start..fn_decl.params_end];
    for (params) |inst, i| {
        const param_pl = hir.insts.items(.data)[inst].pl_node.pl;
        const param = hir.extraData(param_pl, Hir.Inst.Param);
        param_types[i] = refToType(param.ty);
    }

    const return_type = refToType(fn_decl.return_ty);
    return Type.Function.init(comp.gpa, return_type, param_types);
}

fn declName(comp: *Compilation, inst: Hir.Index) !void {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const dbg_value = hir.extraData(pl, Hir.Inst.DebugValue);

    const name = try hir.interner.get(dbg_value.name);
    const buf = try std.mem.joinZ(comp.gpa, "", &[_][]const u8{ name });
    const value = Hir.Inst.refToIndex(dbg_value.value).?;
    const owned_decl = comp.globals.get(value).?;
    const decl_ptr = comp.declPtr(owned_decl);

    const decl_index = try comp.allocateDecl();
    const decl = comp.declPtr(decl_index);

    const ref_val = try comp.gpa.create(Value.Payload.Reference);
    ref_val.* = .{ .ref = owned_decl };
    decl.name = buf;
    decl.ty = decl_ptr.ty;
    decl.val = .{ .payload = &ref_val.base };

    try comp.backend.updateDecl(decl);
}
