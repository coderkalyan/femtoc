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

gpa: Allocator,
config: *Driver.Configuration,
hir: *const Hir,

decls: std.SegmentedList(Decl, 0),
globals: std.AutoHashMapUnmanaged(Hir.Index, Decl.Index),
backend: struct {
    module: llvm.Module,
    globals: std.AutoHashMapUnmanaged(*Decl, llvm.Value),
},
hash: std.crypto.hash.Blake3,

pub fn compile(gpa: Allocator, hir: *const Hir, config: *Driver.Configuration) !void {
    const backend_module = llvm.createModule("femto_main");
    defer llvm.destroyModule(backend_module);

    var comp = Compilation {
        .gpa = gpa,
        .config = config,
        .hir = hir,
        .decls = .{},
        .globals = .{},
        .backend = .{
            .module = backend_module,
            .globals = .{},
        },
        .hash = std.crypto.hash.Blake3.init(.{}),
    };

    const module_index = @intCast(u32, hir.insts.len - 1);
    try comp.compileModule(module_index);

    var err = @intToPtr([*c]u8, 0);
    {
        const result = llvm.verifyModule(comp.backend.module, llvm.c.LLVMPrintMessageAction, &err);
        if (result != 0) {
            std.debug.print("analysis failed\n", .{});
            llvm.c.LLVMDisposeMessage(err);
        }
    }

    if (comp.config.verbose_llvm_ir) {
        llvm.dumpModule(comp.backend.module);
    }

    if (comp.config.emit_llvm) {
        if (comp.config.stage == .assembly) {
            const name = comp.config.output.ptr;
            const result = llvm.c.LLVMWriteBitcodeToFile(comp.backend.module, name);
            if (result != 0) {
                std.debug.print("llvm assembly failed\n", .{});
                llvm.c.LLVMDisposeMessage(err);
            }
        } else {
            const name = comp.config.output.ptr;
            const result = llvm.c.LLVMPrintModuleToFile(comp.backend.module, name, &err);
            if (result != 0) {
                std.debug.print("llvm emit failed\n", .{});
                llvm.c.LLVMDisposeMessage(err);
            }
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

    const insts = hir.extra_data[pl + 1..pl + 1 + module.len];
    for (insts) |inst| {
        switch (hir.insts.items(.tag)[inst]) {
            .fn_decl => try comp.fnDecl(inst),
            .dbg_value => try comp.declName(inst),
            else => {},
        }
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

fn fnDecl(comp: *Compilation, function_inst: Hir.Index) !void {
    const hir = comp.hir;
    const pl = hir.insts.items(.data)[function_inst].pl_node.pl;
    const fn_decl = hir.extraData(pl, Hir.Inst.FnDecl);

    const decl_index = try comp.allocateDecl();
    const decl = comp.declPtr(decl_index);
    try comp.globals.put(comp.gpa, function_inst, decl_index);
    var hash: [16]u8 = undefined;
    _ = try std.fmt.bufPrint(&hash, "{x}{x}", .{fn_decl.hash_upper, fn_decl.hash_lower});
    const name = try std.mem.joinZ(comp.gpa, "_", &[_][]const u8{ "f", &hash });
    decl.name = name.ptr;
    decl.ty = try comp.initFunctionType(function_inst);

    const function_decl = try comp.gpa.create(Decl.Function);
    function_decl.* = .{ .decl = decl_index, .hir_inst = function_inst };

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
        .interner = &hir.interner,
    };
    var block = Analyzer.Block {
        .parent = null,
        .analyzer = &analyzer,
        .instructions = .{},
    };
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
        .interner = &hir.interner,
    };

    if (comp.config.verbose_mir) {
        const out = std.io.getStdOut();
        var buffered_out = std.io.bufferedWriter(out.writer());
        var writer = buffered_out.writer();
        var mir_renderer = render.MirRenderer(2, @TypeOf(writer)).init(writer, &mir);
        try mir_renderer.render();
        try buffered_out.flush();
    }

    const function_val = try comp.gpa.create(Value.Payload.Function);
    function_val.* = .{ .func = function_decl };
    decl.val = .{ .payload = &function_val.base };

    var dg = DeclGen {
        .gpa = comp.gpa,
        .comp = comp,
        .decl = decl,
        .module = comp.backend.module,
    };
    try dg.generate();
    
    const builder = llvm.createBuilder();
    defer llvm.destroyBuilder(builder);
    var codegen = CodeGen {
        .gpa = comp.gpa,
        .comp = comp,
        .mir = &mir,
        .module = comp.backend.module,
        .map = .{},
        .function = llvm.c.LLVMGetNamedFunction(comp.backend.module, decl.name),
        .builder = builder,
    };
    try codegen.generate();
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
    // const owned_decl = ptr: {
        // const decl = comp.declPtr(decl_index);
        // std.debug.print("{}\n", .{decl});
        // break :index decl;

        // const base = decl.val.payload;
        // const function_val = Value.Payload.cast(base, Value.Payload.Function).?;
        // const function_decl = function_val.func;
        // break :index function_decl.decl;
    // } else unreachable;

    const decl_index = try comp.allocateDecl();
    const decl = comp.declPtr(decl_index);

    const ref_val = try comp.gpa.create(Value.Payload.Reference);
    ref_val.* = .{ .ref = owned_decl };
    decl.name = buf;
    decl.ty = decl_ptr.ty;
    decl.val = .{ .payload = &ref_val.base };

    var dg = DeclGen {
        .gpa = comp.gpa,
        .comp = comp,
        .decl = decl,
        .module = comp.backend.module,
    };
    try dg.generate();
}
