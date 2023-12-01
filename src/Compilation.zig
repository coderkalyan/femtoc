// const std = @import("std");
// const Hir = @import("Hir.zig");
// const Mir = @import("Mir.zig");
// const Analyzer = @import("Analyzer.zig");
// const Type = @import("typing.zig").Type;
// const Value = @import("value.zig").Value;
// const Driver = @import("Driver.zig");
// const llvm = @import("codegen/llvm/llvm.zig");
// const DeclGen = @import("codegen/llvm/DeclGen.zig"); // TODO: encapsulate
// const CodeGen = @import("codegen/llvm/CodeGen.zig"); // TODO: encapsulate
// const render = @import("render.zig");

// const Allocator = std.mem.Allocator;

// pub const Decl = struct {
//     name: [*:0]const u8,
//     ty: Type,
//     val: Value,
//     mut: bool, // TODO: this is probably sparse

//     pub const Index = u32;

//     pub const Global = struct {
//         decl: Decl.Index,
//         hir_inst: Hir.Index,
//     };

//     pub const Function = struct {
//         decl: Decl.Index,
//         hir_inst: Hir.Index,
//     };
// };

// const Compilation = @This();
// const InlineMap = std.AutoHashMapUnmanaged(Hir.Index, Decl.Index);

// gpa: Allocator,
// arena: Allocator,
// config: *Driver.Configuration,
// hir: *const Hir,

// // decls: std.SegmentedList(Decl, 0),
// // export_decls: std.AutoHashMapUnmanaged(Decl.Index, void),
// // globals: std.AutoHashMapUnmanaged(Hihttps://rndkbd.com/products/gazzew-u4-boba-switches?variant=43886967062762r.Index, Decl.Index),
// backend: *llvm.Backend,

// pub fn compile(gpa: Allocator, hir: *const Hir, config: *Driver.Configuration) !void {
//     var comp = try gpa.create(Compilation);
//     defer gpa.destroy(comp);
//     var backend = llvm.Backend.create(gpa, comp, "femto_main");
//     defer backend.destroy();

//     var arena = std.heap.ArenaAllocator.init(gpa);
//     defer arena.deinit();

//     comp.* = Compilation{
//         .gpa = gpa,
//         .arena = arena.allocator(),
//         .config = config,
//         .hir = hir,
//         .export_decls = .{},
//         .globals = .{},
//         .backend = &backend,
//     };

//     try comp.compileModule(hir.module_index);

//     if (config.verbose_llvm_ir) backend.module.print();
//     try backend.module.verify();

//     if (config.emit_llvm) {
//         if (config.stage == .assembly) {
//             try backend.module.writeBitcode(config.output);
//         } else {
//             try backend.module.writeIR(config.output);
//         }
//     }
// }

// pub fn addDecl(comp: *Compilation, decl: Decl) !u32 {
//     const index = comp.decls.count();
//     var ptr = try comp.decls.addOne(comp.gpa);
//     ptr.* = decl;

//     return index;
// }

// pub fn allocateDecl(comp: *Compilation) !u32 {
//     const count: u32 = @intCast(comp.decls.count());
//     _ = try comp.decls.addOne(comp.gpa);
//     return count;
// }

// pub inline fn declPtr(comp: *Compilation, index: u32) *Decl {
//     std.debug.assert(index < comp.decls.count());
//     return comp.decls.at(index);
// }

// fn compileModule(comp: *Compilation, module_inst: Hir.Index) !void {
//     const hir = comp.hir;

//     const module_pl = hir.insts.items(.data)[hir.module_index].pl_node.pl;
//     const module = hir.extraData(module_pl, Hir.Inst.Module);
//     _ = module_inst;

//     var extra_index: u32 = 0;
//     while (extra_index < module.len * 2) : (extra_index += 2) {
//         const base = module_pl + 1;
//         const id = hir.extra_data[base + extra_index];
//         const inst = hir.extra_data[base + extra_index + 1];
//         const member_str = try hir.interner.get(id);
//         _ = member_str;

//         try comp.blockInline(id, inst);
//         // try r.renderInst(inst);

//         // const decl_index = comp.globals.get(inst).?;
//         // const decl_ref = try analyzer.analyzeInlineBlock(&block, inst);
//         // const decl_inst = Mir.refToIndex(decl_ref).?;
//         // const decl_index = analyzer.instructions.items(.data)[decl_inst].pl;
//         // const decl_ptr = comp.declPtr(decl_index);
//         // try comp.globals.put(comp.gpa, inst, decl_index);

//         // TODO: move this to a rename decl instruction
//         // const member_str = try hir.interner.get(ids[i]);
//         // TODO: figure out memory lifetimes for previous and current name
//         // const name = try std.mem.joinZ(comp.gpa, "", &.{member_str});
//         // decl_ptr.name = name.ptr;
//         // try comp.backend.updateDecl(decl_index);
//     }

//     // todo: zig 0.11 multi object for loops
//     // for (insts, 0..) |inst, i| {
//     //     var arena = std.heap.ArenaAllocator.init(comp.gpa);
//     //     defer arena.deinit();

//     //     _ = inst;
//     //     _ = i;

//     // var analyzer = Analyzer{
//     //     .comp = comp,
//     //     .gpa = comp.gpa,
//     //     .arena = arena.allocator(),
//     //     .map = .{},
//     //     .hir = comp.hir,
//     //     .instructions = .{},
//     //     .extra = .{},
//     //     .values = .{},
//     //     .scratch = .{},
//     //     .errors = .{},
//     //     .interner = &comp.hir.interner,
//     // };
//     // var block = Analyzer.Block{
//     //     .parent = null,
//     //     .analyzer = &analyzer,
//     //     .instructions = .{},
//     // };

//     // const decl_index = comp.globals.get(inst).?;
//     // const decl_ref = try analyzer.analyzeInlineBlock(&block, inst);
//     // const decl_inst = Mir.refToIndex(decl_ref).?;
//     // const decl_index = analyzer.instructions.items(.data)[decl_inst].pl;
//     // const decl_ptr = comp.declPtr(decl_index);
//     // try comp.globals.put(comp.gpa, inst, decl_index);

//     // TODO: move this to a rename decl instruction
//     // const member_str = try hir.interner.get(ids[i]);
//     // TODO: figure out memory lifetimes for previous and current name
//     // const name = try std.mem.joinZ(comp.gpa, "", &.{member_str});
//     // decl_ptr.name = name.ptr;
//     // try comp.backend.updateDecl(decl_index);
//     // }
// }

// pub fn refToType(ref: Hir.Ref) Type {
//     if (Hir.Inst.refToIndex(ref)) |index| {
//         _ = index;
//         unreachable;
//         // std.debug.assert(mir.insts.items(.tag)[index] == .ty);
//         // return mir.insts.items(.data)[index].ty;
//     } else {
//         return switch (ref) {
//             .bool_ty => Type.initInt(1, false),
//             .i8_ty => Type.initInt(8, true),
//             .u8_ty => Type.initInt(8, false),
//             .i16_ty => Type.initInt(16, true),
//             .u16_ty => Type.initInt(16, false),
//             .i32_ty => Type.initInt(32, true),
//             .u32_ty => Type.initInt(32, false),
//             .i64_ty => Type.initInt(64, true),
//             .u64_ty => Type.initInt(64, false),
//             .f32_ty => Type.initFloat(32),
//             .f64_ty => Type.initFloat(64),
//             .void_ty => Type.initVoid(),
//             .zero_val, .one_val, .btrue_val, .bfalse_val, .void_val => unreachable,
//             _ => unreachable,
//         };
//     }
// }

// fn blockInline(comp: *Compilation, ident_id: u32, block_inst: Hir.Index) !void {
//     const hir = comp.hir;
//     const pl = hir.insts.items(.data)[block_inst].pl_node.pl;
//     const data = hir.extraData(pl, Hir.Inst.Block);

//     const insts = hir.block_slices[data.head];
//     for (insts) |inst| {
//         const decl = switch (hir.insts.items(.tag)[inst]) {
//             .fn_decl => try comp.fnDecl(inst, ident_id),
//             else => {},
//         };
//         _ = decl;
//     }
// }

// fn fnDecl(comp: *Compilation, inst: Hir.Index,)
