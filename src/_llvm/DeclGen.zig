const std = @import("std");
const Hir = @import("../Hir.zig");
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const CodeGen = @import("CodeGen.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;

const DeclGen = @This();

arena: Allocator,
context: *Context,
hir: *const Hir,
name: u32,
inline_block: Hir.Index,
// TODO: is LLVM thread safe?
// builder: *llvm.Builder,

pub fn generate(dg: *DeclGen) !void {
    const hir = dg.hir;
    const block_pl = hir.insts.items(.data)[dg.inline_block].pl_node.pl;
    const block_data = hir.extraData(block_pl, Hir.Inst.Block);

    const insts = hir.block_slices[block_data.head];
    for (insts) |inst| {
        switch (hir.insts.items(.tag)[inst]) {
            .constant => {
                const pl = hir.insts.items(.data)[inst].pl_node.pl;
                const data = hir.extraData(pl, Hir.Inst.Constant);
                const ty = hir.resolveType(data.ty);

                switch (ty.kind()) {
                    .function => _ = try dg.function(inst),
                    else => unreachable,
                }
            },
            .yield_inline => {}, // TODO
            .ty => {},
            else => {
                std.debug.print("{}\n", .{hir.insts.items(.tag)[inst]});
                unreachable;
            },
        }
    }

    // if (dg.backend.globals.get(decl)) |val| {
    //     // already exists, update
    //     llvm.c.LLVMSetValueName2(val, decl.name, std.mem.len(decl.name));
    //     const is_exported = comp.export_decls.contains(dg.decl_index);
    //     const linkage = if (is_exported) llvm.c.LLVMExternalLinkage else llvm.c.LLVMInternalLinkage;
    //     llvm.c.LLVMSetLinkage(val, @intCast(linkage));
    // } else {
    //     // create the decl
    //     // TODO: switch from value kind to decl kind (need to create that enum)
    //     const val = switch (decl.val.kind()) {
    //         .function => try dg.function(),
    //         else => try dg.global(),
    //         // .zero,
    //         // .one,
    //         // .u32,
    //         // .u64,
    //         // .f64 => try dg.,
    //         // .reference => try dg.reference(),
    //         // else => unreachable,
    //         // else => return,
    //     };
    //     try dg.backend.globals.put(dg.gpa, decl, val);
    // }
}

fn function(dg: *DeclGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = dg.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const ty = hir.resolveType(data.ty);
    const val = hir.values[data.val];
    const payload = val.payload.cast(Value.Payload.Function).?;
    const func = payload.func;

    const member_str = try hir.interner.get(dg.name);
    const name = try std.fmt.allocPrintZ(dg.arena, "{s}", .{member_str});
    defer dg.arena.free(name);

    const llvm_type = try dg.context.convertType(ty);
    const ref = dg.context.addFunction(name, llvm_type);

    var builder = Context.Builder.init(dg.context, ref);
    defer builder.deinit();
    var codegen = CodeGen{
        .arena = dg.arena,
        .builder = &builder,
        .hir = hir,
        .func = func,
        .map = .{},
    };
    try codegen.generate();

    return ref;
}

// fn global(dg: *DeclGen) !llvm.c.LLVMValueRef {
//     const comp = dg.backend.comp;
//     const decl = comp.declPtr(dg.decl_index);
//     const llvm_type = try llvm.getType(dg.gpa, dg.backend.context.context, decl.ty);
//     const g = llvm.c.LLVMAddGlobal(dg.backend.module.module, llvm_type, decl.name);
//     llvm.c.LLVMSetGlobalConstant(g, @intFromBool(!decl.mut));

//     const val = switch (decl.ty.kind()) {
//         .uint => llvm.c.LLVMConstInt(llvm_type, @intCast(decl.val.toInt()), 0),
//         .sint => llvm.c.LLVMConstInt(llvm_type, @intCast(decl.val.toInt()), 1),
//         .float => llvm.c.LLVMConstReal(llvm_type, decl.val.toFloat()),
//         else => unreachable,
//     };
//     llvm.c.LLVMSetInitializer(g, val);
//     return g;
// }
