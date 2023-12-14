const std = @import("std");
const Hir = @import("../Hir.zig");
const Type = @import("../hir/type.zig").Type;
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const CodeGen = @import("CodeGen.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;

const DeclGen = @This();

gpa: Allocator,
arena: Allocator,
context: *Context,
hir: *const Hir,
name: u32,
inline_block: Hir.Index,
map: std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
global_map: *std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
// TODO: is LLVM thread safe?

pub fn generate(dg: *DeclGen, codegens: *std.ArrayList(CodeGen)) !c.LLVMValueRef {
    const hir = dg.hir;
    const block_pl = hir.insts.items(.data)[dg.inline_block].pl_node.pl;
    const block_data = hir.extraData(block_pl, Hir.Inst.Block);

    const insts = hir.block_slices[block_data.head];
    for (insts) |inst| {
        const ref = switch (hir.insts.items(.tag)[inst]) {
            .constant => ref: {
                const pl = hir.insts.items(.data)[inst].pl_node.pl;
                const data = hir.extraData(pl, Hir.Inst.Constant);
                const ty = try hir.resolveType(dg.gpa, data.ty);

                break :ref switch (ty.kind()) {
                    .function => try dg.function(inst, codegens),
                    .comptime_uint, .comptime_sint, .comptime_float => continue,
                    .uint, .sint, .float => try dg.literal(inst),
                    else => unreachable,
                };
            },
            .global_mut => ref: {
                const op = hir.insts.items(.data)[inst].un_node.operand;
                const g = dg.resolveInst(op);
                c.LLVMSetGlobalConstant(g, 0);
                break :ref g;
            },
            .yield_inline => {
                const operand = hir.insts.items(.data)[inst].un_node.operand;
                return dg.resolveInst(operand);
            },
            .ty => continue,
            .link_extern => ref: {
                const operand = hir.insts.items(.data)[inst].un_node.operand;
                const g = dg.resolveInst(operand);
                c.LLVMSetLinkage(g, c.LLVMExternalLinkage);
                break :ref g;
            },
            else => {
                std.debug.print("{}\n", .{hir.insts.items(.tag)[inst]});
                unreachable;
            },
        };
        try dg.map.put(dg.arena, inst, ref);
    }

    unreachable;
}

fn resolveInst(dg: *DeclGen, index: Hir.Index) c.LLVMValueRef {
    const hir = dg.hir;
    if (hir.insts.items(.tag)[index] == .load_global) {
        const pl = hir.insts.items(.data)[index].pl_node.pl;
        _ = pl;
        unreachable;
        // const ident = hir.interner.get(pl) catch unreachable;
        // TODO: we can probably remove global map and just use llvm to look it up
        // via the name
        // return dg.global_map.get(pl).?;
        // if (decl.val.kind() == .function) {
        //     return llvm.c.LLVMGetNamedFunction(backend.module.module, decl.name);
        // } else {
        //     return llvm.c.LLVMGetNamedGlobal(backend.module.module, decl.name);
        // }
    } else {
        std.debug.print("resolve: {}\n", .{index});
        return dg.map.get(index).?;
    }
}

fn function(dg: *DeclGen, inst: Hir.Index, codegens: *std.ArrayList(CodeGen)) !c.LLVMValueRef {
    const hir = dg.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const ty = try hir.resolveType(dg.gpa, data.ty);
    const val = hir.values[data.val];
    const func = val.extended.cast(Value.Function).?;

    const member_str = try hir.interner.get(dg.name);
    const name = try std.fmt.allocPrintZ(dg.arena, "{s}", .{member_str});
    defer dg.arena.free(name);

    const llvm_type = try dg.context.convertType(ty);
    const ref = dg.context.addFunction(name, llvm_type);
    c.LLVMSetLinkage(ref, c.LLVMInternalLinkage);

    const builder = try dg.gpa.create(Context.Builder);
    builder.* = Context.Builder.init(dg.context, ref);
    var codegen = CodeGen{
        .arena = dg.arena,
        .builder = builder,
        .hir = hir,
        .func = func,
        .map = .{},
        .global_map = dg.global_map,
    };
    try codegens.append(codegen);

    return ref;
}

fn literal(dg: *DeclGen, inst: Hir.Index) !c.LLVMValueRef {
    const hir = dg.hir;
    const pl = hir.insts.items(.data)[inst].pl_node.pl;
    const data = hir.extraData(pl, Hir.Inst.Constant);
    const ty = try hir.resolveType(dg.gpa, data.ty);
    const llvm_type = try dg.context.convertType(ty);
    const member_str = try hir.interner.get(dg.name);
    const name = try std.fmt.allocPrintZ(dg.arena, "{s}", .{member_str});
    defer dg.arena.free(name);

    const val = switch (ty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint => val: {
            const val = hir.instToInt(inst);
            break :val c.LLVMConstInt(llvm_type, @intCast(val), 0);
        },
        .sint => val: {
            const val = hir.instToInt(inst);
            break :val c.LLVMConstInt(llvm_type, @intCast(val), 1);
        },
        .float => val: {
            const val = hir.instToFloat(inst);
            break :val c.LLVMConstReal(llvm_type, val);
        },
        else => unreachable, // unimplemented
    };

    const g = dg.context.addGlobal(name, llvm_type);
    c.LLVMSetGlobalConstant(g, 1);
    c.LLVMSetInitializer(g, val);
    return g;
}
