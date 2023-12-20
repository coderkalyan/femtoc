const std = @import("std");
const Hir = @import("../Hir.zig");
const Type = @import("../hir/type.zig").Type;
const Context = @import("Context.zig");
const Value = @import("../value.zig").Value;
const CodeGen = @import("CodeGen.zig");
const DeclInfo = @import("../hir/DeclInfo.zig");
const TypedValue = @import("../hir/TypedValue.zig");
const Allocator = std.mem.Allocator;
const c = Context.c;

const DeclGen = @This();

gpa: Allocator,
arena: Allocator,
context: *Context,
hir: *const Hir,
name: u32,
inline_block: Hir.Index,
global_map: *std.AutoHashMapUnmanaged(Hir.Index, c.LLVMValueRef),
decl_info: *DeclInfo,

pub fn generate(dg: *DeclGen, codegens: *std.ArrayList(CodeGen)) !c.LLVMValueRef {
    const hir = dg.hir;
    const info = dg.decl_info;

    const ident = try hir.interner.get(dg.name);
    const name = try std.fmt.allocPrintZ(dg.arena, "{s}", .{ident});
    defer dg.arena.free(name);

    const llvm_type = try dg.context.convertType(info.ty.?);
    const decl = switch (info.ty.?.kind()) {
        // these should be taken care of by the frontend (type_analysis)
        .comptime_uint,
        .comptime_sint,
        .comptime_float,
        .void,
        .comptime_array,
        .string,
        => unreachable,
        .uint, .sint, .float, .pointer, .many_pointer => decl: {
            const global = dg.context.addGlobal(name, llvm_type);
            const is_constant = @intFromBool(!info.mutable);
            c.LLVMSetGlobalConstant(global, is_constant);
            if (info.initializer) |init| {
                c.LLVMSetInitializer(global, try dg.initializer(init));
            }

            break :decl global;
        },
        .function => decl: {
            const func = dg.context.addFunction(name, llvm_type);
            if (info.initializer != null) {
                const builder = try dg.gpa.create(Context.Builder);
                builder.* = Context.Builder.init(dg.context, func);
                const function = info.initializer.?.val.function;
                var codegen = CodeGen{
                    .arena = dg.arena,
                    .builder = builder,
                    .hir = hir,
                    .func = function,
                    .map = .{},
                    .global_map = dg.global_map,
                    .value_map = .{},
                    .scratch = .{},
                };
                try codegens.append(codegen);
            }

            break :decl func;
        },
        .array => {
            // TODO
            std.log.err("codegen: arrays not implemented\n", .{});
            unreachable;
        },
        .slice => {
            // TODO
            std.log.err("codegen: slices not implemented\n", .{});
            unreachable;
        },
        .structure => {
            // TODO
            std.log.err("codegen: structures not implemented\n", .{});
            unreachable;
        },
    };

    switch (info.linkage) {
        .internal => c.LLVMSetLinkage(decl, c.LLVMInternalLinkage),
        .external => c.LLVMSetLinkage(decl, c.LLVMExternalLinkage),
    }

    return decl;
}

fn initializer(dg: *DeclGen, tv: TypedValue) !c.LLVMValueRef {
    const llvm_type = try dg.context.convertType(tv.ty);

    const val = switch (tv.ty.kind()) {
        .comptime_uint, .comptime_sint, .comptime_float => unreachable,
        .uint => val: {
            const val = tv.val.integer;
            break :val c.LLVMConstInt(llvm_type, @intCast(val), 0);
        },
        .sint => val: {
            const val: i64 = @bitCast(tv.val.integer);
            break :val c.LLVMConstInt(llvm_type, @intCast(val), 1);
        },
        .float => val: {
            const val: f64 = @bitCast(tv.val.float);
            break :val c.LLVMConstReal(llvm_type, val);
        },
        else => |kind| {
            // unimplemented
            std.log.err("codegen: initializer not implemented for type {}\n", .{kind});
            unreachable;
        },
    };

    return val;
}
