const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Air = @import("../air/Air.zig");
const Type = @import("../air/type.zig").Type;
const TypedValue = @import("../air/TypedValue.zig");
const CodeGen = @import("CodeGen.zig");
const Context = @import("Context.zig");
const Scope = @import("Scope.zig");
// const c = @cImport({
//     @cInclude("femto_llvm.h");
// });
const Decl = Air.Decl;
const Allocator = std.mem.Allocator;
const c = Context.c;

const Backend = @This();

gpa: Allocator,
arena: Allocator,
context: Context,
// context: *c.Context,
pool: *InternPool,

pub fn init(gpa: Allocator, arena: Allocator, pool: *InternPool, filename: []const u8) Backend {
    return .{
        .gpa = gpa,
        .arena = arena,
        .context = Context.init(arena, "femto_main", pool, filename),
        // .context = c.fm_context_init().?,
        .pool = pool,
    };
}

pub fn deinit(self: *Backend) void {
    self.context.deinit();
    // c.fm_context_deinit(self.context);
}

pub fn generate(self: *Backend) !void {
    // first pass - forward declare
    // TODO: we can probably just have a single pass and have a "generate or get decl" function
    // var pool_iterator = self.pool.decls.iterator(0);
    // while (pool_iterator.next()) |decl| {
    //     _ = try self.context.generateDecl(decl);
    // }
    var decl_index: u32 = 0;
    while (decl_index < self.pool.decls.len) : (decl_index += 1) {
        _ = try self.context.generateDecl(@enumFromInt(decl_index));
    }

    // second pass - function body
    // pool_iterator = self.pool.decls.iterator(0);
    // while (pool_iterator.next()) |decl| {
    decl_index = 0;
    while (decl_index < self.pool.decls.len) : (decl_index += 1) {
        const decl = self.pool.decls.at(decl_index);
        // search for function bodies
        if (decl.initializer) |initializer| {
            const tv = self.pool.indexToKey(initializer).tv;
            switch (tv.val) {
                .body => |body| {
                    const function = self.context.resolveDecl(@enumFromInt(decl_index));
                    const air = self.pool.bodies.at(@intFromEnum(body));
                    var builder = Context.Builder.init(&self.context, function);
                    const di = &self.context.di;
                    const name = switch (decl.name) {
                        .named => |name| self.pool.getString(name).?,
                        .unnamed => "unnamed",
                    };
                    const ty = try di.resolveType(self.pool.indexToType(decl.ty));
                    const difunc = c.LLVMDIBuilderCreateFunction(
                        di.builder,
                        di.file,
                        name.ptr,
                        name.len,
                        name.ptr,
                        name.len,
                        di.file,
                        0,
                        ty,
                        @intFromBool(false),
                        @intFromBool(true),
                        0,
                        c.LLVMDIFlagPrototyped,
                        @intFromBool(false),
                    );

                    var codegen: CodeGen = .{
                        .arena = self.arena,
                        .builder = &builder,
                        .pool = self.pool,
                        .air = air,
                        .scratch = .{},
                        .map = .{},
                        .lazy = .{},
                        // .control_flow = .{},
                        .scope = undefined,
                        .discopes = .{},
                    };
                    try codegen.discopes.append(self.arena, difunc);
                    defer codegen.deinit();

                    try codegen.generate();
                },
                else => {},
            }
        }
    }

    self.context.di.finalize();
}

pub fn dumpToStdout(self: *Backend) !void {
    // c.fm_context_dump(self.context);
    self.context.dump();
}

pub fn printToFile(self: *Backend, filename: []const u8) !void {
    try self.context.printToFile(filename);
}
