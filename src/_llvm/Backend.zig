const std = @import("std");
const InternPool = @import("../InternPool.zig");
const Air = @import("../air/Air.zig");
const Type = @import("../air/type.zig").Type;
const TypedValue = @import("../air/TypedValue.zig");
const CodeGen = @import("CodeGen.zig");
const Context = @import("Context.zig");
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

pub fn init(gpa: Allocator, arena: Allocator, pool: *InternPool) Backend {
    return .{
        .gpa = gpa,
        .arena = arena,
        .context = Context.init(arena, "femto_main", pool),
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
    var pool_iterator = self.pool.decls.iterator(0);
    while (pool_iterator.next()) |decl| {
        _ = try self.context.generateDecl(decl);
    }

    // second pass - function body
    pool_iterator = self.pool.decls.iterator(0);
    while (pool_iterator.next()) |decl| {
        // search for function bodies
        if (decl.initializer) |initializer| {
            std.debug.print("{}\n", .{initializer});
            const tv = self.pool.indexToKey(initializer).tv;
            switch (tv.val) {
                .body => |body| {
                    const function = self.context.resolveDecl(decl);
                    const air = self.pool.bodies.at(@intFromEnum(body));
                    var builder = Context.Builder.init(&self.context, function);
                    var codegen: CodeGen = .{
                        .arena = self.arena,
                        .builder = &builder,
                        .pool = self.pool,
                        .air = air,
                        .scratch = .{},
                        .map = .{},
                    };
                    defer codegen.deinit();
                    try codegen.generate();
                },
                else => {},
            }
        }
    }
}

pub fn dumpToStdout(self: *Backend) !void {
    // c.fm_context_dump(self.context);
    self.context.dump();
}

pub fn printToFile(self: *Backend, filename: []const u8) !void {
    try self.context.printToFile(filename);
}
