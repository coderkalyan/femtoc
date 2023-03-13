const std = @import("std");
const parse = @import("parse.zig");
const HirGen = @import("HirGen.zig");
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");
const Mir = @import("Mir.zig");
const render = @import("render.zig");

const time = std.time;
const max_file_size = std.math.maxInt(u32);

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(allocator);
    _ = args.skip();

    var timer = try time.Timer.start();

    var file = try std.fs.cwd().openFile(args.next().?, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }
    var source = try allocator.allocSentinel(u8, @intCast(usize, stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.process.exit(1);
    }
    const read_time = timer.lap() / 1000;

    const ast = try parse.parse(allocator, source);
    const ast_time = timer.lap() / 1000;

    const hir = try HirGen.generate(allocator, &ast);
    const hirgen_time = timer.lap() / 1000;

    // const module = try MirGen.generate(allocator, &hir);
    // _ = mir;
    const mirgen_time = timer.lap() / 1000;

    std.debug.print("read={}us ast={}us hirgen={}us mirgen={}us\n", .{read_time, ast_time, hirgen_time, mirgen_time});

    const out = std.io.getStdOut();
    var buf = std.io.bufferedWriter(out.writer());
    var writer = buf.writer();

    var ast_renderer = render.AstRenderer(4, @TypeOf(writer)).init(writer, &ast);
    _ = ast_renderer;
    // try ast_renderer.render();
    // try buf.flush();

    var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, &hir);
    // _ = hir_renderer;
    try hir_renderer.render();
    try buf.flush();

    // for (module.function_mir) |mir| {
    //     var mir_renderer = render.MirRenderer(2, @TypeOf(writer)).init(writer, &mir);
    //     // _ = mir_renderer;
    //     try mir_renderer.render();
    //     try buf.flush();
    // }
}
