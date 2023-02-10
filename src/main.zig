const std = @import("std");
const parse = @import("parse.zig");
const hirgen = @import("hirgen.zig");
const render = @import("render.zig");

const time = std.time;
const max_file_size = std.math.maxInt(u32);

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var timer = try time.Timer.start();

    var file = try std.fs.cwd().openFile("src/spec/factorial.fm", .{});
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

    const out = std.io.getStdOut();
    var buf = std.io.bufferedWriter(out.writer());
    var writer = buf.writer();

    // try writer.writeAll("hello");

    const hir = try hirgen.generate(allocator, &ast);
    _ = hir;
    const hirgen_time = timer.lap() / 1000;

    std.debug.print("read={}us ast={}us hirgen={}us\n", .{read_time, ast_time, hirgen_time});
    var ast_renderer = render.AstRenderer(4, @TypeOf(writer)).init(writer, &ast);
    _ = ast_renderer;
    // try ast_renderer.render();
    try buf.flush();

    // var hir_renderer = render.HirRenderer(4, @TypeOf(writer)).init(writer, &hir);
    // try hir_renderer.render();
    // try buf.flush();
}
