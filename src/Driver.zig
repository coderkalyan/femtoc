const std = @import("std");
const parse = @import("parse.zig");
const HirGen = @import("HirGen.zig");
const MirGen = @import("MirGen.zig");
const CodeGen = @import("CodeGen.zig");
const render = @import("render.zig");

const Allocator = std.mem.Allocator;

const time = std.time;
const max_file_size = std.math.maxInt(u32);

pub const Configuration = struct {
    input: []const u8,
    output: []const u8,
    stage: enum {
        object,
        assembly,
        llvm_bc,
        llvm_ll,
        executable,
    },
    verbose_hir: bool,
    verbose_mir: bool,
    verbose_llvm_ir: bool,
};

pub fn build(gpa: Allocator, config: *Configuration) !void {
    const out = std.io.getStdOut();
    var buffered_out = std.io.bufferedWriter(out.writer());
    var writer = buffered_out.writer();

    var file = try std.fs.cwd().openFile(config.input, .{});
    defer file.close();

    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    var source = try gpa.allocSentinel(u8, @intCast(usize, stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.process.exit(1);
    }

    const ast = try parse.parse(gpa, source);

    const hir = try HirGen.generate(gpa, &ast);
    if (config.verbose_hir) {
        var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, &hir);
        try hir_renderer.render();
        try buffered_out.flush();
    }

    const compilation = try MirGen.generate(gpa, &hir, config);

    try CodeGen.generate(gpa, &compilation);
}
