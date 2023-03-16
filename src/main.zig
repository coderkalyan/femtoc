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

    // Iterate through arguments
    // Set filename to last positional filename arg for now
    // Future versions might have multiple source files (?)
    var arg = args.next();
    var fileName: ?[:0]const u8 = null;
    const fstageMap = std.ComptimeStringMap(u8, .{
        .{ "lex", 0 },
        .{ "ast", 1 },
        .{ "hir", 2 },
        .{ "mir", 3 },
        .{ "llvm", 4 },
    });
    var fstage: u8 = 4;
    var enableVerbose = std.StringArrayHashMap(bool).init(allocator);
    try enableVerbose.put("lex", false);
    try enableVerbose.put("ast", false);
    try enableVerbose.put("hir", false);
    try enableVerbose.put("mir", false);
    try enableVerbose.put("llvm", false);

    while (arg != null) {
        // std.log.debug("{s}", .{arg.?});
        if (std.mem.eql(u8, arg.?, "--help")) {
            std.debug.print(
                \\femtoc, the femto compiler
                \\Usage: femtoc [--args] <filename>
                \\
                \\Arguments:
                \\  --fstage=<lex, ast, hir, mir, llvm> Choose how far to run the pipeline.
                \\  --overbose=<lex, ast, hir, mir, llvm> Enable verbose output for specified stage. Can be specified multiple times.
            , .{});
        } else if (std.mem.startsWith(u8, arg.?, "--fstage=")) {
            var iter = std.mem.split(u8, arg.?, "=");
            _ = iter.next();
            const stage = iter.next().?;
            fstage = fstageMap.get(stage).?;
        } else if (std.mem.startsWith(u8, arg.?, "--overbose=")) {
            var iter = std.mem.split(u8, arg.?, "=");
            _ = iter.next();
            try enableVerbose.put(iter.next().?, true);
        } else {
            fileName = arg;
        }
        arg = args.next();
    }

    if (fileName == null) {
        std.log.err("A file name must be supplied.", .{});
        std.process.exit(1);
    }

    var file = std.fs.cwd().openFile(fileName.?, .{}) catch {
        std.log.err("Femto file {s} was not found.", .{fileName.?});
        std.process.exit(1);
    };

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

    // Create stdout
    const out = std.io.getStdOut();
    var buf = std.io.bufferedWriter(out.writer());
    var writer = buf.writer();

    // Run stages
    const ast = try parse.parse(allocator, source);
    const ast_time = timer.lap() / 1000;
    if (enableVerbose.get("ast").?) {
        var ast_renderer = render.AstRenderer(4, @TypeOf(writer)).init(writer, &ast);
        try ast_renderer.render();
        try buf.flush();
    }
    // if (fstage == fstageMap.get("ast").?) {
    //     std.log.info("Stopping at [ast].", .{});
    //     std.debug.print("read={}us ast={}us \n", .{ read_time, ast_time });
    //     std.process.exit(0);
    // }

    const hir = try HirGen.generate(allocator, &ast);
    const hirgen_time = timer.lap() / 1000;
    if (enableVerbose.get("hir").?) {
        var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, &hir);
        try hir_renderer.render();
        try buf.flush();
    }
    if (fstage == fstageMap.get("hir").?) {
        std.log.info("Stopping at [hir].", .{});
        std.debug.print("read={}us ast={}us hirgen={}us\n", .{ read_time, ast_time, hirgen_time });
        std.process.exit(0);
    }

    const compilation = try MirGen.generate(allocator, &hir);
    // _ = mir;
    const mirgen_time = timer.lap() / 1000;
    if (enableVerbose.get("mir").?) {
        for (compilation.mir) |mir| {
            var mir_renderer = render.MirRenderer(2, @TypeOf(writer)).init(writer, &mir);
            try mir_renderer.render();
            try buf.flush();
        }
    }
    if (fstage == fstageMap.get("mir").?) {
        std.log.info("Stopping at [mir].", .{});
        std.debug.print("read={}us ast={}us hirgen={}us mirgen={}us\n", .{ read_time, ast_time, hirgen_time, mirgen_time });
        std.process.exit(0);
    }

    std.debug.print("read={}us ast={}us hirgen={}us mirgen={}us\n", .{ read_time, ast_time, hirgen_time, mirgen_time });
}
