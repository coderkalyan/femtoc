const std = @import("std");
const error_handler = @import("error_handler.zig");
const Driver = @import("Driver.zig");
const render = @import("render.zig");
const Allocator = std.mem.Allocator;

const debug = std.debug;
const io = std.io;
const max_file_size = std.math.maxInt(u32);

fn fatal(msg: []const u8) noreturn {
    debug.print("{s}\n", .{msg});
    std.process.exit(1);
}

fn print_usage() !void {
    const message =
        \\ --help           Display this help and exit.
        \\ -o <str>         Specify the output filename.
        \\ -S               Compile, but don't assemble.
        \\ -c               Compile and assemble, but don't link.
        \\ --emit-llvm      Emit LLVM IR (.ll) file and exit.
        \\ --verbose-hir    Dump femto HIR to stdout.
        \\ --verbose-llvm   Dump LLVM IR to stdout.
        \\ <str>            Source filename to compile.
    ;
    std.log.info(message, .{});
    std.os.exit(0);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(allocator);
    defer args.deinit();

    var input_filename: ?[]const u8 = null;
    var output_filename: ?[]const u8 = null;
    var stage: Driver.Stage = .assemble;
    var stage_set = false;
    var verbose_hir = false;
    var verbose_llvm = false;
    var i: u32 = 1;
    _ = args.next(); // skip executable
    while (args.next()) |arg| : (i += 1) {
        if (std.mem.eql(u8, arg, "--help")) {
            try print_usage();
        } else if (std.mem.eql(u8, arg, "-o")) {
            if (output_filename != null) {
                std.log.err("can only specify single output file\n", .{});
                std.os.exit(1);
            }
            if (args.next()) |next| {
                output_filename = next;
            } else {
                try print_usage();
            }
        } else if (std.mem.eql(u8, arg, "-S")) {
            if (stage_set) {
                std.log.err("cannot use multiple stage parameters [-S|-c|--emit-llvm] together\n", .{});
                std.os.exit(1);
            }
            stage = .compile;
            stage_set = true;
        } else if (std.mem.eql(u8, arg, "-c")) {
            if (stage_set) {
                std.log.err("cannot use multiple stage parameters [-S|-c|--emit-llvm] together\n", .{});
                std.os.exit(1);
            }
            stage = .assemble;
            stage_set = true;
        } else if (std.mem.eql(u8, arg, "--emit-llvm")) {
            if (stage_set) {
                std.log.err("cannot use multiple stage parameters [-S|-c|--emit-llvm] together\n", .{});
                std.os.exit(1);
            }
            stage = .llvm;
            stage_set = true;
        } else if (std.mem.eql(u8, arg, "--verbose-hir")) {
            verbose_hir = true;
        } else if (std.mem.eql(u8, arg, "--verbose-llvm")) {
            verbose_llvm = true;
        } else if (arg.len == 0 or arg[0] == '-') {
            try print_usage();
        } else if (input_filename != null) {
            std.log.err("can only specify single input file\n", .{});
            std.os.exit(1);
        } else {
            input_filename = arg;
        }
    }

    if (input_filename == null) {
        std.log.err("must specify input filename\n", .{});
        std.os.exit(1);
    }

    const out = std.io.getStdOut();
    var buffered_out = std.io.bufferedWriter(out.writer());
    var writer = buffered_out.writer();

    var driver = try Driver.init(allocator, arena.allocator(), input_filename.?, output_filename, stage);
    const stage_bits = @intFromEnum(stage);

    var source = try driver.readSource();
    const ast = try driver.parseAst(source);
    if (ast.errors.len > 0) {
        const errors = try error_handler.LocatedSourceError.locateErrors(allocator, &ast, ast.errors);
        var error_renderer = error_handler.CompileErrorRenderer(2, @TypeOf(writer)).init(writer, allocator, &ast, input_filename.?, errors);

        try error_renderer.render();
        try buffered_out.flush();
        std.os.exit(1);
    }

    if (stage_bits & Driver.HIR == 0) std.os.exit(0);
    const hir = try driver.generateHir(&ast);
    if (hir.errors.len > 0) {
        const errors = try error_handler.LocatedSourceError.locateErrors(allocator, &ast, hir.errors);
        var error_renderer = error_handler.CompileErrorRenderer(2, @TypeOf(writer)).init(writer, allocator, &ast, input_filename.?, errors);

        try error_renderer.render();
        try buffered_out.flush();
        std.os.exit(1);
    }

    if (verbose_hir) {
        var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, &hir);
        try hir_renderer.render();
        try buffered_out.flush();
    }

    if (stage_bits & Driver.CODEGEN == 0) std.os.exit(0);
    var backend = try driver.generateLlvm(&hir);
    defer backend.deinit();
    if (verbose_llvm) {
        try driver.dumpLlvm(&backend);
    }
    if (stage == .llvm) {
        try driver.printLlvmToFile(&backend);
        std.os.exit(0);
    }
}
