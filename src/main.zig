const std = @import("std");
const parse = @import("parse.zig");
const HirGen = @import("HirGen.zig");
const LlvmBackend = @import("_llvm/Backend.zig");
const error_handler = @import("error_handler.zig");
const render = @import("render.zig");
const Allocator = std.mem.Allocator;

const io = std.io;
const max_file_size = std.math.maxInt(u32);

pub const HIR = (1 << 0);
pub const CODEGEN = (1 << 1);
pub const COMPILE = (1 << 2);
pub const ASSEMBLE = (1 << 3);

pub const Stage = enum(u8) {
    llvm = HIR | CODEGEN,
    compile = HIR | CODEGEN | COMPILE,
    assemble = HIR | CODEGEN | COMPILE | ASSEMBLE,
};

fn fatal(msg: []const u8) noreturn {
    std.debug.print("{s}\n", .{msg});
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

fn stem(path: []const u8) []const u8 {
    var start = path.len - 1;
    while (path[start] != '/') : (start -= 1) {}
    var end = path.len - 1;
    while (path[end] != '.') : (end -= 1) {}
    return path[start + 1 .. end];
}

fn guessOutputFilename(allocator: Allocator, input_filename: []const u8, stage: Stage) ![]u8 {
    const input_stem = stem(input_filename);
    const extension = switch (stage) {
        .llvm => "ll",
        .compile => "s",
        .assemble => "o",
    };

    return std.fmt.allocPrint(allocator, "{s}.{s}", .{ input_stem, extension });
}

pub fn readSource(gpa: Allocator, input_filename: []const u8) ![:0]u8 {
    var file = try std.fs.cwd().openFile(input_filename, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    var source = try gpa.allocSentinel(u8, @intCast(stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.os.exit(1);
    }

    return source;
}

pub fn main() !void {
    var allocator: std.heap.GeneralPurposeAllocator(.{}) = .{};
    const gpa = allocator.allocator();
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var args = try std.process.argsWithAllocator(gpa);
    defer args.deinit();

    var input_filename: ?[]const u8 = null;
    var output_filename: ?[]const u8 = null;
    var stage: Stage = .assemble;
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
    if (output_filename == null) {
        output_filename = try guessOutputFilename(gpa, input_filename.?, stage);
    }

    const out = std.io.getStdOut();
    var buffered_out = std.io.bufferedWriter(out.writer());
    var writer = buffered_out.writer();

    const stage_bits = @intFromEnum(stage);

    var source = try readSource(gpa, input_filename.?);
    const ast = try parse.parse(gpa, source);
    if (ast.errors.len > 0) {
        const errors = try error_handler.LocatedSourceError.locateErrors(gpa, &ast, ast.errors);
        var error_renderer = error_handler.CompileErrorRenderer(2, @TypeOf(writer)).init(writer, gpa, &ast, input_filename.?, errors);

        try error_renderer.render();
        try buffered_out.flush();
        std.os.exit(1);
    }

    if (stage_bits & HIR == 0) std.os.exit(0);
    // var hirgen = HirGen.init(gpa, &ast);
    // try hirgen.lowerAst();
    // try hirgen.semanticAnalysis();
    // const hir = try hirgen.toOwnedHir();
    const hir = try HirGen.generate(gpa, &ast);

    if (hir.errors.len > 0) {
        const errors = try error_handler.LocatedSourceError.locateErrors(gpa, &ast, hir.errors);
        var error_renderer = error_handler.CompileErrorRenderer(2, @TypeOf(writer)).init(writer, gpa, &ast, input_filename.?, errors);

        try error_renderer.render();
        try buffered_out.flush();
        std.os.exit(1);
    }

    if (verbose_hir) {
        var hir_arena = std.heap.ArenaAllocator.init(gpa);
        defer hir_arena.deinit();
        var hir_renderer = render.HirRenderer(2, @TypeOf(writer)).init(writer, hir_arena.allocator(), &hir);
        try hir_renderer.render();
        try buffered_out.flush();
    }

    if (stage_bits & CODEGEN == 0) std.os.exit(0);
    var backend = LlvmBackend.init(gpa, arena.allocator());
    defer backend.deinit();
    try backend.generate(&hir);

    if (verbose_llvm) {
        try backend.dumpToStdout();
    }
    if (stage == .llvm) {
        try backend.printToFile(output_filename.?);
        std.os.exit(0);
    }
}
