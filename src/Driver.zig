const std = @import("std");
const parse = @import("parse.zig");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const HirGen = @import("HirGen.zig");
const error_handler = @import("error_handler.zig");
const LlvmBackend = @import("_llvm/Backend.zig");

const Allocator = std.mem.Allocator;

const time = std.time;
const max_file_size = std.math.maxInt(u32);
const Driver = @This();

gpa: Allocator,
arena: Allocator,
input_filename: []const u8,
output_filename: []const u8,
stage: Stage,

pub const HIR = (1 << 0);
pub const CODEGEN = (1 << 1);
pub const COMPILE = (1 << 2);
pub const ASSEMBLE = (1 << 3);

pub const Stage = enum(u8) {
    llvm = HIR | CODEGEN,
    compile = HIR | CODEGEN | COMPILE,
    assemble = HIR | CODEGEN | COMPILE | ASSEMBLE,
};

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

pub fn init(gpa: Allocator, arena: Allocator, input_filename: []const u8, output_filename: ?[]const u8, stage: Stage) !Driver {
    return .{
        .gpa = gpa,
        .arena = arena,
        .input_filename = input_filename,
        .output_filename = output_filename orelse try guessOutputFilename(gpa, input_filename, stage),
        .stage = stage,
    };
}

pub fn readSource(driver: *Driver) ![:0]u8 {
    var file = try std.fs.cwd().openFile(driver.input_filename, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    var source = try driver.gpa.allocSentinel(u8, @intCast(stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.os.exit(1);
    }

    return source;
}

pub fn parseAst(driver: *Driver, source: [:0]const u8) !Ast {
    return parse.parse(driver.gpa, source);
}

pub fn generateHir(driver: *Driver, ast: *const Ast) !Hir {
    return HirGen.generate(driver.gpa, ast);
}

pub fn generateLlvm(driver: *Driver, hir: *const Hir) !LlvmBackend {
    var backend = LlvmBackend.init(driver.gpa, driver.arena);
    try backend.generate(hir);
    return backend;
}

pub fn dumpLlvm(driver: *Driver, backend: *LlvmBackend) !void {
    _ = driver;
    try backend.dumpToStdout();
}

pub fn printLlvmToFile(driver: *Driver, backend: *LlvmBackend) !void {
    try backend.printToFile(driver.output_filename);
}
