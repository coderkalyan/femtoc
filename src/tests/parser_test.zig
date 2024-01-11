const std = @import("std");
const Ast = @import("../Ast.zig");
const parse = @import("../parse.zig");
const ast_formatter = @import("../fmt/ast_formatter.zig");

const Allocator = std.mem.Allocator;
const Node = Ast.Node;
const AstYamlFormatter = ast_formatter.AstYamlFormatter;

const max_file_size = std.math.maxInt(u32);

pub fn readSource(gpa: Allocator, input_filename: []const u8) ![:0]u8 {
    var file = try std.fs.cwd().openFile(input_filename, .{});
    defer file.close();
    const stat = try file.stat();
    if (stat.size > max_file_size) {
        std.log.err("File size too large, must be at most {} bytes", .{max_file_size});
        std.process.exit(1);
    }

    const source = try gpa.allocSentinel(u8, @intCast(stat.size), 0);
    const size = try file.readAll(source);
    if (stat.size != size) {
        std.log.err("Failed to read entire source file", .{});
        std.os.exit(1);
    }

    return source;
}

test "format trial" {
    const allocator = std.testing.allocator;
    const stdio = std.io.getStdOut().writer();
    const Formatter = AstYamlFormatter(2, @TypeOf(stdio));

    const source = try readSource(allocator, "euler/p04.fm");
    var ast = try parse.parse(allocator, source);
    defer ast.deinit(allocator);

    var fmt = Formatter.init(stdio, &ast);
    try fmt.fmt();
}
