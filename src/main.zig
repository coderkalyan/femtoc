const std = @import("std");
const parse = @import("parse.zig");
const hirgen = @import("hirgen.zig");

const max_file_size = std.math.maxInt(u32);

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file = try std.fs.cwd().openFile("samples/arith.fm", .{});
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

    const ast = try parse.parse(allocator, source);
    std.log.debug("{any} {any}", .{ast.nodes.items(.tag), ast.nodes.items(.main_token)});
    std.log.debug("{any}", .{ast.nodes.items(.data)});

    const hir = try hirgen.generate(allocator, &ast);
    _ = hir;
}
