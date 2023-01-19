const std = @import("std");
const lex = @import("lex.zig");

const max_file_size = std.math.maxInt(u32);

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var file = try std.fs.cwd().openFile("lang.fm", .{});
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

    var lexer = lex.Lexer.init(source);
    while (true) {
        const token = lexer.next();
        if (token.tag == .eof) {
            break;
        }

        std.debug.print("({} {}..{}) ", .{token.tag, token.loc.start, token.loc.end});
        if (token.tag == .semi) {
            std.debug.print("\n", .{});
        }
    }
}
