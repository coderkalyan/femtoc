const std = @import("std");
const Lexer = @import("lex.zig").Lexer;

fn cMain() callconv(.C) void {
    main() catch unreachable;
}

const max_file_size = std.math.maxInt(u32);

fn main() !void {
    var allocator = std.heap.GeneralPurposeAllocator(.{}){};
    var gpa = allocator.allocator();

    const stdin = std.io.getStdIn();
    // remember, we only support u32 sized files
    const source = try stdin.readToEndAllocOptions(gpa, std.math.maxInt(u32), null, 1, 0);
    defer gpa.free(source);

    var lexer = Lexer.init(source);
    while (true) {
        const token = lexer.next();
        if (token.tag == .eof) break;
    }
}

comptime {
    @export(cMain, .{ .name = "main", .linkage = .Strong });
}
