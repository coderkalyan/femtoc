const std = @import("std");
const parse = @import("parse.zig");
const AstFormatter = @import("fmt/ast_formatter.zig").AstFormatter;

test "arith.fm" {
    const gpa = std.testing.allocator;
    const readonly_src = @embedFile("tests/factorial.fm");
    var src = try gpa.allocSentinel(u8, readonly_src.len, 0); // required so ast can free src
    @memcpy(src, readonly_src);
    var ast = try parse.parse(gpa, src);
    defer ast.deinit(gpa);

    var out = std.ArrayList(u8).init(gpa);
    defer out.deinit();
    const writer = out.writer();

    try out.ensureUnusedCapacity(src.len);
    var fmt = AstFormatter(4, @TypeOf(writer)).init(writer, &ast);
    try fmt.render();
    std.debug.print("{s}", .{out.items});
    try std.testing.expectEqualDeep(out.items, src);
}
