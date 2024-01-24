const std = @import("std");
const Ast = @import("../Ast.zig");
const Node = Ast.Node;
const Allocator = std.mem.Allocator;

pub const SrcLoc = struct {
    line: u32,
    col: u32,
};

pub const LazySrcLoc = union(enum) {
    node: Node.Index,
    token: Ast.TokenIndex,
    none,

    pub fn resolveToken(self: LazySrcLoc, ast: *const Ast) Ast.TokenIndex {
        return switch (self) {
            .node => |node| ast.mainToken(node),
            .token => |token| token,
            .none => 0,
        };
    }

    pub fn comparator(context: *const Ast, l: LazySrcLoc, r: LazySrcLoc) bool {
        return l.resolveToken(context) < r.resolveToken(context);
    }
};

const TmpLoc = struct {
    lazy_loc: LazySrcLoc,
    index: u32,

    pub fn comparator(context: *const Ast, l: TmpLoc, r: TmpLoc) bool {
        return LazySrcLoc.comparator(context, l.lazy_loc, r.lazy_loc);
    }
};

pub fn locateSources(
    gpa: Allocator,
    lazy_locs: []const LazySrcLoc,
    ast: *const Ast,
) ![]const SrcLoc {
    const tmp = try gpa.alloc(TmpLoc, lazy_locs.len);
    defer gpa.free(tmp);
    for (lazy_locs, 0..) |lazy_loc, i| {
        tmp[i] = .{ .lazy_loc = lazy_loc, .index = @intCast(i) };
    }
    std.mem.sort(TmpLoc, tmp, ast, TmpLoc.comparator);

    const locs = try gpa.alloc(SrcLoc, lazy_locs.len);

    var loc_index: usize = 0;
    var byte_offset: Ast.ByteOffset = 0;
    var line_number: u32 = 1;
    var column_number: u32 = 0;
    var line_start: Ast.ByteOffset = 0;
    while (byte_offset < ast.source.len) : (byte_offset += 1) {
        if (loc_index >= tmp.len) break;
        if (ast.source[byte_offset] == '\n') {
            line_number += 1;
            column_number = 0;
            line_start = byte_offset + 1;
        } else {
            column_number += 1;
        }

        var token = tmp[loc_index].lazy_loc.resolveToken(ast);
        while (ast.tokens.items(.start)[token] == byte_offset) {
            locs[tmp[loc_index].index] = .{
                .line = line_number,
                .col = column_number,
            };
            loc_index += 1;
            if (loc_index >= tmp.len) break;
            token = tmp[loc_index].lazy_loc.resolveToken(ast);
        }
    }

    return locs;
}
