const std = @import("std");
const clap = @import("clap");
const Driver = @import("Driver.zig");

const debug = std.debug;
const io = std.io;

fn fatal(msg: []const u8) noreturn {
    debug.print("{s}\n", .{msg});
    std.process.exit(1);
}

fn stem(path: []const u8) []const u8 {
    var start = path.len - 1;
    while (path[start] != '/') : (start -= 1) {}
    var end = path.len - 1;
    while (path[end] != '.') : (end -= 1) {}
    return path[start+1..end];
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help            Display this help and exit.
        \\-o , --output <str>   Specify the output filename.
        \\-c, --compile         Emit object file.
        \\-S, --assemble        Emit assembly file.
        \\--emit-llvm           Emit LLVM IR (.bc) file.
        \\--verbose-hir         Dump femto HIR to stdout.
        \\--verbose-mir         Dump femto HIR to stdout.
        \\--verbose-llvm-ir     Dump LLVM IR to stdout.
        \\<str>                 Source filename to compile.
    );

    var diag = clap.Diagnostic {};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{
        .diagnostic = &diag,
    }) catch |err| {
        diag.report(io.getStdErr().writer(), err) catch {};
        return;
    };
    defer res.deinit();

    if (res.positionals.len == 0) {
        fatal("Missing source filename");
    }

    var config = Driver.Configuration {
        .input = res.positionals[0],
        .output = "",
        .stage = .executable,
        .verbose_hir = res.args.@"verbose-hir",
        .verbose_mir = res.args.@"verbose-mir",
        .verbose_llvm_ir = res.args.@"verbose-llvm-ir",
        .emit_llvm = res.args.@"emit-llvm",
    };

    const input_stem = stem(config.input);
    var extension: []const u8 = "o";
    if (res.args.compile) {
        if (res.args.assemble) {
            fatal("-c cannot be used along with -S");
        }
        extension = "o";
        config.stage = .object;
    } else if (res.args.assemble) {
        extension = "s";
        config.stage = .assembly;
    }

    if (res.args.@"emit-llvm") {
        if (res.args.assemble) {
            extension = "ll";
            config.stage = .llvm_ll;
        } else {
            extension = "bc";
            config.stage = .llvm_bc;
        }
    }
    const chunks: [2][]const u8 = .{ input_stem, extension };
    config.output = try std.mem.joinZ(allocator, ".", &chunks);
    defer allocator.free(config.output);

    try Driver.build(allocator, &config);
}
