const std = @import("std");
const clap = @import("clap");
const parse = @import("parse.zig");
const HirGen = @import("HirGen.zig");
const MirGen = @import("MirGen.zig");
const MirMap = @import("MirMap.zig");
const Mir = @import("Mir.zig");
const render = @import("render.zig");
const CodeGen = @import("CodeGen.zig");
const Driver = @import("Driver.zig");

const debug = std.debug;
const io = std.io;

fn fatal(msg: []const u8) noreturn {
    debug.print("{s}\n", .{msg});
    std.process.exit(1);
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
    };

    const input_stem = std.fs.path.basename(config.input);
    if (res.args.compile) {
        if (res.args.assemble) {
            fatal("-c cannot be used along with -S");
        }
        config.output = try std.fmt.allocPrint(allocator, "{s}.o", .{input_stem});
        config.stage = .object;
    } else if (res.args.assemble) {
        config.output = try std.fmt.allocPrint(allocator, "{s}.s", .{input_stem});
        config.stage = .assembly;
    }

    if (res.args.@"emit-llvm") {
        if (res.args.assemble) {
            allocator.free(config.output);
            config.output = try std.fmt.allocPrint(allocator, "{s}.ll", .{input_stem});
            config.stage = .llvm_ll;
        } else {
            config.output = try std.fmt.allocPrint(allocator, "{s}.bc", .{input_stem});
            config.stage = .llvm_bc;
        }
    }

    try Driver.build(allocator, &config);
}
