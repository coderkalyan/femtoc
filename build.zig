const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "femtoc",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("LLVM-16");
    exe.linkSystemLibrary("libelf");
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run femtoc");
    run_step.dependOn(&run_cmd.step);

    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/test.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe_tests.linkLibC();

    const behavior_tests = b.addTest(.{
        .root_source_file = .{ .path = "tests/behavior/test.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");

    const dirname = "tests/behavior";
    const cwd = std.fs.cwd();
    var dir = cwd.openIterableDir(dirname, .{}) catch unreachable;
    var walker = dir.walk(b.allocator) catch @panic("OOM");
    defer walker.deinit();

    while (walker.next() catch unreachable) |entry| {
        const ext = std.fs.path.extension(entry.basename);
        if (!std.mem.eql(u8, ext, ".fm")) continue;

        const stem = std.fs.path.stem(entry.basename);
        const src = std.fs.path.join(b.allocator, &.{ dirname, entry.path }) catch @panic("OOM");
        const frontend = b.addRunArtifact(exe);
        frontend.addFileArg(.{ .path = src });
        frontend.addArg("--emit-llvm");
        test_step.dependOn(&frontend.step);

        const ll = std.mem.join(b.allocator, ".", &.{ stem, "ll" }) catch @panic("OOM");
        const llc = b.addSystemCommand(&.{"llc"});
        llc.addFileArg(.{ .path = ll });
        llc.addArg("-o");
        const s_filename = std.mem.join(b.allocator, ".", &.{ stem, "s" }) catch @panic("OOM");
        const s = llc.addOutputFileArg(s_filename);
        test_step.dependOn(&llc.step);

        behavior_tests.addCSourceFile(.{ .file = s, .flags = &.{} });
    }

    const run_behavior_tests = b.addRunArtifact(behavior_tests);
    test_step.dependOn(&run_behavior_tests.step);
}
