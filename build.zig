const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "femtoc",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.linkLibC();
    exe.linkSystemLibrary("c");
    exe.linkSystemLibrary("LLVM-16");
    exe.addModule("clap", clap.module("clap"));
    // exe.linkLibrary(clap.artifact("clap"));
    // exe.addPackage(.{
    //     .name = "clap",
    //     .source = .{ .path = "lib/zig-clap/clap.zig" },
    // });
    // exe.install();
    b.installArtifact(exe);

    // const run_cmd = exe.run();
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    // run_cmd.step.dependOn(&b.step);
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
    // exe_tests.setTarget(target);
    // exe_tests.setBuildMode(opt);
    exe_tests.linkLibC();

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}
