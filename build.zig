const std = @import("std");

pub fn build(b: *std.build.Builder) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const tres_module = b.dependency("tres", .{}).module("tres");

    const exe = b.addExecutable(.{
        .name = "lspmm-zig",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("tres", tres_module);
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(b.getInstallStep());

    var tests = b.addTest(.{
        .root_source_file = .{ .path = "libs/tres/tres.zig" },
        .target = target,
    });
    tests.addModule("tres", tres_module);

    test_step.dependOn(&b.addRunArtifact(tests).step);
}
