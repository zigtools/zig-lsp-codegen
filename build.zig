const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const meta_model_path = b.option([]const u8, "meta-model", "Specify path to the metaModel.json") orelse "metaModel.json";

    const exe = b.addExecutable(.{
        .name = "zig-lsp-codegen",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    b.installArtifact(exe);

    const run_codegen = b.addRunArtifact(exe);
    run_codegen.addFileArg(.{ .cwd_relative = meta_model_path });
    const lsp_output_file = run_codegen.addOutputFileArg("lsp.zig");

    _ = b.addModule("lsp", .{
        .root_source_file = lsp_output_file,
        .target = target,
        .optimize = optimize,
    });

    const install_lsp_artifact = b.addInstallFile(lsp_output_file, "artifacts/lsp.zig");
    b.getInstallStep().dependOn(&install_lsp_artifact.step);

    const tests = b.addTest(.{
        .root_source_file = lsp_output_file,
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
