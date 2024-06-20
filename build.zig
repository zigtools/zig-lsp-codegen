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
    const lsp_output_path = run_codegen.addOutputFileArg("lsp.zig");

    const lsp_module = b.addModule("lsp", .{
        .root_source_file = lsp_output_path,
        .target = target,
        .optimize = optimize,
    });
    b.getInstallStep().dependOn(&b.addInstallFile(lsp_output_path, "artifacts/lsp.zig").step);

    const test_step = b.step("test", "Run all the tests");

    const module_tests = b.addTest(.{
        .root_source_file = lsp_output_path,
        .target = target,
        .optimize = optimize,
    });
    test_step.dependOn(&b.addRunArtifact(module_tests).step);

    const tests = b.addTest(.{
        .root_source_file = b.path("tests/tests.zig"),
        .target = target,
        .optimize = optimize,
    });
    tests.root_module.addImport("lsp", lsp_module);

    test_step.dependOn(&b.addRunArtifact(tests).step);
}
