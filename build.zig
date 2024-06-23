const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match filter") orelse &[0][]const u8{};

    // -------------------------------------------------------------------------

    const exe = b.addExecutable(.{
        .name = "zig-lsp-codegen",
        .root_source_file = b.path("src/main.zig"),
        .target = b.graph.host,
    });
    // The metaMode.json file should be removed once https://github.com/ziglang/zig/issues/17895 has been resolved.
    exe.root_module.addAnonymousImport("meta-model", .{ .root_source_file = b.path("metaModel.json") });

    const run_codegen = b.addRunArtifact(exe);
    const lsp_types_output_file = run_codegen.addOutputFileArg("lsp_types.zig");

    b.getInstallStep().dependOn(&b.addInstallFile(lsp_types_output_file, "artifacts/lsp_types.zig").step);
    b.getInstallStep().dependOn(&b.addInstallFile(b.path("src/lsp.zig"), "artifacts/lsp.zig").step);

    const lsp_types_module = b.addModule("lsp-types", .{
        .root_source_file = lsp_types_output_file,
        .target = target,
        .optimize = optimize,
    });

    const lsp_module = b.addModule("lsp", .{
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = optimize,
    });
    lsp_module.addImport("lsp-types", lsp_types_module);

    // -------------------------------- Autodoc --------------------------------

    const autodoc_exe = b.addObject(.{
        .name = "lsp",
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = .Debug,
    });

    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc_exe.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc/lsp-codegen",
    });

    const docs_step = b.step("docs", "Generate and install documentation");
    docs_step.dependOn(&install_docs.step);

    // --------------------------------- Tests ---------------------------------

    const tests = b.addTest(.{
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
    });
    tests.root_module.addImport("lsp-types", lsp_types_module);

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
