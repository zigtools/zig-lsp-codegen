const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use Zig's llvm code backend");
    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match filter") orelse &[0][]const u8{};

    // -------------------------------------------------------------------------

    const exe = b.addExecutable(.{
        .name = "zig-lsp-codegen",
        .root_source_file = b.path("src/main.zig"),
        .target = b.graph.host,
    });
    // The metaModel.json file should be removed once https://github.com/ziglang/zig/issues/17895 has been resolved.
    exe.root_module.addAnonymousImport("meta-model", .{ .root_source_file = b.path("metaModel.json") });

    const run_codegen = b.addRunArtifact(exe);
    const lsp_types_output_file = run_codegen.addOutputFileArg("lsp_types.zig");

    const lsp_parser_module = b.addModule("lsp-parser", .{ .root_source_file = b.path("src/parser.zig") });
    const lsp_types_module = b.addModule("lsp-types", .{ .root_source_file = lsp_types_output_file });
    lsp_types_module.addImport("parser", lsp_parser_module);

    const lsp_module = b.addModule("lsp", .{
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = optimize,
    });
    lsp_module.addImport("parser", lsp_parser_module);
    lsp_module.addImport("types", lsp_types_module);

    // -------------------------------- Autodoc --------------------------------

    // This can be simplified with https://github.com/ziglang/zig/pull/20388
    const autodoc_exe = b.addObject(.{
        .name = "lsp",
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = .Debug,
    });
    autodoc_exe.root_module.addImport("parser", lsp_parser_module);
    autodoc_exe.root_module.addImport("types", lsp_types_module);

    const install_docs = b.addInstallDirectory(.{
        .source_dir = autodoc_exe.getEmittedDocs(),
        .install_dir = .prefix,
        .install_subdir = "doc/lsp-codegen",
    });

    const docs_step = b.step("docs", "Generate and install documentation");
    docs_step.dependOn(&install_docs.step);

    // --------------------------------- Tests ---------------------------------

    // This can be simplified with https://github.com/ziglang/zig/pull/20388
    const tests = b.addTest(.{
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
        .use_lld = use_llvm,
        .use_llvm = use_llvm,
    });
    tests.root_module.addImport("parser", lsp_parser_module);
    tests.root_module.addImport("types", lsp_types_module);

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addRunArtifact(tests).step);
}
