const std = @import("std");
const builtin = @import("builtin");

const minimum_zig_version = std.SemanticVersion.parse("0.14.0-dev.3445+6c3cbb0c8") catch unreachable;

pub fn build(b: *std.Build) void {
    comptime if (builtin.zig_version.order(minimum_zig_version) == .lt) {
        @compileError(std.fmt.comptimePrint(
            \\Your Zig version does not meet the minimum build requirement:
            \\  required Zig version: {[minimum_zig_version]}
            \\  actual   Zig version: {[current_version]}
            \\
        , .{
            .current_version = builtin.zig_version,
            .minimum_zig_version = minimum_zig_version,
        }));
    };

    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const use_llvm = b.option(bool, "use-llvm", "Use Zig's llvm code backend");
    const test_filters = b.option([]const []const u8, "test-filter", "Skip tests that do not match filter") orelse &[0][]const u8{};

    // -------------------------------------------------------------------------

    const exe = b.addExecutable(.{
        .name = "lsp-codegen",
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
    const lsp_tests = b.addTest(.{
        .root_source_file = b.path("src/lsp.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
        .use_lld = use_llvm,
        .use_llvm = use_llvm,
    });
    lsp_tests.root_module.addImport("parser", lsp_parser_module);
    lsp_tests.root_module.addImport("types", lsp_types_module);

    const lsp_parser_tests = b.addTest(.{
        .name = "test parser",
        .root_source_file = b.path("src/parser.zig"),
        .target = target,
        .optimize = optimize,
        .filters = test_filters,
        .use_lld = use_llvm,
        .use_llvm = use_llvm,
    });

    const test_step = b.step("test", "Run all the tests");
    test_step.dependOn(&b.addRunArtifact(lsp_tests).step);
    test_step.dependOn(&b.addRunArtifact(lsp_parser_tests).step);

    // ----------------------------- Code Coverage -----------------------------

    const kcov_bin = b.findProgram(&.{"kcov"}, &.{}) catch "kcov";

    const addOutputDirectoryArg = comptime if (@import("builtin").zig_version.order(.{ .major = 0, .minor = 13, .patch = 0 }) == .lt)
        std.Build.Step.Run.addOutputFileArg
    else
        std.Build.Step.Run.addOutputDirectoryArg;

    const kcov_merge = std.Build.Step.Run.create(b, "kcov merge coverage");
    kcov_merge.rename_step_with_output_arg = false;
    kcov_merge.addArg(kcov_bin);
    kcov_merge.addArg("--merge");
    const coverage_output = addOutputDirectoryArg(kcov_merge, ".");

    for ([_]*std.Build.Step.Compile{ lsp_tests, lsp_parser_tests }) |test_artifact| {
        const kcov_collect = std.Build.Step.Run.create(b, "kcov collect coverage");
        kcov_collect.addArg(kcov_bin);
        kcov_collect.addArg("--collect-only");
        kcov_collect.addPrefixedDirectoryArg("--include-pattern=", b.path("."));
        kcov_merge.addDirectoryArg(addOutputDirectoryArg(kcov_collect, test_artifact.name));
        kcov_collect.addArtifactArg(test_artifact);
        kcov_collect.enableTestRunnerMode();
    }

    const install_coverage = b.addInstallDirectory(.{
        .source_dir = coverage_output,
        .install_dir = .{ .custom = "coverage" },
        .install_subdir = "",
    });

    const coverage_step = b.step("coverage", "Generate a coverage report with kcov");
    coverage_step.dependOn(&install_coverage.step);
}
