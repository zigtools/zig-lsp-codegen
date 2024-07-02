[![CI](https://github.com/zigtools/zig-lsp-codegen/actions/workflows/main.yml/badge.svg)](https://github.com/zigtools/zig-lsp-codegen/actions)
[![codecov](https://codecov.io/gh/zigtools/zig-lsp-codegen/graph/badge.svg?token=C3HCN59E4C)](https://codecov.io/gh/zigtools/zig-lsp-codegen)
[![Documentation](https://badgen.net/badge/icon/Docs?icon=wiki&label)](https://zigtools.github.io/zig-lsp-codegen)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# Zig LSP Codegen

Generates `std.json` compatible Zig code based on the official [LSP MetaModel](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#metaModel)

## Installation

```bash
# Initialize a `zig build` project if you haven't already
zig init
# Add the `lsp-codegen` package to your `build.zig.zon`
zig fetch --save git+https://github.com/zigtools/zig-lsp-codegen.git
```

You can then import `lsp-codegen` in your `build.zig` with:

```zig
const lsp_codegen = b.dependency("lsp-codegen", .{});
const exe = b.addExecutable(...);
exe.root_module.addImport("lsp", lsp_codegen.module("lsp"));
```
