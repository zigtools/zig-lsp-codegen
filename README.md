# zig-lsp-codegen

Zig LSP codegen from the newly released, official metamodel! This actually good code replaces the much hackier [lsp-typegen](https://github.com/zigtools/lsp-typegen);

## Usage

1. `git clone --recurse-submodules https://github.com/zigtools/zig-lsp-codegen`
2. Plop `metaModel.json` in this cloned repo. A copy can be found [here](https://github.com/microsoft/vscode-languageserver-node/blob/main/protocol/metaModel.json).
3. `zig build run`
4. Tada! You should now have a `lsp.zig` file that can be `zig fmt`ed and used to your heart's content! Enjoy :)
