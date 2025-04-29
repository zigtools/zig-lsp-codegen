//! This file implements an example LSP client.
//!
//! This example is NOT meant to be a blueprint on how to design an LSP client.
//! Instead it meant to showcase the various low-level utilities provided by
//! this library to LSP client authors.
//!
//! This library will take care of the tedious boilerplate (stdio, JSON-RPC,
//! LSP data types) while allowing authors to freely decide on how to
//! architect their LSP client.
//!
//! For detailed information on how the language server protocol works, checkout the official specificiation:
//! https://microsoft.github.io/language-server-protocol/specifications/specification-current
//!
//! The run this example program with the build system, use the following command:
//! ```
//! zig build run-hello-client -- path/to/unformatted/file.zig /path/to/zls
//! ```
//!
//! Omitting the arguments to the language server will use `./hello_server.zig` as the langauge server:
//! ```
//! zig build run-hello-client -- path/to/unformatted/file.zig
//! ```
//!
//! See the `usage` below for more information.
//!

const std = @import("std");
const builtin = @import("builtin");
const lsp = @import("lsp");

pub const std_options: std.Options = .{
    .log_level = .info,
};

/// Be aware that the output doesn't clearly show the source (client or server) of the output
const inherit_langauge_server_stderr: bool = false;

const usage =
    \\hello-client
    \\
    \\Give me a document an I will ask the language server whether it has any suggested changes to format the document.
    \\
    \\Usage:   hello-client /path/to/unformatted/file.zig <language server arguments>
    \\
    \\Example: hello-client file.zig     /path/to/zls
    \\         hello-client ../file.zig  zls
    \\
    \\
;

fn fatalWithUsage(comptime format: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writeAll(usage) catch {};
    std.log.err(format, args);
    std.process.exit(1);
}

fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    std.process.exit(1);
}

var debug_allocator: std.heap.DebugAllocator(.{}) = .init;

pub fn main() !void {
    const gpa, const is_debug = switch (builtin.mode) {
        .Debug, .ReleaseSafe => .{ debug_allocator.allocator(), true },
        .ReleaseFast, .ReleaseSmall => .{ std.heap.smp_allocator, false },
    };
    defer if (is_debug) {
        _ = debug_allocator.deinit();
    };

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len < 3) fatalWithUsage("expected at least 2 arguments but got {d}", .{args.len - 1});

    const input_file = std.fs.cwd().readFileAlloc(gpa, args[1], std.math.maxInt(u32)) catch |err|
        fatal("failed to read file '{s}': {}", .{ args[1], err });
    defer gpa.free(input_file);

    // Spawn the language server as a child process.
    var child_process: std.process.Child = .init(args[2..], gpa);
    child_process.stdin_behavior = .Pipe;
    child_process.stdout_behavior = .Pipe;
    child_process.stderr_behavior = if (inherit_langauge_server_stderr) .Inherit else .Ignore;

    child_process.spawn() catch |err| fatal("child process could not be created: {}", .{err});
    child_process.waitForSpawn() catch |err| fatal("child process could not be created: {}", .{err});

    // Language servers can support multiple communication channels (e.g. stdio, pipes, sockets).
    // See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#implementationConsiderations
    //
    // The `TransportOverStdio` implements the necessary logic to read and write messages over stdio.
    var transport: lsp.TransportOverStdio = .init(child_process.stdout.?, child_process.stdin.?);

    // The order of exchanged messages will look similar to this:
    //
    // 1. send `initialize` request and receive response
    // 2. send `initialized` notification
    // 3. send various requests like `textDocument/formatting`
    // 4. send `shutdown` request and receive response
    // 5. send `exit` notification

    // Send an "initialize" request to the server
    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialize
    try sendRequestToServer(gpa, transport.any(), .{ .number = 0 }, "initialize", lsp.types.InitializeParams{
        .capabilities = .{}, // the client capabilities tell the server what "features" the client supports
    });

    // Wait for the response from the server
    // For the sake of simplicity, we will block here and read messages until the response to our request has been found. All other messages will be ignored.
    // A more sophisticated client implementation will need to handle messages asynchronously.
    const initialize_response = try readAndIgnoreUntilResponse(gpa, transport.any(), .{ .number = 0 }, "initialize");
    defer initialize_response.deinit();

    const initialize_result: lsp.types.InitializeResult = initialize_response.value;
    _ = initialize_result.capabilities; // the server capabilities tell the client what "features" the server supports

    const serverName = if (initialize_result.serverInfo) |serverInfo| serverInfo.name else "unknown";
    std.log.info("Good morning Mx. {s}.", .{serverName});

    // Check whether the server supports the `textDocument/formatting` request
    const supports_formatting = blk: {
        const documentFormattingProvider = initialize_result.capabilities.documentFormattingProvider orelse break :blk false;
        switch (documentFormattingProvider) {
            .bool => |supported| break :blk supported,
            .DocumentFormattingOptions => break :blk true,
        }
    };

    if (!supports_formatting) {
        std.log.err("It seems like you're not qualified for this task. Get out!", .{});
        std.process.exit(1);
    }

    // Send a "initialized" notification to the server
    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
    try sendNotificationToServer(gpa, transport.any(), "initialized", lsp.types.InitializedParams{});

    // ----------------

    std.log.info("This document recently came in by the CLI.", .{});
    try sendNotificationToServer(gpa, transport.any(), "textDocument/didOpen", lsp.types.DidOpenTextDocumentParams{
        .textDocument = .{
            .uri = "untitled:Document", // Usually a file system uri will be provided like 'file:///path/to/main.zig'
            .languageId = "",
            .text = input_file,
            .version = 0,
        },
    });

    std.log.info("Just to double check, could you verify that it is formatted correctly?", .{});
    try sendRequestToServer(gpa, transport.any(), .{ .number = 1 }, "textDocument/formatting", lsp.types.DocumentFormattingParams{
        .textDocument = .{ .uri = "untitled:Document" },
        .options = .{ .tabSize = 4, .insertSpaces = true },
    });

    const formatting_response = try readAndIgnoreUntilResponse(gpa, transport.any(), .{ .number = 1 }, "textDocument/formatting");
    defer formatting_response.deinit();

    const text_edits = formatting_response.value orelse &.{};
    if (text_edits.len == 0) {
        std.log.info("{s}: I have no comments.", .{serverName});
    } else {
        std.log.info("{s}: I have identified {d} non-compliance(s) with my formatting specification", .{ serverName, text_edits.len });
    }

    // ----------------

    std.log.info("Well, thanks for your insight on this. Now get out!", .{});

    // Send a "shutdown" request to the server
    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#shutdown
    // Even though this is a request, we do not wait for a response because we are going to close the server anyway.
    try sendRequestToServer(gpa, transport.any(), .{ .number = 2 }, "shutdown", {});

    // Send a "exit" notification to the server
    // https://microsoft.github.io/language-server-protocol/specifications/specification-current/#initialized
    try sendNotificationToServer(gpa, transport.any(), "exit", {});

    // The "exit" notification will ask the server to exit its process. Ideally we should wait with a timeout in case the server is not behaving correctly.
    _ = try child_process.wait();
}

fn sendRequestToServer(
    allocator: std.mem.Allocator,
    transport: lsp.AnyTransport,
    id: lsp.JsonRPCMessage.ID,
    comptime method: []const u8,
    params: lsp.ParamsType(method),
) !void {
    std.log.debug("sending '{s}' request to server", .{method});

    const request: lsp.TypedJsonRPCRequest(lsp.ParamsType(method)) = .{
        .id = id,
        .method = method,
        .params = params,
    };

    const request_stringified = try std.json.stringifyAlloc(allocator, request, .{ .emit_null_optional_fields = false });
    defer allocator.free(request_stringified);

    try transport.writeJsonMessage(request_stringified);
}

fn sendNotificationToServer(
    allocator: std.mem.Allocator,
    transport: lsp.AnyTransport,
    comptime method: []const u8,
    params: lsp.ParamsType(method),
) !void {
    std.log.debug("sending '{s}' notification to server", .{method});

    const notification: lsp.TypedJsonRPCNotification(lsp.ParamsType(method)) = .{
        .method = method,
        .params = params,
    };

    const notification_stringified = try std.json.stringifyAlloc(allocator, notification, .{ .emit_null_optional_fields = false });
    defer allocator.free(notification_stringified);

    try transport.writeJsonMessage(notification_stringified);
}

/// Do not use such a function in an actual implementation.
fn readAndIgnoreUntilResponse(
    allocator: std.mem.Allocator,
    transport: lsp.AnyTransport,
    id: lsp.JsonRPCMessage.ID,
    comptime method: []const u8,
) !std.json.Parsed(lsp.ResultType(method)) {
    while (true) {
        // read the unparsed JSON-RPC message
        const json_message = try transport.readJsonMessage(allocator);
        defer allocator.free(json_message);
        std.log.debug("received message from server: {s}", .{json_message});

        // try to find the "id" field
        const parsed_message = try std.json.parseFromSlice(
            struct { id: ?lsp.JsonRPCMessage.ID = null },
            allocator,
            json_message,
            .{ .ignore_unknown_fields = true },
        );
        defer parsed_message.deinit();

        const actual_id = parsed_message.value.id orelse {
            // std.log.info("received message from server while waiting for '{}'. Ignoring...", .{std.json.fmt(id, .{})});
            continue;
        };

        if (!id.eql(actual_id)) {
            // std.log.info("received message from server while waiting for '{}'. Ignoring...", .{std.json.fmt(id, .{})});
            continue;
        }

        // parse the "result" field to the expected type
        const parsed_response = try std.json.parseFromSlice(
            struct { result: lsp.ResultType(method) },
            allocator,
            json_message,
            .{ .ignore_unknown_fields = true, .allocate = .alloc_always },
        );

        return .{
            .arena = parsed_response.arena,
            .value = parsed_response.value.result,
        };
    }
}
