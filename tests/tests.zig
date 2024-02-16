const Server = @This();

const lsp = @import("lsp");
const std = @import("std");

const SampleDirection = enum {
    client_to_server,
    server_to_client,
};

const SampleEntryKind = enum {
    @"send-request",
    @"receive-request",

    @"send-response",
    @"receive-response",

    @"send-notification",
    @"receive-notification",

    fn getDirection(self: SampleEntryKind) SampleDirection {
        return switch (self) {
            .@"send-request", .@"send-response", .@"send-notification" => .client_to_server,
            else => .server_to_client,
        };
    }
};

// TODO: Handle responses
const SampleEntry = struct {
    isLSPMessage: bool,
    type: SampleEntryKind,
    message: std.json.Value,
};

test {
    var log_file = try std.fs.cwd().openFile("samples/amogus-json.log", .{});
    defer log_file.close();

    const reader = log_file.reader();

    var read_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer read_buffer.deinit();

    while (true) {
        try reader.readUntilDelimiterArrayList(&read_buffer, '\n', std.math.maxInt(u32)) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        const parsed_sample_entry = try std.json.parseFromSlice(SampleEntry, std.testing.allocator, read_buffer.items, .{});
        const sample_entry = parsed_sample_entry.value;

        if (sample_entry.isLSPMessage) {
            switch (sample_entry.type) {
                .@"send-notification",
                .@"receive-notification",
                => a: {
                    _ = tres.parse(lsp.Notification, sample_entry.message, std.testing.allocator) catch |err| {
                        // Ignore unknown methods such as custom VSCode LSP methods
                        if (err == error.UnknownMethod) break :a;
                        std.log.err("Cannot handle Request or Notification of method \"{s}\"", .{entry.message.Object.get("method").?.String});
                        break :a;
                    };
                },
                .@"send-request",
                .@"receive-request",
                => a: {
                    _ = tres.parse(lsp.Request, entry.message, std.testing.allocator) catch |err| {
                        // Ignore unknown methods such as custom VSCode LSP methods
                        if (err == error.UnknownMethod) break :a;
                        std.log.err("Cannot handle Request or Notification of method \"{s}\"", .{entry.message.Object.get("method").?.String});
                        break :a;
                    };
                },
                else => {},
            }
        }
    }
}
