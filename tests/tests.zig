const Server = @This();

const lsp = @import("lsp");
const std = @import("std");

const SampleEntry = struct {
    isLSPMessage: bool,
    type: Kind,
    message: std.json.Value,
    timestamp: u64,

    const Kind = enum {
        @"send-request",
        @"receive-request",

        @"send-notification",
        @"receive-notification",

        @"send-response",
        @"receive-response",

        fn getDirection(self: Kind) Direction {
            switch (self) {
                .@"send-request", .@"send-response", .@"send-notification" => return .client_to_server,
                .@"receive-request", .@"receive-notification", .@"receive-response" => return .server_to_client,
            }
        }
    };

    const Direction = enum {
        client_to_server,
        server_to_client,
    };
};

test {
    var log_file = try std.fs.cwd().openFile("samples/amogus-json.log", .{});
    defer log_file.close();

    const reader = log_file.reader();

    var read_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer read_buffer.deinit();

    while (true) {
        read_buffer.clearRetainingCapacity();
        reader.readUntilDelimiterArrayList(&read_buffer, '\n', std.math.maxInt(u32)) catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };

        const parsed_sample_entry = try std.json.parseFromSlice(SampleEntry, std.testing.allocator, read_buffer.items, .{});
        defer parsed_sample_entry.deinit();
        const sample_entry = parsed_sample_entry.value;

        if (sample_entry.isLSPMessage) {
            const message = try std.json.parseFromValue(lsp.JsonRPCMessage, std.testing.allocator, sample_entry.message, .{});
            defer message.deinit();

            const MessageTag = std.meta.Tag(lsp.JsonRPCMessage);
            const expected_tag: MessageTag = switch (sample_entry.type) {
                .@"send-request", .@"receive-request" => .request,
                .@"send-notification", .@"receive-notification" => .notification,
                .@"send-response", .@"receive-response" => .response,
            };
            try std.testing.expectEqual(expected_tag, std.meta.activeTag(message.value));
        } else {
            @panic("TODO");
        }
    }
}

comptime {
    for (lsp.notification_metadata) |metadata| {
        if (metadata.Params) |Params| {
            testType(Params);
        }
    }
    for (lsp.request_metadata) |metadata| {
        if (std.mem.eql(u8, metadata.method, "textDocument/selectionRange")) continue; // TODO
        if (metadata.Params) |Params| {
            testType(Params);
        }
        testType(metadata.Result);
        if (metadata.PartialResult) |PartialResult| {
            testType(PartialResult);
        }
        if (metadata.ErrorData) |ErrorData| {
            testType(ErrorData);
        }
    }
}

fn testType(comptime T: type) void {
    if (T == void) return;
    if (T == ?void) return;

    const S = struct {
        fn parseFromValue() void {
            _ = std.json.parseFromValue(T, undefined, undefined, undefined) catch unreachable;
        }
        fn innerParse() void {
            var source: std.json.Scanner = undefined;
            _ = std.json.innerParse(T, undefined, &source, undefined) catch unreachable;
        }
        fn stringify() void {
            const value: T = undefined;
            _ = std.json.stringify(value, undefined, std.io.null_writer) catch unreachable;
        }
    };
    _ = &S.parseFromValue;
    _ = &S.innerParse;
    _ = &S.stringify;
}
