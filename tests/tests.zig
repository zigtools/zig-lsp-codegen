const Server = @This();

const lsp = @import("lsp");
const std = @import("std");
const tres = @import("tres");

arena: std.heap.ArenaAllocator,
parser: std.json.Parser,

read_buf: std.ArrayList(u8),
write_buf: std.ArrayList(u8),

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
    @"type": SampleEntryKind,
    message: std.json.Value,
};

pub fn readLine(self: *Server, reader: anytype) !void {
    while (true) {
        var byte = try reader.readByte();

        if (byte == '\n') {
            return;
        }

        if (self.read_buf.items.len == self.read_buf.capacity) {
            try self.read_buf.ensureTotalCapacity(self.read_buf.capacity + 1);
        }

        try self.read_buf.append(byte);
    }
}

pub fn flushArena(self: *Server) void {
    self.arena.deinit();
    self.arena.state = .{};
}

test {
    @setEvalBranchQuota(100_000);

    var log_dir = try std.fs.cwd().openDir("samples", .{ .iterate = true });
    defer log_dir.close();

    var log = try log_dir.openFile("amogus-json.log", .{});
    defer log.close();

    var reader = log.reader();
    // reader.readAll()

    const allocator = std.heap.page_allocator;
    var arena = std.heap.ArenaAllocator.init(allocator);

    var server = Server{
        .arena = arena,
        .parser = std.json.Parser.init(arena.allocator(), false),

        .read_buf = try std.ArrayList(u8).initCapacity(allocator, 1024),
        .write_buf = try std.ArrayList(u8).initCapacity(allocator, 1024),
    };

    var parser = std.json.Parser.init(server.arena.allocator(), false);

    while (true) {
        server.readLine(reader) catch |err| switch (err) {
            error.EndOfStream => return,
            else => return std.log.err("{s}", .{err}),
        };

        const tree = try parser.parse(server.read_buf.items);
        defer parser.reset();
        const entry = try tres.parse(SampleEntry, tree.root, arena.allocator());

        if (entry.isLSPMessage) {
            switch (entry.@"type") {
                .@"send-notification",
                .@"receive-notification",
                => a: {
                    _ = tres.parse(lsp.Notification, entry.message, allocator) catch |err| {
                        // Ignore unknown methods such as custom VSCode LSP methods
                        if (err == error.UnknownMethod) break :a;
                        std.log.err("Cannot handle Request or Notification of method \"{s}\"", .{entry.message.Object.get("method").?.String});
                        break :a;
                    };
                },
                .@"send-response" => {
                    // std.log.err("\nSEND RESPONSE\n", .{});
                },
                else => {},
            }
        }

        server.read_buf.items.len = 0;
        // arena.deinit();
    }
}
