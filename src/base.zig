//! generated by zig-lsp-codegen

const std = @import("std");

const URI = []const u8;
/// The URI of a document
pub const DocumentUri = []const u8;
/// A JavaScript regular expression; never used
pub const RegExp = []const u8;

pub const LSPAny = std.json.Value;
pub const LSPArray = []LSPAny;
pub const LSPObject = std.json.ObjectMap;

pub const RequestId = union(enum) {
    integer: i64,
    string: []const u8,
    pub usingnamespace UnionParser(@This());
};

pub const ResponseError = struct {
    /// A number indicating the error type that occurred.
    code: i64,
    /// A string providing a short description of the error.
    message: []const u8,

    /// A primitive or structured value that contains additional
    /// information about the error. Can be omitted.
    data: std.json.Value = .null,
};

/// Indicates in which direction a message is sent in the protocol.
pub const MessageDirection = enum {
    clientToServer,
    serverToClient,
    both,
};

pub const RegistrationMetadata = struct {
    method: ?[]const u8,
    Options: ?type,
};

pub const NotificationMetadata = struct {
    method: []const u8,
    documentation: ?[]const u8,
    direction: MessageDirection,
    Params: ?type,
    registration: RegistrationMetadata,
};

pub const RequestMetadata = struct {
    method: []const u8,
    documentation: ?[]const u8,
    direction: MessageDirection,
    Params: ?type,
    Result: type,
    PartialResult: ?type,
    ErrorData: ?type,
    registration: RegistrationMetadata,
};

pub fn Map(comptime Key: type, comptime Value: type) type {
    if (Key != []const u8) @compileError("TODO support non string Key's");
    return std.json.ArrayHashMap(Value);
}

pub fn UnionParser(comptime T: type) type {
    return struct {
        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            const json_value = try std.json.parseFromTokenSourceLeaky(std.json.Value, allocator, source, options);
            return try jsonParseFromValue(allocator, json_value, options);
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            inline for (std.meta.fields(T)) |field| {
                if (std.json.parseFromValueLeaky(field.type, allocator, source, options)) |result| {
                    return @unionInit(T, field.name, result);
                } else |_| {}
            }
            return error.UnexpectedToken;
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            switch (self) {
                inline else => |value| try stream.write(value),
            }
        }
    };
}

pub fn EnumCustomStringValues(comptime T: type, comptime contains_empty_enum: bool) type {
    return struct {
        const kvs = build_kvs: {
            const KV = struct { []const u8, T };
            const fields = @typeInfo(T).Union.fields;
            var kvs_array: [fields.len - 1]KV = undefined;
            inline for (fields[0 .. fields.len - 1], 0..) |field, i| {
                kvs_array[i] = .{ field.name, @field(T, field.name) };
            }
            break :build_kvs kvs_array[0..];
        };
        /// NOTE: this maps 'empty' to .empty when T contains an empty enum
        /// this shouldn't happen but this doesn't do any harm
        const map = std.ComptimeStringMap(T, kvs);

        pub fn eql(a: T, b: T) bool {
            const tag_a = std.meta.activeTag(a);
            const tag_b = std.meta.activeTag(b);
            if (tag_a != tag_b) return false;

            if (tag_a == .custom_value) {
                return std.mem.eql(u8, a.custom_value, b.custom_value);
            } else {
                return true;
            }
        }

        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            const slice = try std.json.parseFromTokenSourceLeaky([]const u8, allocator, source, options);
            if (contains_empty_enum and slice.len == 0) return .empty;
            return map.get(slice) orelse return .{ .custom_value = slice };
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            const slice = try std.json.parseFromValueLeaky([]const u8, allocator, source, options);
            if (contains_empty_enum and slice.len == 0) return .empty;
            return map.get(slice) orelse return .{ .custom_value = slice };
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            if (contains_empty_enum and self == .empty) {
                try stream.write("");
                return;
            }
            switch (self) {
                .custom_value => |str| try stream.write(str),
                else => |val| try stream.write(@tagName(val)),
            }
        }
    };
}

pub fn EnumStringifyAsInt(comptime T: type) type {
    return struct {
        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            try stream.write(@intFromEnum(self));
        }
    };
}

comptime {
    _ = @field(@This(), "notification_metadata");
    _ = @field(@This(), "request_metadata");
}
