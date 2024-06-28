//! Custom `std.json` parser functions.

const std = @import("std");

pub fn Map(comptime Key: type, comptime Value: type) type {
    if (Key != []const u8) @compileError("TODO support non string Key's");
    return std.json.ArrayHashMap(Value);
}

pub fn StaticStringMap(comptime T: type) type {
    const static_string_map_renamed_zig_version = std.SemanticVersion.parse("0.13.0-dev.33+8af59d1f9") catch unreachable;
    if (@import("builtin").zig_version.order(static_string_map_renamed_zig_version) == .lt) {
        return type;
    } else {
        return std.StaticStringMap(T);
    }
}

pub fn staticStringMapInitComptime(comptime T: type, comptime kvs_list: anytype) StaticStringMap(T) {
    const static_string_map_renamed_zig_version = std.SemanticVersion.parse("0.13.0-dev.33+8af59d1f9") catch unreachable;
    if (@import("builtin").zig_version.order(static_string_map_renamed_zig_version) == .lt) {
        @setEvalBranchQuota(kvs_list.len * kvs_list.len);
        return std.ComptimeStringMap(T, kvs_list);
    } else {
        return std.StaticStringMap(T).initComptime(kvs_list);
    }
}

pub fn UnionParser(comptime T: type) type {
    return struct {
        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            // TODO this implementation is incredibly naive, surely it can be improved somewhat
            const json_value = try std.json.Value.jsonParse(allocator, source, options);
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

test UnionParser {
    {
        const U = union(enum) {
            number: u32,
            string: []const u8,
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .number = 3 }, "3");
        try expectParseEqual(U, U{ .string = "foo" }, "\"foo\"");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        // same as above but the fields are in reverse order
        const U = union(enum) {
            string: []const u8,
            number: u32,
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .number = 3 }, "3");
        try expectParseEqual(U, U{ .string = "foo" }, "\"foo\"");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        const U = union(enum) {
            foo: struct {},
            bar: struct { member: u32 },
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .foo = .{} }, "{}");
        try expectParseEqual(U, U{ .bar = .{ .member = 5 } }, "{\"member\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }

    {
        // same as above but the fields are in reverse order
        const U = union(enum) {
            bar: struct { member: u32 },
            foo: struct {},
            pub const jsonParse = UnionParser(@This()).jsonParse;
            pub const jsonParseFromValue = UnionParser(@This()).jsonParseFromValue;
        };

        try expectParseEqual(U, U{ .foo = .{} }, "{}");
        try expectParseEqual(U, U{ .bar = .{ .member = 5 } }, "{\"member\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "{\"mumber\": 5}");
        try expectParseEqual(U, error.UnexpectedToken, "true");
        try expectParseEqual(U, error.UnexpectedToken, "null");
    }
}

test "UnionParser.jsonStringify" {
    const U = union(enum) {
        number: u32,
        string: []const u8,
        pub const jsonStringify = UnionParser(@This()).jsonStringify;
    };

    try std.testing.expectFmt("5", "{}", .{std.json.fmt(U{ .number = 5 }, .{})});
    try std.testing.expectFmt("\"foo\"", "{}", .{std.json.fmt(U{ .string = "foo" }, .{})});
}

pub fn EnumCustomStringValues(comptime T: type, comptime contains_empty_enum: bool) type {
    return struct {
        comptime {
            if (contains_empty_enum) std.debug.assert(@hasField(T, "empty"));
            std.debug.assert(@hasField(T, "custom_value") or @hasField(T, "unknown_value"));
        }
        const special_value_indicator: std.meta.Tag(T) = if (@hasField(T, "custom_value")) T.custom_value else T.unknown_value;
        const special_value_field_name: []const u8 = if (@hasField(T, "custom_value")) "custom_value" else "unknown_value";

        const kvs = build_kvs: {
            const KV = struct { []const u8, T };
            const fields = @typeInfo(T).Union.fields;
            var kvs_array: [fields.len - 1]KV = undefined;
            for (fields[0 .. fields.len - 1], &kvs_array) |field, *kv| {
                if (contains_empty_enum and std.mem.eql(u8, field.name, "empty")) {
                    kv.* = .{ "", T.empty };
                } else {
                    kv.* = .{ field.name, @field(T, field.name) };
                }
            }
            break :build_kvs kvs_array;
        };

        const enum_from_string_map: StaticStringMap(T) = staticStringMapInitComptime(T, kvs);

        pub fn eql(a: T, b: T) bool {
            const tag_a = std.meta.activeTag(a);
            const tag_b = std.meta.activeTag(b);
            if (tag_a != tag_b) return false;

            if (tag_a == special_value_indicator) {
                return std.mem.eql(u8, @field(a, special_value_field_name), @field(b, special_value_field_name));
            } else {
                return true;
            }
        }

        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!T {
            const slice = try std.json.innerParse([]const u8, allocator, source, options);
            return enum_from_string_map.get(slice) orelse
                return @unionInit(T, special_value_field_name, slice);
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!T {
            const slice = try std.json.parseFromValueLeaky([]const u8, allocator, source, options);
            return enum_from_string_map.get(slice) orelse
                return @unionInit(T, special_value_field_name, slice);
        }

        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            if (contains_empty_enum and self == .empty) {
                try stream.write("");
                return;
            }
            switch (self) {
                special_value_indicator => |str| try stream.write(str),
                else => |val| try stream.write(@tagName(val)),
            }
        }
    };
}

test EnumCustomStringValues {
    {
        const E = union(enum) {
            foo,
            bar,
            baz,
            custom_value: []const u8,
            pub const jsonParse = EnumCustomStringValues(@This(), false).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), false).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), false).jsonStringify;
        };

        try std.testing.expectFmt("\"foo\"", "{}", .{std.json.fmt(E{ .foo = {} }, .{})});
        try std.testing.expectFmt("\"bar\"", "{}", .{std.json.fmt(E{ .bar = {} }, .{})});
        try std.testing.expectFmt("\"baz\"", "{}", .{std.json.fmt(E{ .baz = {} }, .{})});
        try std.testing.expectFmt("\"\"", "{}", .{std.json.fmt(E{ .custom_value = "" }, .{})});
        try std.testing.expectFmt("\"boo\"", "{}", .{std.json.fmt(E{ .custom_value = "boo" }, .{})});

        try expectParseEqual(E, E.foo, "\"foo\"");
        try expectParseEqual(E, E.bar, "\"bar\"");
        try expectParseEqual(E, E.baz, "\"baz\"");

        try expectParseEqual(E, E{ .custom_value = "boo" }, "\"boo\"");
        try expectParseEqual(E, E{ .custom_value = "" }, "\"\"");
    }

    {
        const E = union(enum) {
            foo,
            bar,
            baz,
            empty,
            unknown_value: []const u8,
            pub const jsonParse = EnumCustomStringValues(@This(), true).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), true).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), true).jsonStringify;
        };

        try std.testing.expectFmt("\"foo\"", "{}", .{std.json.fmt(E{ .foo = {} }, .{})});
        try std.testing.expectFmt("\"bar\"", "{}", .{std.json.fmt(E{ .bar = {} }, .{})});
        try std.testing.expectFmt("\"baz\"", "{}", .{std.json.fmt(E{ .baz = {} }, .{})});
        try std.testing.expectFmt("\"\"", "{}", .{std.json.fmt(E{ .empty = {} }, .{})});
        try std.testing.expectFmt("\"boo\"", "{}", .{std.json.fmt(E{ .unknown_value = "boo" }, .{})});

        try expectParseEqual(E, E.foo, "\"foo\"");
        try expectParseEqual(E, E.bar, "\"bar\"");
        try expectParseEqual(E, E.baz, "\"baz\"");
        try expectParseEqual(E, E.empty, "\"\"");

        try expectParseEqual(E, E{ .unknown_value = "boo" }, "\"boo\"");
    }

    {
        const E = union(enum) {
            foo,
            empty,
            custom_value: []const u8,
            pub const jsonParse = EnumCustomStringValues(@This(), false).jsonParse;
            pub const jsonParseFromValue = EnumCustomStringValues(@This(), false).jsonParseFromValue;
            pub const jsonStringify = EnumCustomStringValues(@This(), false).jsonStringify;
        };

        try std.testing.expectFmt("\"foo\"", "{}", .{std.json.fmt(E{ .foo = {} }, .{})});
        try std.testing.expectFmt("\"empty\"", "{}", .{std.json.fmt(E{ .empty = {} }, .{})});
        try std.testing.expectFmt("\"boo\"", "{}", .{std.json.fmt(E{ .custom_value = "boo" }, .{})});

        try expectParseEqual(E, E.foo, "\"foo\"");
        try expectParseEqual(E, E.empty, "\"empty\"");

        try expectParseEqual(E, E{ .custom_value = "boo" }, "\"boo\"");
    }
}

pub fn EnumStringifyAsInt(comptime T: type) type {
    return struct {
        pub fn jsonStringify(self: T, stream: anytype) @TypeOf(stream.*).Error!void {
            try stream.write(@intFromEnum(self));
        }
    };
}

test EnumStringifyAsInt {
    {
        const E = enum {
            foo,
            bar,
            baz,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("0", "{}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("1", "{}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("2", "{}", .{std.json.fmt(E.baz, .{})});
    }

    {
        const E = enum(u8) {
            foo = 2,
            bar,
            baz = 5,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("2", "{}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("3", "{}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("5", "{}", .{std.json.fmt(E.baz, .{})});
    }

    {
        const E = enum(u8) {
            foo,
            bar = 3,
            baz,
            _,
            pub const jsonStringify = EnumStringifyAsInt(@This()).jsonStringify;
        };
        try std.testing.expectFmt("0", "{}", .{std.json.fmt(E.foo, .{})});
        try std.testing.expectFmt("3", "{}", .{std.json.fmt(E.bar, .{})});
        try std.testing.expectFmt("4", "{}", .{std.json.fmt(E.baz, .{})});
        try std.testing.expectFmt("7", "{}", .{std.json.fmt(@as(E, @enumFromInt(7)), .{})});
    }
}

fn expectParseEqual(comptime T: type, comptime expected: anytype, s: []const u8) !void {
    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    if (@typeInfo(@TypeOf(expected)) != .ErrorSet) {
        const actual_from_slice = try std.json.parseFromSliceLeaky(T, arena, s, .{});
        try std.testing.expectEqualDeep(@as(T, expected), actual_from_slice);

        const value = try std.json.parseFromSliceLeaky(std.json.Value, arena, s, .{});
        const actual_from_value = std.json.parseFromValueLeaky(T, arena, value, .{});
        try std.testing.expectEqualDeep(@as(T, expected), actual_from_value);
    } else {
        try std.testing.expectError(expected, std.json.parseFromSliceLeaky(T, arena, s, .{}));
        const value = try std.json.parseFromSliceLeaky(std.json.Value, arena, s, .{});
        try std.testing.expectError(expected, std.json.parseFromValueLeaky(T, arena, value, .{}));
    }
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
