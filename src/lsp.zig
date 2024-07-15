const std = @import("std");

pub const types = @import("types");
pub const parser = @import("parser");

/// See https://www.jsonrpc.org/specification
pub const JsonRPCMessage = union(enum) {
    request: Request,
    notification: Notification,
    response: Response,

    pub const ID = union(enum) {
        number: i64,
        string: []const u8,

        pub fn eql(a: ID, b: ID) bool {
            if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;
            switch (a) {
                .number => return a.number == b.number,
                .string => return std.mem.eql(u8, a.string, b.string),
            }
        }

        test eql {
            const id_number_3: ID = .{ .number = 3 };
            const id_number_7: ID = .{ .number = 7 };
            const id_string_foo: ID = .{ .string = "foo" };
            const id_string_bar: ID = .{ .string = "bar" };
            const id_string_3: ID = .{ .string = "3" };

            try std.testing.expect(id_number_3.eql(id_number_3));
            try std.testing.expect(!id_number_3.eql(id_number_7));

            try std.testing.expect(id_string_foo.eql(id_string_foo));
            try std.testing.expect(!id_string_foo.eql(id_string_bar));

            try std.testing.expect(!id_number_3.eql(id_string_foo));
            try std.testing.expect(!id_number_3.eql(id_string_3));
        }

        pub fn jsonParse(allocator: std.mem.Allocator, source: anytype, options: std.json.ParseOptions) std.json.ParseError(@TypeOf(source.*))!ID {
            switch (try source.peekNextTokenType()) {
                .number => return .{ .number = try std.json.innerParse(i64, allocator, source, options) },
                .string => return .{ .string = try std.json.innerParse([]const u8, allocator, source, options) },
                else => return error.UnexpectedToken,
            }
        }

        pub fn jsonParseFromValue(allocator: std.mem.Allocator, source: std.json.Value, options: std.json.ParseOptions) std.json.ParseFromValueError!ID {
            _ = allocator;
            _ = options;
            switch (source) {
                .integer => |number| return .{ .number = number },
                .string => |string| return .{ .string = string },
                else => return error.UnexpectedToken,
            }
        }

        pub fn jsonStringify(self: ID, stream: anytype) @TypeOf(stream.*).Error!void {
            switch (self) {
                inline else => |value| try stream.write(value),
            }
        }
    };

    pub const Request = struct {
        comptime jsonrpc: []const u8 = "2.0",
        /// The request id.
        id: ID,
        /// The method to be invoked.
        method: []const u8,
        /// The requests's params. The `std.json.Value` can only be `.null`, `.array` or `.object`.
        ///
        /// `params == null` means that the was no `"params"` field. `params == .null` means that the `"params"` field was set to `null`.
        params: ?std.json.Value,
    };

    pub const Notification = struct {
        comptime jsonrpc: []const u8 = "2.0",
        /// The method to be invoked.
        method: []const u8,
        /// The notification's params. The `std.json.Value` can only be `.null`, `.array` or `.object`.
        ///
        /// `params == null` means that the was no `"params"` field. `params == .null` means that the `"params"` field was set to `null`.
        params: ?std.json.Value,
    };

    pub const Response = struct {
        comptime jsonrpc: []const u8 = "2.0",
        /// The request id.
        ///
        /// It must be the same as the value of the `id` member in the `Request` object.
        /// If there was an error in detecting the id in the `Request` object (e.g. `Error.Code.parse_error`/`Error.Code.invalid_request`), it must be `null`.
        id: ?ID,
        result_or_error: union(enum) {
            /// The result of a request.
            result: ?std.json.Value,
            /// The error object in case a request fails.
            @"error": Error,
        },

        pub const Error = struct {
            /// A number indicating the error type that occurred.
            code: Code,
            /// A string providing a short description of the error.
            message: []const u8,
            /// A primitive or structured value that contains additional
            /// information about the error. Can be omitted.
            data: std.json.Value = .null,

            /// The error codes from and including -32768 to -32000 are reserved for pre-defined errors. Any code within this range, but not defined explicitly below is reserved for future use.
            ///
            /// The remainder of the space is available for application defined errors.
            pub const Code = enum(i64) {
                /// Invalid JSON was received by the server. An error occurred on the server while parsing the JSON text.
                parse_error = -32700,
                /// The JSON sent is not a valid Request object.
                invalid_request = -32600,
                /// The method does not exist / is not available.
                method_not_found = -32601,
                /// Invalid method parameter(s).
                invalid_params = -32602,
                /// Internal JSON-RPC error.
                internal_error = -32603,

                /// -32000 to -32099 are reserved for implementation-defined server-errors.
                _,

                pub fn jsonStringify(code: Code, stream: anytype) @TypeOf(stream.*).Error!void {
                    try stream.write(@intFromEnum(code));
                }
            };
        };

        pub fn jsonStringify(response: Response, stream: anytype) @TypeOf(stream.*).Error!void {
            try stream.beginObject();

            try stream.objectField("jsonrpc");
            try stream.write("2.0");

            if (response.id) |id| {
                try stream.objectField("id");
                try stream.write(id);
            } else if (stream.options.emit_null_optional_fields) {
                try stream.objectField("id");
                try stream.write(null);
            }

            switch (response.result_or_error) {
                inline else => |value, tag| {
                    try stream.objectField(@tagName(tag));
                    try stream.write(value);
                },
            }

            try stream.endObject();
        }
    };

    pub fn jsonParse(
        allocator: std.mem.Allocator,
        source: anytype,
        options: std.json.ParseOptions,
    ) std.json.ParseError(@TypeOf(source.*))!JsonRPCMessage {
        if (try source.next() != .object_begin)
            return error.UnexpectedToken;

        var fields: Fields = .{};

        while (true) {
            const field_name = blk: {
                const name_token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
                const maybe_field_name = switch (name_token) {
                    .string, .allocated_string => |slice| std.meta.stringToEnum(std.meta.FieldEnum(Fields), slice),
                    .object_end => break, // No more fields.
                    else => return error.UnexpectedToken,
                };

                switch (name_token) {
                    .string => {},
                    .allocated_string => |slice| allocator.free(slice),
                    else => unreachable,
                }

                break :blk maybe_field_name orelse {
                    if (options.ignore_unknown_fields) {
                        try source.skipValue();
                        continue;
                    } else {
                        return error.UnknownField;
                    }
                };
            };

            // check for contradicting fields
            switch (field_name) {
                .jsonrpc => {},
                .id => {},
                .method, .params => {
                    const is_result_set = if (fields.result) |result| result != .null else false;
                    if (is_result_set or fields.@"error" != null) {
                        return error.UnexpectedToken;
                    }
                },
                .result => {
                    if (fields.@"error" != null) {
                        // Allows { "error": {...}, "result": null }
                        switch (try source.peekNextTokenType()) {
                            .null => {
                                std.debug.assert(try source.next() == .null);
                                continue;
                            },
                            else => return error.UnexpectedToken,
                        }
                    }
                },
                .@"error" => {
                    const is_result_set = if (fields.result) |result| result != .null else false;
                    if (is_result_set) {
                        return error.UnexpectedToken;
                    }
                },
            }

            switch (field_name) {
                inline else => |comptime_field_name| {
                    if (comptime_field_name == field_name) {
                        if (@field(fields, @tagName(comptime_field_name))) |_| {
                            switch (options.duplicate_field_behavior) {
                                .use_first => {
                                    _ = try Fields.parse(comptime_field_name, allocator, source, options);
                                    continue;
                                },
                                .@"error" => return error.DuplicateField,
                                .use_last => {},
                            }
                        }
                        @field(fields, @tagName(comptime_field_name)) = try Fields.parse(comptime_field_name, allocator, source, options);
                    }
                },
            }
        }

        return try fields.toMessage();
    }

    pub fn jsonParseFromValue(
        allocator: std.mem.Allocator,
        source: std.json.Value,
        options: std.json.ParseOptions,
    ) std.json.ParseFromValueError!JsonRPCMessage {
        if (source != .object) return error.UnexpectedToken;

        var fields: Fields = .{};

        for (source.object.keys(), source.object.values()) |field_name, field_source| {
            inline for (std.meta.fields(Fields)) |field| {
                const field_enum = comptime std.meta.stringToEnum(std.meta.FieldEnum(Fields), field.name).?;
                if (std.mem.eql(u8, field.name, field_name)) {
                    @field(fields, field.name) = try Fields.parseFromValue(field_enum, allocator, field_source, options);
                    break;
                }
            } else {
                // Didn't match anything.
                if (!options.ignore_unknown_fields)
                    return error.UnknownField;
            }
        }

        return try fields.toMessage();
    }

    pub fn jsonStringify(message: JsonRPCMessage, stream: anytype) @TypeOf(stream.*).Error!void {
        switch (message) {
            inline else => |item| try stream.write(item),
        }
    }

    /// Method names that begin with the word rpc followed by a period character (U+002E or ASCII 46) are reserved for rpc-internal methods and extensions and MUST NOT be used for anything else.
    pub fn isReservedMethodName(name: []const u8) bool {
        return std.mem.startsWith(u8, name, "rpc.");
    }

    test isReservedMethodName {
        try std.testing.expect(isReservedMethodName("rpc.foo"));
        try std.testing.expect(!isReservedMethodName("textDocument/completion"));
    }

    const Fields = struct {
        jsonrpc: ?[]const u8 = null,
        method: ?[]const u8 = null,
        id: ?ID = null,
        params: ?std.json.Value = null,
        result: ?std.json.Value = null,
        @"error": ?Response.Error = null,

        fn parse(
            comptime field: std.meta.FieldEnum(@This()),
            allocator: std.mem.Allocator,
            source: anytype,
            options: std.json.ParseOptions,
        ) std.json.ParseError(@TypeOf(source.*))!std.meta.FieldType(@This(), field) {
            return switch (field) {
                .jsonrpc, .method => try std.json.innerParse([]const u8, allocator, source, options),
                .id => try std.json.innerParse(?JsonRPCMessage.ID, allocator, source, options),
                .params => switch (try source.peekNextTokenType()) {
                    .null => {
                        std.debug.assert(try source.next() == .null);
                        return .null;
                    },
                    .object_begin, .array_begin => try std.json.Value.jsonParse(allocator, source, options),
                    else => return error.UnexpectedToken, // "params" field must be null/object/array
                },
                .result => try std.json.Value.jsonParse(allocator, source, options),
                .@"error" => try std.json.innerParse(Response.Error, allocator, source, options),
            };
        }

        fn parseFromValue(
            comptime field: std.meta.FieldEnum(@This()),
            allocator: std.mem.Allocator,
            source: std.json.Value,
            options: std.json.ParseOptions,
        ) std.json.ParseFromValueError!std.meta.FieldType(@This(), field) {
            return switch (field) {
                .jsonrpc, .method => try std.json.innerParseFromValue([]const u8, allocator, source, options),
                .id => try std.json.innerParseFromValue(?JsonRPCMessage.ID, allocator, source, options),
                .params => switch (source) {
                    .null, .object, .array => source,
                    else => return error.UnexpectedToken, // "params" field must be null/object/array
                },
                .result => source,
                .@"error" => try std.json.innerParseFromValue(Response.Error, allocator, source, options),
            };
        }

        fn toMessage(self: Fields) !JsonRPCMessage {
            const jsonrpc = self.jsonrpc orelse
                return error.MissingField;
            if (!std.mem.eql(u8, jsonrpc, "2.0"))
                return error.UnexpectedToken; // the "jsonrpc" field must be "2.0"

            if (self.method) |method_val| {
                if (self.result != null or self.@"error" != null) {
                    return error.UnexpectedToken; // the "method" field indicates a request or notification which can't have the "result" or "error" field
                }
                if (self.params) |params_val| {
                    switch (params_val) {
                        .null, .object, .array => {},
                        else => unreachable,
                    }
                }

                if (self.id) |id_val| {
                    return .{
                        .request = .{
                            .method = method_val,
                            .params = self.params,
                            .id = id_val,
                        },
                    };
                } else {
                    return .{
                        .notification = .{
                            .method = method_val,
                            .params = self.params,
                        },
                    };
                }
            } else {
                if (self.@"error" != null) {
                    const is_result_set = if (self.result) |result| result != .null else false;
                    if (is_result_set)
                        return error.UnexpectedToken; // the "result" and "error" fields can't both be set
                } else {
                    const is_result_set = self.result != null;
                    if (!is_result_set)
                        return error.MissingField;
                }

                return .{
                    .response = .{
                        .id = self.id,
                        .result_or_error = if (self.@"error") |err|
                            .{ .@"error" = err }
                        else
                            .{ .result = self.result },
                    },
                };
            }
        }
    };

    test {
        try testParseExpectedError(
            \\5
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );
        try testParseExpectedError(
            \\{}
        ,
            error.MissingField,
            error.MissingField,
            .{},
        );
        try testParseExpectedError(
            \\{"method": "foo", "params": null}
        ,
            error.MissingField,
            error.MissingField,
            .{},
        );
        try testParseExpectedError(
            \\{"jsonrpc": "1.0", "method": "foo", "params": null}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );
        try testParseExpectedError(
            \\{
        ,
            error.UnexpectedEndOfInput,
            error.UnexpectedToken,
            .{},
        );
    }

    test Request {
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "method": "Die", "params": null}
        , .{
            .request = .{
                .id = .{ .number = 1 },
                .method = "Die",
                .params = .null,
            },
        }, .{});
        try testParse(
            \\{"id": "Würde", "method": "des", "params": null, "jsonrpc": "2.0"}
        , .{
            .request = .{
                .id = .{ .string = "Würde" },
                .method = "des",
                .params = .null,
            },
        }, .{});
        try testParse(
            \\{"method": "ist", "params": {}, "jsonrpc": "2.0", "id": "Menschen"}
        , .{
            .request = .{
                .id = .{ .string = "Menschen" },
                .method = "ist",
                .params = .{ .object = undefined },
            },
        }, .{});
        try testParse(
            \\{"method": ".", "jsonrpc": "2.0", "id": "unantastbar"}
        , .{
            .request = .{
                .id = .{ .string = "unantastbar" },
                .method = ".",
                .params = null,
            },
        }, .{});
    }

    test Notification {
        try testParse(
            \\{"jsonrpc": "2.0", "method": "foo", "params": null}
        , .{
            .notification = .{
                .method = "foo",
                .params = .null,
            },
        }, .{});
        try testParse(
            \\{"method": "bar", "params": null, "jsonrpc": "2.0"}
        , .{
            .notification = .{
                .method = "bar",
                .params = .null,
            },
        }, .{});
        try testParse(
            \\{"params": [], "method": "baz", "jsonrpc": "2.0"}
        , .{
            .notification = .{
                .method = "baz",
                .params = .{ .array = undefined },
            },
        }, .{});
        try testParse(
            \\{"method": "booze?", "jsonrpc": "2.0"}
        , .{
            .notification = .{
                .method = "booze?",
                .params = null,
            },
        }, .{});
    }

    test "Notification allow setting the 'id' field to null" {
        try testParse(
            \\{"jsonrpc": "2.0", "id": null, "method": "foo", "params": null}
        , .{
            .notification = .{
                .method = "foo",
                .params = .null,
            },
        }, .{});
    }

    test Response {
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "result": null}
        , .{ .response = .{
            .id = .{ .number = 1 },
            .result_or_error = .{ .result = .null },
        } }, .{});

        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1}
        ,
            error.MissingField,
            error.MissingField,
            .{},
        );

        try testParse(
            \\{"id": "id", "jsonrpc": "2.0", "result": null, "error": {"code": 3, "message": "foo", "data": null}}
        , .{ .response = .{
            .id = .{ .string = "id" },
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(3), .message = "foo", .data = .null } },
        } }, .{});
        try testParse(
            \\{"id": "id", "jsonrpc": "2.0", "error": {"code": 42, "message": "bar"}}
        , .{ .response = .{
            .id = .{ .string = "id" },
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(42), .message = "bar", .data = .null } },
        } }, .{});
        try testParse(
            \\{"id": "id", "jsonrpc": "2.0", "error": {"code": 42, "message": "bar"}, "result": null}
        , .{ .response = .{
            .id = .{ .string = "id" },
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(42), .message = "bar", .data = .null } },
        } }, .{});
    }

    test "validate that the 'params' is null/array/object" {
        // null
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": null}
        , .{ .request = .{
            .id = .{ .number = 1 },
            .method = "foo",
            .params = .null,
        } }, .{});
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo"}
        , .{ .request = .{
            .id = .{ .number = 1 },
            .method = "foo",
            .params = null,
        } }, .{});

        // bool
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": true}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );

        // integer
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": 5}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );

        // float
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": 4.2}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );

        // string
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": "bar"}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{},
        );

        // array
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": []}
        , .{ .request = .{
            .id = .{ .number = 1 },
            .method = "foo",
            .params = .{ .array = undefined },
        } }, .{});

        // object
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "method": "foo", "params": {}}
        , .{ .request = .{
            .id = .{ .number = 1 },
            .method = "foo",
            .params = .{ .object = undefined },
        } }, .{});
    }

    test "escaped field name" {
        try testParse(
            \\{"jsonrpc": "2.0", "\u006Dethod": "foo", "params": null}
        , .{
            .notification = .{
                .method = "foo",
                .params = .null,
            },
        }, .{});
    }

    test "duplicate_field_behavior" {
        // https://github.com/ziglang/zig/pull/20430
        if (comptime @import("builtin").zig_version.order(std.SemanticVersion.parse("0.14.0-dev.149+8f7b50e2c") catch unreachable) == .lt) return error.SkipZigTest;

        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "jsonrpc": "2.0", "method": "foo", "params": null}
        ,
            error.DuplicateField,
            error.DuplicateField,
            .{},
        );

        try testParse(
            \\{"jsonrpc": "2.0", "jsonrpc": "1.0", "method": "foo", "params": null}
        ,
            .{ .notification = .{ .method = "foo", .params = .null } },
            .{ .duplicate_field_behavior = .use_first },
        );
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "jsonrpc": "1.0", "method": "foo", "params": null}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{ .duplicate_field_behavior = .use_last },
        );

        try testParseExpectedError(
            \\{"jsonrpc": "1.0", "jsonrpc": "2.0", "method": "bar", "params": null}
        ,
            error.UnexpectedToken,
            error.UnexpectedToken,
            .{ .duplicate_field_behavior = .use_first },
        );
        try testParse(
            \\{"jsonrpc": "1.0", "jsonrpc": "2.0", "method": "bar", "params": null}
        ,
            .{ .notification = .{ .method = "bar", .params = .null } },
            .{ .duplicate_field_behavior = .use_last },
        );
    }

    test "ignore_unknown_fields" {
        try testParse(
            \\{"jsonrpc": "2.0", "id": 1, "other": null, "method": "foo", "params": null, "extra": "."}
        , .{
            .request = .{
                .id = .{ .number = 1 },
                .method = "foo",
                .params = .null,
            },
        }, .{ .ignore_unknown_fields = true });
        try testParse(
            \\{"other": "", "jsonrpc": "2.0", "extra": {}, "method": "bar"}
        , .{
            .notification = .{
                .method = "bar",
                .params = null,
            },
        }, .{ .ignore_unknown_fields = true });
        try testParseExpectedError(
            \\{"jsonrpc": "2.0", "id": 1, "other": null, ".": "Sie", "params": {}, "extra": {}}
        ,
            error.UnknownField,
            error.UnknownField,
            .{ .ignore_unknown_fields = false },
        );
    }

    test "emit_null_optional_fields" {
        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","method":"exit"}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .notification = .{ .method = "exit", .params = null } }, .{ .emit_null_optional_fields = false })});
        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","method":"exit","params":null}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .notification = .{ .method = "exit", .params = null } }, .{ .emit_null_optional_fields = true })});
        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","method":"exit","params":null}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .notification = .{ .method = "exit", .params = .null } }, .{ .emit_null_optional_fields = false })});
        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","method":"exit","params":null}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .notification = .{ .method = "exit", .params = .null } }, .{ .emit_null_optional_fields = true })});

        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","result":null}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .response = .{ .id = null, .result_or_error = .{ .result = null } } }, .{ .emit_null_optional_fields = false })});
        try std.testing.expectFmt(
            \\{"jsonrpc":"2.0","id":null,"result":null}
        , "{}", .{std.json.fmt(JsonRPCMessage{ .response = .{ .id = null, .result_or_error = .{ .result = null } } }, .{ .emit_null_optional_fields = true })});
    }

    fn testParse(message: []const u8, expected: JsonRPCMessage, parse_options: std.json.ParseOptions) !void {
        const allocator = std.testing.allocator;

        const parsed_from_slice = try std.json.parseFromSlice(JsonRPCMessage, allocator, message, parse_options);
        defer parsed_from_slice.deinit();

        const parsed_value = try std.json.parseFromSlice(std.json.Value, allocator, message, parse_options);
        defer parsed_value.deinit();

        const parsed_from_value = try std.json.parseFromValue(JsonRPCMessage, allocator, parsed_value.value, parse_options);
        defer parsed_from_value.deinit();

        const from_slice_stringified = try std.json.stringifyAlloc(allocator, parsed_from_slice.value, .{ .whitespace = .indent_2 });
        defer allocator.free(from_slice_stringified);

        const from_value_stringified = try std.json.stringifyAlloc(allocator, parsed_from_value.value, .{ .whitespace = .indent_2 });
        defer allocator.free(from_value_stringified);

        if (!std.mem.eql(u8, from_slice_stringified, from_value_stringified)) {
            std.debug.print(
                \\
                \\====== std.json.parseFromSlice: ======
                \\{s}
                \\====== std.json.parseFromValue: ======
                \\{s}
                \\======================================\
                \\
            , .{ from_slice_stringified, from_value_stringified });
            return error.TestExpectedEqual;
        }

        try expectEqual(parsed_from_slice.value, parsed_from_value.value);
        try expectEqual(parsed_from_slice.value, expected);
        try expectEqual(parsed_from_value.value, expected);
    }

    fn testParseExpectedError(
        message: []const u8,
        expected_parse_error: std.json.ParseError(std.json.Scanner),
        expected_parse_from_value_error: std.json.ParseFromValueError,
        parse_options: std.json.ParseOptions,
    ) !void {
        const allocator = std.testing.allocator;

        try std.testing.expectError(expected_parse_error, std.json.parseFromSlice(JsonRPCMessage, allocator, message, parse_options));

        const parsed_value = std.json.parseFromSlice(std.json.Value, allocator, message, parse_options) catch |err| {
            try std.testing.expectEqual(expected_parse_error, err);
            return;
        };
        defer parsed_value.deinit();

        try std.testing.expectError(expected_parse_from_value_error, std.json.parseFromValue(JsonRPCMessage, allocator, parsed_value.value, parse_options));
    }

    fn expectEqual(a: JsonRPCMessage, b: JsonRPCMessage) !void {
        try std.testing.expectEqual(std.meta.activeTag(a), std.meta.activeTag(b));
        switch (a) {
            .request => {
                try std.testing.expectEqualDeep(a.request.id, b.request.id);
                try std.testing.expectEqualStrings(a.request.method, b.request.method);

                // this only a shallow equality check
                try std.testing.expectEqual(a.request.params == null, b.request.params == null);
                if (a.request.params != null) {
                    try std.testing.expectEqual(std.meta.activeTag(a.request.params.?), std.meta.activeTag(b.request.params.?));
                }
            },
            .notification => {
                try std.testing.expectEqualStrings(a.notification.method, b.notification.method);

                // this only a shallow equality check
                try std.testing.expectEqual(a.notification.params == null, b.notification.params == null);
                if (a.notification.params != null) {
                    try std.testing.expectEqual(std.meta.activeTag(a.notification.params.?), std.meta.activeTag(b.notification.params.?));
                }
            },
            .response => {
                try std.testing.expectEqualDeep(a.response.id, b.response.id);
                try std.testing.expectEqual(std.meta.activeTag(a.response.result_or_error), std.meta.activeTag(b.response.result_or_error));

                switch (a.response.result_or_error) {
                    .result => {
                        // this only a shallow equality check
                        try std.testing.expectEqual(a.response.result_or_error.result == null, b.response.result_or_error.result == null);
                        if (a.response.result_or_error.result != null) {
                            try std.testing.expectEqual(std.meta.activeTag(a.response.result_or_error.result.?), std.meta.activeTag(b.response.result_or_error.result.?));
                        }
                    },
                    .@"error" => {
                        try std.testing.expectEqualDeep(a.response.result_or_error.@"error", b.response.result_or_error.@"error");
                    },
                }
            },
        }
    }
};

/// A minimal non-allocating parser for the LSP Base Protocol Header Part.
///
/// See https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#headerPart
pub const BaseProtocolHeader = struct {
    content_length: usize,

    pub const max_header_length: usize = 1024;

    pub const ParseError = error{
        EndOfStream,
        /// The message is longer than `std.math.maxInt(usize)`.
        OversizedMessage,
        /// The header field is longer than `max_header_length`. The ": " doesn't count towards the header field length.
        OversizedHeaderField,
        /// The header is missing the mandatory `Content-Length` field.
        MissingContentLength,
        /// The header field value of `Content-Length` is not a valid unsigned integer.
        InvalidContentLength,
        /// The header is ill-formed.
        InvalidHeaderField,
    };

    /// It is strongly recommended to provide a buffering reader because the parser has to read 1-byte at a time.
    pub inline fn parse(reader: anytype) (@TypeOf(reader).Error || ParseError)!BaseProtocolHeader {
        return @errorCast(parseAny(reader.any()));
    }

    /// It is strongly recommended to provide a buffering reader because the parser has to read 1-byte at a time.
    ///
    /// Type erased version of `parse`.
    pub fn parseAny(reader: std.io.AnyReader) (anyerror || ParseError)!BaseProtocolHeader {
        var content_length: ?usize = null;

        outer: while (true) {
            var maybe_colon_index: ?usize = null;

            var buffer: [max_header_length]u8 = undefined;
            var buffer_index: usize = 0;

            while (true) {
                const byte: u8 = try reader.readByte();
                switch (byte) {
                    '\n' => return error.InvalidHeaderField,
                    '\r' => {
                        if (try reader.readByte() != '\n') return error.InvalidHeaderField;
                        if (buffer_index == 0) break :outer;
                        break;
                    },
                    ':' => {
                        // The ": " is not being added to the buffer here!
                        if (try reader.readByte() != ' ') return error.InvalidHeaderField;
                        maybe_colon_index = buffer_index;
                    },
                    else => {
                        if (buffer_index == max_header_length) return error.OversizedHeaderField;
                        buffer[buffer_index] = byte;
                        buffer_index += 1;
                    },
                }
            }

            const colon_index = maybe_colon_index orelse return error.InvalidHeaderField;

            const header = buffer[0..buffer_index];
            const header_name = header[0..colon_index];
            const header_value = header[colon_index..];

            if (!std.ascii.eqlIgnoreCase(header_name, "content-length")) continue;

            content_length = std.fmt.parseUnsigned(usize, header_value, 10) catch |err| switch (err) {
                error.Overflow => return error.OversizedMessage,
                error.InvalidCharacter => return error.InvalidContentLength,
            };
        }

        return .{
            .content_length = content_length orelse return error.MissingContentLength,
        };
    }

    test parse {
        try expectParseError("", error.EndOfStream);
        try expectParseError("\n", error.InvalidHeaderField);
        try expectParseError("\n\r", error.InvalidHeaderField);
        try expectParseError("\r", error.EndOfStream);
        try expectParseError("\r\n", error.MissingContentLength);
        try expectParseError("\r\n\r\n", error.MissingContentLength);

        try expectParseError("content-length: 32\r\n", error.EndOfStream);
        try expectParseError("content-length 32\r\n\r\n", error.InvalidHeaderField);
        try expectParseError("content-length:32\r\n\r\n", error.InvalidHeaderField);
        try expectParseError("contentLength: 32\r\n\r\n", error.MissingContentLength);
        try expectParseError("content-length: abababababab\r\n\r\n", error.InvalidContentLength);
        try expectParseError("content-length: 9999999999999999999999999999999999\r\n\r\n", error.OversizedMessage);

        try expectParse("content-length: 32\r\n\r\n", .{ .content_length = 32 });
        try expectParse("Content-Length: 32\r\n\r\n", .{ .content_length = 32 });

        try expectParse("content-type: whatever\r\nContent-Length: 666\r\n\r\n", .{ .content_length = 666 });
        try expectParse("Content-Type: impostor\r\ncontent-length: 42\r\n\r\n", .{ .content_length = 42 });
    }

    pub fn format(
        header: BaseProtocolHeader,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        out_stream: anytype,
    ) !void {
        _ = options;
        if (fmt.len != 0) std.fmt.invalidFmtError(fmt, header);
        try std.fmt.format(out_stream, "Content-Length: {d}\r\n\r\n", .{header.content_length});
    }

    test format {
        try std.testing.expectFmt("Content-Length: 0\r\n\r\n", "{}", .{BaseProtocolHeader{ .content_length = 0 }});
        try std.testing.expectFmt("Content-Length: 42\r\n\r\n", "{}", .{BaseProtocolHeader{ .content_length = 42 }});
        try std.testing.expectFmt("Content-Length: 4294967295\r\n\r\n", "{}", .{BaseProtocolHeader{ .content_length = std.math.maxInt(u32) }});
    }

    fn expectParse(input: []const u8, expected_header: BaseProtocolHeader) !void {
        var fbs = std.io.fixedBufferStream(input);
        const actual_header = try parse(fbs.reader());
        try std.testing.expectEqual(expected_header.content_length, actual_header.content_length);
    }

    fn expectParseError(input: []const u8, expected_error: ParseError) !void {
        var fbs = std.io.fixedBufferStream(input);
        try std.testing.expectError(expected_error, parse(fbs.reader()));
    }
};

pub const TransportOverStdio = struct {
    impl: struct {
        in: std.io.BufferedReader(512, std.fs.File.Reader),
        out: std.fs.File,
    },

    pub fn init(in: std.fs.File, out: std.fs.File) TransportOverStdio {
        return .{ .impl = .{
            .in = std.io.bufferedReaderSize(512, in.reader()),
            .out = out,
        } };
    }

    pub fn any(transport: *TransportOverStdio) AnyTransport {
        return .{ .impl = .{
            .transport = transport,
            .readJsonMessage = @ptrCast(&readJsonMessage),
            .writeJsonMessage = @ptrCast(&writeJsonMessage),
        } };
    }

    pub fn readJsonMessage(transport: *TransportOverStdio, allocator: std.mem.Allocator) (std.mem.Allocator.Error || AnyTransport.ReadError)![]u8 {
        const reader = transport.impl.in.reader().any();

        const header = BaseProtocolHeader.parseAny(reader) catch |err| return @as(AnyTransport.ReadError, @errorCast(err));

        const json_message = try allocator.alloc(u8, header.content_length);
        errdefer allocator.free(json_message);

        reader.readNoEof(json_message) catch |err| return @as(AnyTransport.ReadError, @errorCast(err));

        return json_message;
    }

    pub fn writeJsonMessage(transport: *TransportOverStdio, json_message: []const u8) AnyTransport.WriteError!void {
        const header: BaseProtocolHeader = .{ .content_length = json_message.len };

        var buffer: [64]u8 = undefined;
        const prefix = std.fmt.bufPrint(&buffer, "{}", .{header}) catch unreachable;

        var iovecs = [_]std.posix.iovec_const{
            .{ .base = prefix.ptr, .len = prefix.len },
            .{ .base = json_message.ptr, .len = json_message.len },
        };

        try transport.impl.out.writevAll(&iovecs);
    }
};

test TransportOverStdio {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const send_to_client = try tmp_dir.dir.createFile("a", .{});
    const send_to_server = try tmp_dir.dir.createFile("b", .{});

    const receive_from_server = try tmp_dir.dir.openFile("a", .{});
    const receive_from_client = try tmp_dir.dir.openFile("b", .{});

    var server_transport = TransportOverStdio.init(receive_from_client, send_to_client);
    var client_transport = TransportOverStdio.init(receive_from_server, send_to_server);

    // Server -> Client
    try server_transport.writeJsonMessage("\"hello from server\"");

    // Client <- Server
    const message_from_server = try client_transport.readJsonMessage(std.testing.allocator);
    defer std.testing.allocator.free(message_from_server);
    try std.testing.expectEqualStrings("\"hello from server\"", message_from_server);

    // Client -> Server
    try client_transport.any().writeJsonMessage("\"hello from client\"");

    // Server <- Client
    const message_from_client = try server_transport.readJsonMessage(std.testing.allocator);
    defer std.testing.allocator.free(message_from_client);

    try std.testing.expectEqualStrings("\"hello from client\"", message_from_client);

    var buffer: [512]u8 = undefined;
    try std.testing.expectEqual(0, receive_from_server.read(&buffer));
    try std.testing.expectEqual(0, receive_from_client.read(&buffer));

    try std.testing.expectError(error.EndOfStream, server_transport.readJsonMessage(std.testing.allocator));
    try std.testing.expectError(error.EndOfStream, client_transport.readJsonMessage(std.testing.allocator));

    try std.testing.expectError(error.EndOfStream, server_transport.any().readJsonMessage(std.testing.allocator));
    try std.testing.expectError(error.EndOfStream, client_transport.any().readJsonMessage(std.testing.allocator));
}

/// This implementation is totally untested and probably completely wrong. Who is going to use this anyway...
const TransportOverStream = struct {
    impl: struct {
        server: std.net.Server,
        connection: std.net.Server.Connection,
        buffered_reader: std.io.BufferedReader(512, std.net.Stream.Reader),
    },

    pub fn initSocket(address: std.net.Address, options: std.net.Address.ListenOptions) (std.net.Address.ListenError || std.net.Server.AcceptError)!TransportOverStream {
        var server = try address.listen(options);
        errdefer server.deinit();
        const connection = try server.accept();
        errdefer connection.stream.close();

        return .{ .impl = .{
            .server = server,
            .connection = connection,
            .buffered_reader = std.io.bufferedReaderSize(512, connection.stream.reader()),
        } };
    }

    pub fn deinit(transport: *TransportOverStream) void {
        transport.impl.connection.stream.close();
        transport.impl.server.deinit();
        transport.* = undefined;
    }

    pub fn any(transport: *TransportOverStream) AnyTransport {
        return .{ .impl = .{
            .transport = transport,
            .readJsonMessage = @ptrCast(&readJsonMessage),
            .writeJsonMessage = @ptrCast(&writeJsonMessage),
        } };
    }

    pub fn readJsonMessage(transport: *TransportOverStream, allocator: std.mem.Allocator) (std.mem.Allocator.Error || AnyTransport.ReadError)![]u8 {
        const reader = transport.impl.buffered_reader.reader().any();

        const header = BaseProtocolHeader.parseAny(reader) catch |err| return @as(AnyTransport.ReadError, @errorCast(err));

        const json_message = try allocator.alloc(u8, header.content_length);
        errdefer allocator.free(json_message);

        reader.readNoEof(json_message) catch |err| return @as(AnyTransport.ReadError, @errorCast(err));

        return json_message;
    }

    pub fn writeJsonMessage(transport: *TransportOverStream, json_message: []const u8) AnyTransport.WriteError!void {
        const header: BaseProtocolHeader = .{ .content_length = json_message.len };

        var buffer: [64]u8 = undefined;
        const prefix = std.fmt.bufPrint(&buffer, "{}", .{header}) catch unreachable;

        var iovecs = [_]std.posix.iovec_const{
            .{ .base = prefix.ptr, .len = prefix.len },
            .{ .base = json_message.ptr, .len = json_message.len },
        };

        try transport.impl.connection.stream.writevAll(&iovecs);
    }
};

pub const ThreadSafeTransportConfig = struct {
    ChildTransport: type,
    /// Makes `readJsonMessage` thread-safe.
    thread_safe_read: bool,
    /// Makes `writeJsonMessage` thread-safe.
    thread_safe_write: bool,
    MutexType: ?type = null,
};

/// Wraps a non-thread-safe transport and makes it thread-safe.
pub fn ThreadSafeTransport(config: ThreadSafeTransportConfig) type {
    return struct {
        child_transport: config.ChildTransport,
        in_mutex: @TypeOf(in_mutex_init) = in_mutex_init,
        out_mutex: @TypeOf(out_mutex_init) = out_mutex_init,

        // Is there any better name of this?
        const Self = @This();

        pub fn any(transport: *Self) AnyTransport {
            return .{ .impl = .{
                .transport = transport,
                .readJsonMessage = @ptrCast(&readJsonMessage),
                .writeJsonMessage = @ptrCast(&writeJsonMessage),
            } };
        }

        pub fn readJsonMessage(transport: *Self, allocator: std.mem.Allocator) (std.mem.Allocator.Error || AnyTransport.ReadError)![]u8 {
            transport.in_mutex.lock();
            defer transport.in_mutex.unlock();

            return try transport.child_transport.readJsonMessage(allocator);
        }

        pub fn writeJsonMessage(transport: *Self, json_message: []const u8) AnyTransport.WriteError!void {
            transport.out_mutex.lock();
            defer transport.out_mutex.unlock();

            return try transport.child_transport.writeJsonMessage(json_message);
        }

        const in_mutex_init = if (config.MutexType) |T|
            T{}
        else if (config.thread_safe_read)
            std.Thread.Mutex{}
        else
            DummyMutex{};

        const out_mutex_init = if (config.MutexType) |T|
            T{}
        else if (config.thread_safe_write)
            std.Thread.Mutex{}
        else
            DummyMutex{};

        const DummyMutex = struct {
            fn lock(_: *DummyMutex) void {}
            fn unlock(_: *DummyMutex) void {}
        };
    };
}

/// A type-erased Transport.
pub const AnyTransport = struct {
    impl: struct {
        transport: *anyopaque,
        readJsonMessage: *const fn (transport: *anyopaque, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ReadError)![]u8,
        writeJsonMessage: *const fn (transport: *anyopaque, json_message: []const u8) WriteError!void,
    },

    pub const ReadError = std.posix.ReadError || error{EndOfStream} || BaseProtocolHeader.ParseError;
    pub const WriteError = std.posix.WriteError;

    pub fn readJsonMessage(transport: AnyTransport, allocator: std.mem.Allocator) (std.mem.Allocator.Error || ReadError)![]u8 {
        return try transport.impl.readJsonMessage(transport.impl.transport, allocator);
    }

    pub fn writeJsonMessage(transport: AnyTransport, json_message: []const u8) WriteError!void {
        try transport.impl.writeJsonMessage(transport.impl.transport, json_message);
    }
};

pub const MethodWithParams = struct {
    method: []const u8,
    /// The `std.json.Value` can only be `.null`, `.array` or `.object`.
    ///
    /// `params == null` means that the was no `"params"` field. `params == .null` means that the `"params"` field was set to `null`.
    params: ?std.json.Value,
};

pub const MessageConfig = struct {
    /// Must be a tagged union with the following properties:
    ///   - the field name is the method name of a request message
    ///   - the field type must be the params type of the method (i.e. `ParamsType(field.name)`)
    ///
    /// There must also be a field named `other` of type `MethodWithParams`.
    /// It will be set with the method name and params if request does not match any of the explicitly specified params.
    ///
    /// Example:
    /// ```zig
    /// union(enum) {
    ///     /// The go to implementation request is sent from the client to the server to resolve the implementation location of a symbol at a given text document position.
    ///     @"textDocument/implementation": ImplementationParams,
    ///     /// The Completion request is sent from the client to the server to compute completion items at a given cursor position.
    ///     @"textDocument/completion": CompletionParams,
    ///     /// The shutdown request is sent from the client to the server. It asks the server to shut down, but to not exit.
    ///     shutdown,
    ///     /// The request is not one of the above.
    ///     other: MethodWithParams,
    /// }
    /// ```
    RequestParams: type,
    /// Must be a tagged union with the following properties:
    ///   - the field name is the method name of a notification message
    ///   - the field type must be the params type of the method (i.e. `ParamsType(field.name)`)
    ///
    /// There must also be a field named `other` of type `MethodWithParams`.
    /// It will be set with the method name and params if notification does not match any of the explicitly specified params.
    ///
    /// Example:
    /// ```zig
    /// union(enum) {
    ///     /// The document open notification is sent from the client to the server to signal newly opened text documents.
    ///     @"textDocument/didOpen": DidOpenTextDocumentParams,
    ///     /// The document change notification is sent from the client to the server to signal changes to a text document.
    ///     @"textDocument/didChange": DidChangeTextDocumentParams,
    ///     /// A notification to ask the server to exit its process.
    ///     exit,
    ///     /// The notification is not one of the above.
    ///     other: MethodWithParams,
    /// }
    /// ```
    NotificationParams: type,
};

pub fn Message(comptime config: MessageConfig) type {
    // TODO validate `config.RequestParams` and `config.NotificationParams`
    // This level of comptime should be illegal...
    return union(enum) {
        request: Request,
        notification: Notification,
        response: JsonRPCMessage.Response,

        const Msg = @This();

        pub const Request = struct {
            comptime jsonrpc: []const u8 = "2.0",
            id: JsonRPCMessage.ID,
            params: Params,

            pub const Params = config.RequestParams;

            pub const jsonParse = @compileError("Parsing a Request directly is not implemented! try to parse the Message instead.");
            pub const jsonParseFromValue = @compileError("Parsing a Request directly is not implemented! try to parse the Message instead.");

            pub fn jsonStringify(request: Request, stream: anytype) @TypeOf(stream.*).Error!void {
                try stream.beginObject();

                try stream.objectField("jsonrpc");
                try stream.write("2.0");

                try stream.objectField("id");
                try stream.write(request.id);

                try jsonStringifyParams(request.params, stream);

                try stream.endObject();
            }
        };

        pub const Notification = struct {
            comptime jsonrpc: []const u8 = "2.0",
            params: Params,

            pub const Params = config.NotificationParams;

            pub const jsonParse = @compileError("Parsing a Notification directly is not implemented! try to parse the Message instead.");
            pub const jsonParseFromValue = @compileError("Parsing a Notification directly is not implemented! try to parse the Message instead.");

            pub fn jsonStringify(notification: Notification, stream: anytype) @TypeOf(stream.*).Error!void {
                try stream.beginObject();

                try stream.objectField("jsonrpc");
                try stream.write("2.0");

                try jsonStringifyParams(notification.params, stream);

                try stream.endObject();
            }
        };

        pub fn fromJsonRPCMessage(message: JsonRPCMessage, allocator: std.mem.Allocator, options: std.json.ParseOptions) std.json.ParseFromValueError!Msg {
            switch (message) {
                .request => |item| {
                    var params: Request.Params = undefined;
                    if (methodToParamsParserMap(Request.Params, std.json.Value).get(item.method)) |parse| {
                        params = parse(item.params orelse .null, allocator, options) catch |err|
                            return if (item.params == null) error.MissingField else err;
                    } else if (isNotificationMethod(item.method)) {
                        return error.UnexpectedToken; //  "method" is a notification but there was a "id" field
                    } else {
                        params = .{ .other = .{ .method = item.method, .params = item.params } };
                    }
                    return .{ .request = .{ .id = item.id, .params = params } };
                },
                .notification => |item| {
                    var params: Notification.Params = undefined;
                    if (methodToParamsParserMap(Notification.Params, std.json.Value).get(item.method)) |parse| {
                        params = parse(item.params orelse .null, allocator, options) catch |err|
                            return if (item.params == null) error.MissingField else err;
                    } else if (isRequestMethod(item.method)) {
                        return error.MissingField; //  "method" is a request but there was no "id" field
                    } else {
                        params = .{ .other = .{ .method = item.method, .params = item.params } };
                    }
                    return .{ .notification = .{ .params = params } };
                },
                .response => |response| return .{ .response = response },
            }
        }

        pub const jsonParse = @compileError(
            \\
            \\Implementing an efficient streaming parser for a LSP message is almost impossible!
            \\Please read the entire json message into a buffer and then call `jsonParseFromSlice` like this:
            \\
            \\const json_message =
            \\    \\{
            \\    \\    "jsonrpc": "2.0",
            \\    \\    "id": 1,
            \\    \\    "method": "textDocument/completion",
            \\    \\    "params": {
            \\    \\        ...
            \\    \\    }
            \\    \\}
            \\;
            \\
            \\const Message = lsp.Message(..., ...);
            \\const lsp_message = try Message.jsonParseFromSlice(allocator, json_message, .{});
            \\
        );

        pub fn jsonParseFromValue(
            allocator: std.mem.Allocator,
            source: std.json.Value,
            options: std.json.ParseOptions,
        ) std.json.ParseFromValueError!Msg {
            const message = try JsonRPCMessage.jsonParseFromValue(allocator, source, options);
            return try fromJsonRPCMessage(message, allocator, options);
        }

        pub fn jsonStringify(message: Msg, stream: anytype) @TypeOf(stream.*).Error!void {
            switch (message) {
                inline else => |item| try stream.write(item),
            }
        }

        pub fn parseFromSlice(
            allocator: std.mem.Allocator,
            s: []const u8,
            /// Only `std.json.ParseOptions.duplicate_field_behavior == .@"error"` is supported.
            options: std.json.ParseOptions,
        ) std.json.ParseError(std.json.Scanner)!std.json.Parsed(Msg) {
            var parsed: std.json.Parsed(Msg) = .{
                .arena = try allocator.create(std.heap.ArenaAllocator),
                .value = undefined,
            };
            errdefer allocator.destroy(parsed.arena);
            parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
            errdefer parsed.arena.deinit();

            parsed.value = try parseFromSliceLeaky(parsed.arena.allocator(), s, options);

            return parsed;
        }

        pub fn parseFromSliceLeaky(
            allocator: std.mem.Allocator,
            s: []const u8,
            /// Only `std.json.ParseOptions.duplicate_field_behavior == .@"error"` is supported.
            param_options: std.json.ParseOptions,
        ) std.json.ParseError(std.json.Scanner)!Msg {
            std.debug.assert(param_options.duplicate_field_behavior == .@"error"); // any other behavior is unsupported

            var source = std.json.Scanner.initCompleteInput(allocator, s);
            defer source.deinit();

            var options = param_options;
            if (options.max_value_len == null) {
                options.max_value_len = source.input.len;
            }
            if (options.allocate == null) {
                options.allocate = .alloc_if_needed;
            }

            if (try source.next() != .object_begin)
                return error.UnexpectedToken;

            var jsonrpc: ?[]const u8 = null;
            var id: ?JsonRPCMessage.ID = null;
            var saw_result_field: bool = false;
            var state: ParserState = .unknown;

            while (true) {
                const field_name = blk: {
                    const name_token = try source.nextAllocMax(allocator, .alloc_if_needed, options.max_value_len.?);
                    const maybe_field_name = switch (name_token) {
                        .string, .allocated_string => |slice| std.meta.stringToEnum(std.meta.FieldEnum(JsonRPCMessage.Fields), slice),
                        .object_end => break, // No more fields.
                        else => return error.UnexpectedToken,
                    };

                    switch (name_token) {
                        .string => {},
                        .allocated_string => |slice| allocator.free(slice),
                        else => unreachable,
                    }

                    break :blk maybe_field_name orelse {
                        if (options.ignore_unknown_fields) {
                            try source.skipValue();
                            continue;
                        } else {
                            return error.UnknownField;
                        }
                    };
                };

                switch (field_name) {
                    .jsonrpc => {
                        if (jsonrpc != null)
                            return error.DuplicateField;
                        jsonrpc = try std.json.innerParse([]const u8, allocator, &source, options);
                        continue;
                    },
                    .id => {
                        if (id != null)
                            return error.DuplicateField;
                        id = try std.json.innerParse(?JsonRPCMessage.ID, allocator, &source, options);
                        continue;
                    },
                    else => {},
                }

                if (state == .method and field_name == .params) {
                    state = try parseParamsFromTokenStreamWithKnownMethod(allocator, &source, state.method, options);
                    continue;
                } else if (state == .params and field_name == .method) {
                    const method = try std.json.innerParse([]const u8, allocator, &source, options);
                    var params_source = state.params;
                    state = try parseParamsFromTokenStreamWithKnownMethod(allocator, &params_source, method, options);
                    continue;
                }

                switch (state) {
                    .request, .notification => switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method, .params => return error.DuplicateField,
                        .result, .@"error" => return error.UnexpectedToken,
                    },
                    .uninteresting_request_or_notification => switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method, .params => return error.DuplicateField,
                        .result, .@"error" => return error.UnexpectedToken,
                    },
                    .method => switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method => return error.DuplicateField,
                        .params => unreachable, // checked above
                        .result, .@"error" => return error.UnexpectedToken, // the "method" field indicates a request or notification which can't have the "result" or "error" field
                    },
                    .params => switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method => unreachable, // checked above
                        .params => return error.DuplicateField,
                        .result, .@"error" => return error.UnexpectedToken, // the "params" field indicates a request or notification which can't have the "result" or "error" field
                    },
                    .response_result => |result| switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method, .params => return error.UnexpectedToken, // the "result" field indicates a response which can't have the "method" or "params" field
                        .result => return error.DuplicateField,
                        .@"error" => {
                            // Allows { "result": null, "error": {...} }
                            if (result != .null)
                                return error.UnexpectedToken;
                            state = .{ .response_error = try std.json.innerParse(JsonRPCMessage.Response.Error, allocator, &source, options) };
                            continue;
                        },
                    },
                    .response_error => switch (field_name) {
                        .jsonrpc, .id => unreachable, // checked above
                        .method, .params => return error.UnexpectedToken, // the "error" field indicates a response which can't have the "method" or "params" field
                        .result => {
                            if (saw_result_field)
                                return error.DuplicateField;
                            saw_result_field = true;

                            // Allows { "error": {...}, "result": null }
                            switch (try source.peekNextTokenType()) {
                                .null => {
                                    std.debug.assert(try source.next() == .null);
                                    continue;
                                },
                                else => return error.UnexpectedToken,
                            }
                        },
                        .@"error" => return error.DuplicateField,
                    },
                    .unknown => {
                        state = switch (field_name) {
                            .jsonrpc, .id => unreachable, // checked above
                            .method => .{ .method = try std.json.innerParse([]const u8, allocator, &source, options) },
                            .params => {
                                // Save the `std.json.Scanner` for later and skip the params.
                                state = .{ .params = source };
                                try source.skipValue();
                                continue;
                            },
                            .result => .{ .response_result = try std.json.Value.jsonParse(allocator, &source, options) },
                            .@"error" => .{ .response_error = try std.json.innerParse(JsonRPCMessage.Response.Error, allocator, &source, options) },
                        };
                        continue;
                    },
                }
                @compileError("Where is the fallthrough Lebowski?");
            }

            if (state == .method) {
                // The "params" field is missing. We will artifically insert `"params": null` to see `null` is a valid params field.
                var null_scanner = std.json.Scanner.initCompleteInput(allocator, "null");
                state = parseParamsFromTokenStreamWithKnownMethod(allocator, &null_scanner, state.method, options) catch
                    return error.MissingField;
                if (state == .uninteresting_request_or_notification) {
                    std.debug.assert(state.uninteresting_request_or_notification.params.? == .null);
                    state.uninteresting_request_or_notification.params = null;
                }
            }

            const jsonrpc_value = jsonrpc orelse
                return error.MissingField;
            if (!std.mem.eql(u8, jsonrpc_value, "2.0"))
                return error.UnexpectedToken; // the "jsonrpc" field must be "2.0"

            switch (state) {
                .unknown => return error.MissingField, // Where, fields?
                .params => return error.MissingField, // missing "method" field
                .method => unreachable, // checked above
                .response_result => |result| return .{
                    .response = .{
                        .id = id,
                        .result_or_error = .{ .result = result },
                    },
                },
                .response_error => |err| return .{
                    .response = .{
                        .id = id,
                        .result_or_error = .{ .@"error" = err },
                    },
                },
                .request => |params| return .{
                    .request = .{
                        .id = id orelse
                            return error.MissingField, // "method" is a request but there was not "id" field
                        .params = params,
                    },
                },
                .notification => |params| {
                    if (id != null)
                        return error.UnexpectedToken; // "method" is a notification but there was a "id" field
                    return .{ .notification = .{ .params = params } };
                },
                .uninteresting_request_or_notification => |method_with_params| {
                    // The method could be from a future protocol version so we don't return an error here.
                    if (id) |request_id| {
                        return .{ .request = .{ .id = request_id, .params = .{ .other = method_with_params } } };
                    } else {
                        return .{ .notification = .{ .params = .{ .other = method_with_params } } };
                    }
                },
            }
        }

        fn parseParamsFromTokenStreamWithKnownMethod(
            allocator: std.mem.Allocator,
            /// Must be either `*std.json.Scanner` or `*std.json.Reader`.
            params_source: anytype,
            method: []const u8,
            options: std.json.ParseOptions,
        ) std.json.ParseError(std.json.Scanner)!ParserState {
            std.debug.assert(options.duplicate_field_behavior == .@"error"); // any other behavior is unsupported

            if (methodToParamsParserMap(Request.Params, @TypeOf(params_source)).get(method)) |parse| {
                return .{ .request = try parse(params_source, allocator, options) };
            } else if (methodToParamsParserMap(Notification.Params, @TypeOf(params_source)).get(method)) |parse| {
                return .{ .notification = try parse(params_source, allocator, options) };
            } else {
                // TODO make it configurable if the params should be parsed
                const params = switch (try params_source.peekNextTokenType()) {
                    .null => blk: {
                        std.debug.assert(try params_source.next() == .null);
                        break :blk .null;
                    },
                    .object_begin, .array_begin => try std.json.Value.jsonParse(allocator, params_source, options),
                    else => return error.UnexpectedToken, // "params" field must be null/object/array
                };
                return .{ .uninteresting_request_or_notification = .{ .method = method, .params = params } };
            }
        }

        fn ParamsParserFunc(
            /// Must be either `Request.Params` or `Notification.Params`.
            comptime Params: type,
            /// Must be either `*std.json.Scanner`, `*std.json.Reader` or `std.json.Value`.
            comptime Source: type,
        ) type {
            comptime std.debug.assert(Params == Request.Params or Params == Notification.Params);
            const Error = if (Source == std.json.Value) std.json.ParseFromValueError else std.json.ParseError(std.meta.Child(Source));
            return *const fn (params_source: Source, allocator: std.mem.Allocator, options: std.json.ParseOptions) Error!Params;
        }

        inline fn methodToParamsParserMap(
            /// Must be either `Request.Params` or `Notification.Params`.
            comptime Params: type,
            /// Must be either `*std.json.Scanner`, `*std.json.Reader` or `std.json.Value`.
            comptime Source: type,
        ) parser.StaticStringMap(ParamsParserFunc(Params, Source)) {
            comptime {
                const fields = std.meta.fields(Params);

                var kvs_list: [fields.len - 1]struct { []const u8, ParamsParserFunc(Params, Source) } = undefined;

                var i: usize = 0;
                for (fields) |field| {
                    if (std.mem.eql(u8, field.name, "other"))
                        continue;

                    const parse_func = struct {
                        fn parse(params_source: Source, allocator: std.mem.Allocator, options: std.json.ParseOptions) !Params {
                            if (field.type == void) {
                                switch (Source) {
                                    std.json.Value => if (params_source != .null) {
                                        return error.UnexpectedToken;
                                    },
                                    else => {
                                        if (try params_source.peekNextTokenType() != .null)
                                            return error.UnexpectedToken;
                                        std.debug.assert(try params_source.next() == .null);
                                    },
                                }
                                return @unionInit(Params, field.name, if (field.type == void) {} else null);
                            }
                            const params = switch (Source) {
                                std.json.Value => try std.json.innerParseFromValue(field.type, allocator, params_source, options),
                                else => try std.json.innerParse(field.type, allocator, params_source, options),
                            };
                            return @unionInit(Params, field.name, params);
                        }
                    }.parse;

                    kvs_list[i] = .{ field.name, parse_func };
                    i += 1;
                }

                return parser.staticStringMapInitComptime(ParamsParserFunc(Params, Source), kvs_list);
            }
        }

        fn jsonStringifyParams(self: anytype, stream: anytype) @TypeOf(stream.*).Error!void {
            std.debug.assert(@TypeOf(self) == Request.Params or @TypeOf(self) == Notification.Params);

            switch (self) {
                .other => |method_with_params| {
                    try stream.objectField("method");
                    try stream.write(method_with_params.method);

                    if (method_with_params.params) |params_val| {
                        try stream.objectField("params");
                        try stream.write(params_val);
                    } else if (stream.options.emit_null_optional_fields) {
                        try stream.objectField("params");
                        try stream.write(null);
                    }
                },
                inline else => |params, method| {
                    try stream.objectField("method");
                    try stream.write(method);

                    if (@TypeOf(params) == void) {
                        if (stream.options.emit_null_optional_fields) {
                            try stream.objectField("params");
                            try stream.write(null);
                        }
                    } else {
                        try stream.objectField("params");
                        try stream.write(params);
                    }
                },
            }
        }

        const ParserState = union(enum) {
            /// The `"method"` and `"params"` field has been encountered. The method is part of `Request.Params`.
            request: Request.Params,
            /// The `"method"` and `"params"` field has been encountered. The method is part of `Notification.Params`.
            notification: Notification.Params,
            /// The `"method"` field has been encountered and `"params"` field has been encountered.
            /// The method is valid but not part of `Request.Params` nor `Notification.Params`.
            uninteresting_request_or_notification: MethodWithParams,
            /// Only the `"method"` field has been encountered.
            method: []const u8,
            /// Only the `"params"` field has been encountered.
            ///
            /// The field will not be parsed until the `"method"` field has been encountered.
            /// Once the `"method"` has been encountered, the `std.json.Scanner` is used to jump back to where the `"params"` field was located.
            params: std.json.Scanner, // TODO optimize @sizeof()
            /// Only the `"result"` field has been encountered.
            response_result: std.json.Value,
            /// Only the `"error"` field has been encountered.
            response_error: JsonRPCMessage.Response.Error,
            /// initial state
            unknown,
        };

        fn expectEqual(a: Msg, b: Msg) !void {
            try std.testing.expectEqual(std.meta.activeTag(a), std.meta.activeTag(b));
            switch (a) {
                .request => {
                    try std.testing.expectEqualDeep(a.request.id, b.request.id);
                    try std.testing.expectEqualDeep(std.meta.activeTag(a.request.params), std.meta.activeTag(b.request.params));
                },
                .notification => {
                    try std.testing.expectEqualDeep(std.meta.activeTag(a.notification.params), std.meta.activeTag(b.notification.params));
                },
                .response => {
                    try std.testing.expectEqualDeep(a.response.id, b.response.id);
                    try std.testing.expectEqual(std.meta.activeTag(a.response.result_or_error), std.meta.activeTag(b.response.result_or_error));
                    switch (a.response.result_or_error) {
                        .@"error" => {
                            const a_err = a.response.result_or_error.@"error";
                            const b_err = b.response.result_or_error.@"error";
                            try std.testing.expectEqual(a_err.code, b_err.code);
                            try std.testing.expectEqualStrings(a_err.message, b_err.message);
                        },
                        .result => {
                            const a_result = a.response.result_or_error.result;
                            const b_result = b.response.result_or_error.result;
                            try std.testing.expectEqual(a_result == null, b_result == null);
                            try std.testing.expectEqual(std.meta.activeTag(a_result orelse .null), std.meta.activeTag(b_result orelse .null));
                        },
                    }
                },
            }
        }
    };
}

const ExampleRequestMethods = union(enum) {
    @"textDocument/implementation": types.ImplementationParams,
    @"textDocument/completion": types.CompletionParams,
    shutdown,
    other: MethodWithParams,
};

const ExampleNotificationMethods = union(enum) {
    initialized: types.InitializedParams,
    @"textDocument/didChange": types.DidChangeTextDocumentParams,
    exit,
    other: MethodWithParams,
};

const ExampleMessage = Message(.{
    .RequestParams = ExampleRequestMethods,
    .NotificationParams = ExampleNotificationMethods,
});

fn testMessage(message: ExampleMessage, json_message: []const u8) !void {
    try testMessageWithOptions(message, json_message, .{});
}

fn testMessageWithOptions(
    message: ExampleMessage,
    json_message: []const u8,
    parse_options: std.json.ParseOptions,
) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const value = try std.json.parseFromSliceLeaky(std.json.Value, arena.allocator(), json_message, parse_options);
    const message_from_value = try std.json.parseFromValueLeaky(ExampleMessage, arena.allocator(), value, parse_options);
    const message_from_slice = try ExampleMessage.parseFromSliceLeaky(arena.allocator(), json_message, parse_options);

    const message_string = try std.json.stringifyAlloc(arena.allocator(), message, .{ .whitespace = .indent_2 });
    const message_from_value_string = try std.json.stringifyAlloc(arena.allocator(), message_from_value, .{ .whitespace = .indent_2 });
    const message_from_slice_string = try std.json.stringifyAlloc(arena.allocator(), message_from_slice, .{ .whitespace = .indent_2 });

    try std.testing.expectEqualStrings(message_string, message_from_value_string);
    try std.testing.expectEqualStrings(message_string, message_from_slice_string);

    try ExampleMessage.expectEqual(message, message_from_value);
    try ExampleMessage.expectEqual(message, message_from_slice);
}

fn testMessageExpectError(expected_error: anytype, json_message: []const u8) !void {
    try testMessageExpectErrorWithOptions(expected_error, json_message, .{});
}

fn testMessageExpectErrorWithOptions(expected_error: anytype, json_message: []const u8, parse_options: std.json.ParseOptions) !void {
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const message_from_slice = ExampleMessage.parseFromSliceLeaky(arena.allocator(), json_message, parse_options);
    try std.testing.expectError(expected_error, message_from_slice);

    const value = std.json.parseFromSliceLeaky(std.json.Value, arena.allocator(), json_message, parse_options) catch |actual_error| {
        try std.testing.expectError(expected_error, @as(@TypeOf(actual_error)!void, actual_error));
        return;
    };
    const message_from_value = std.json.parseFromValueLeaky(ExampleMessage, arena.allocator(), value, parse_options);
    try std.testing.expectError(expected_error, message_from_value);
}

// A lot of these tests can be restructured once https://github.com/ziglang/zig/issues/19295 has been resolved.

test Message {
    try testMessageExpectError(error.UnexpectedToken, "5");
    try testMessageExpectError(error.MissingField, "{}");
    try testMessageExpectError(error.UnexpectedEndOfInput, "{");
    try testMessageExpectError(error.UnknownField, "{\"\\uaaaa\": 5}");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"foo\", \"params\": null }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"1.0\", \"method\": \"foo\", \"params\": null }");
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnknownField, "{ \"foo\": null }");

    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"1.0\", \"method\": \"foo\", \"params\": null }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\", \"method\": 5      , \"params\": null }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\", \"method\": \"foo\", \"params\": true }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\", \"method\": \"foo\",  \"id\": true    , \"params\": null }");
}

test "Message - duplicate fields" {
    // https://github.com/ziglang/zig/pull/20430
    if (comptime @import("builtin").zig_version.order(std.SemanticVersion.parse("0.14.0-dev.149+8f7b50e2c") catch unreachable) == .lt) return error.SkipZigTest;

    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"2.0\"     , \"jsonrpc\": \"2.0\"     }");
    try testMessageExpectError(error.DuplicateField, "{ \"id\": 5                , \"id\": 5                }");
    try testMessageExpectError(error.DuplicateField, "{ \"method\": \"shutdown\" , \"method\": \"shutdown\" }");
    try testMessageExpectError(error.DuplicateField, "{ \"params\": null         , \"params\": null         }");
    try testMessageExpectError(error.DuplicateField, "{ \"result\": null         , \"result\": null         }");
    try testMessageExpectError(error.DuplicateField, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"error\": {\"code\": 0, \"message\": \"\"} }");

    try testMessageExpectError(error.DuplicateField, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null , \"result\": null }");
    try testMessageExpectError(error.DuplicateField, "{ \"result\": null , \"error\": {\"code\": 0, \"message\": \"\"} , \"error\": {\"code\": 0, \"message\": \"\"}}");

    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"shutdown\" , \"params\": null, \"method\": \"foo\" }");
    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"shutdown\" , \"params\": null, \"params\": null    }");
    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"exit\"     , \"params\": null, \"method\": \"foo\" }");
    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"exit\"     , \"params\": null, \"params\": null    }");
    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"foo\"      , \"params\": null, \"method\": \"foo\" }");
    try testMessageExpectError(error.DuplicateField, "{ \"jsonrpc\": \"1.0\", \"method\": \"foo\"      , \"params\": null, \"params\": null    }");
}

test "Message - ignore_unknown_fields" {
    try testMessageWithOptions(
        .{
            .notification = .{
                .params = .{
                    .other = .{ .method = "foo", .params = .null },
                },
            },
        },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "other": 5,
        \\    "method": "foo",
        \\    "params": null
        \\}
    ,
        .{ .ignore_unknown_fields = true },
    );
}

test "Message - stringify emit_null_optional_fields" {
    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"exit"}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .exit } }, .{ .emit_null_optional_fields = false })});
    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"exit","params":null}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .exit } }, .{ .emit_null_optional_fields = true })});

    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"foo"}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .{ .other = .{ .method = "foo", .params = null } } } }, .{ .emit_null_optional_fields = false })});
    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"foo","params":null}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .{ .other = .{ .method = "foo", .params = null } } } }, .{ .emit_null_optional_fields = true })});
    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"foo","params":null}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .{ .other = .{ .method = "foo", .params = .null } } } }, .{ .emit_null_optional_fields = false })});
    try std.testing.expectFmt(
        \\{"jsonrpc":"2.0","method":"foo","params":null}
    , "{}", .{std.json.fmt(ExampleMessage{ .notification = .{ .params = .{ .other = .{ .method = "foo", .params = .null } } } }, .{ .emit_null_optional_fields = true })});
}

test "Message.Request" {
    try testMessage(.{
        .request = .{
            .id = .{ .string = "id" },
            .params = .{ .@"textDocument/completion" = .{
                .textDocument = .{ .uri = "foo" },
                .position = .{ .line = 1, .character = 2 },
            } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": "id",
        \\    "method": "textDocument/completion",
        \\    "params": {
        \\        "textDocument": { "uri": "foo" },
        \\        "position": { "line": 1, "character": 2 }
        \\    }
        \\}
    );
}

test "Message.Request - null params" {
    try testMessage(.{
        .request = .{
            .id = .{ .number = 3 },
            .params = .shutdown,
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": 3,
        \\    "method": "shutdown",
        \\    "params": null
        \\}
    );
    try testMessage(.{
        .request = .{
            .id = .{ .string = "bar" },
            .params = .{ .other = .{ .method = "foo", .params = null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "method": "foo",
        \\    "id": "bar"
        \\}
    );
}

test "Message.Request - unknown method" {
    try testMessage(.{
        .request = .{
            .id = .{ .string = "some-id" },
            .params = .{ .other = .{ .method = "some/method", .params = .null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": "some-id",
        \\    "method": "some/method",
        \\    "params": null
        \\}
    );
    try testMessage(.{
        .request = .{
            .id = .{ .string = "some-id" },
            .params = .{ .other = .{ .method = "some/method", .params = .{ .array = std.json.Array.init(std.testing.allocator) } } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": "some-id",
        \\    "method": "some/method",
        \\    "params": []
        \\}
    );
    try testMessage(.{
        .request = .{
            .id = .{ .string = "foo" },
            .params = .{ .other = .{ .method = "boo", .params = null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "id": "foo",
        \\    "method": "boo"
        \\}
    );
}

test "Message.Request - random field order" {
    const msg: ExampleMessage = .{ .request = .{ .id = .{ .number = 5 }, .params = .shutdown } };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"method\": \"shutdown\" , \"params\": null         }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"params\": null         , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"id\": 5                , \"params\": null         }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"params\": null         , \"id\": 5                }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"params\": null         , \"id\": 5                , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"params\": null         , \"method\": \"shutdown\" , \"id\": 5                }");
    try testMessage(msg, "{ \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"params\": null         }");
    try testMessage(msg, "{ \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"params\": null         , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"id\": 5                , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"params\": null         }");
    try testMessage(msg, "{ \"id\": 5                , \"method\": \"shutdown\" , \"params\": null         , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"id\": 5                , \"params\": null         , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"id\": 5                , \"params\": null         , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"params\": null         }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"params\": null         , \"id\": 5                }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"params\": null         }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"id\": 5                , \"params\": null         , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"params\": null         , \"jsonrpc\": \"2.0\"     , \"id\": 5                }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"params\": null         , \"id\": 5                , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"params\": null         , \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"params\": null         , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"id\": 5                }");
    try testMessage(msg, "{ \"params\": null         , \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"params\": null         , \"id\": 5                , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"params\": null         , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"id\": 5                }");
    try testMessage(msg, "{ \"params\": null         , \"method\": \"shutdown\" , \"id\": 5                , \"jsonrpc\": \"2.0\"     }");
}

test "Message.Request - missing 'jsonrpc' field" {
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"method\": \"shutdown\" , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"params\": null         , \"method\": \"shutdown\" }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"shutdown\" , \"id\": 5                , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"shutdown\" , \"params\": null         , \"id\": 5                }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"id\": 5                , \"method\": \"shutdown\" }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"method\": \"shutdown\" , \"id\": 5                }");
}

test "Message.Request - missing 'id' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"params\": null         , \"method\": \"shutdown\" }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"shutdown\" , \"params\": null         , \"jsonrpc\": \"2.0\"     }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     }");
}

test "Message.Request - missing 'method' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"params\": null         , \"id\": 5                }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"params\": null         , \"jsonrpc\": \"2.0\"     }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"jsonrpc\": \"2.0\"     , \"id\": 5                }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"id\": 5                , \"jsonrpc\": \"2.0\"     }");
}

test "Message.Request - missing 'params' field but `ParamsType(method) == void`" {
    const msg: ExampleMessage = .{ .request = .{ .id = .{ .number = 5 }, .params = .shutdown } };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" , \"id\": 5                }");
    try testMessage(msg, "{ \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"method\": \"shutdown\" }");
    try testMessage(msg, "{ \"id\": 5                , \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"jsonrpc\": \"2.0\"     , \"id\": 5                }");
    try testMessage(msg, "{ \"method\": \"shutdown\" , \"id\": 5                , \"jsonrpc\": \"2.0\"     }");
}

test "Message.Request - 'params' field is explicit null and method is unknown" {
    const unknown_msg_null_params: ExampleMessage = .{ .request = .{ .id = .{ .number = 5 }, .params = .{ .other = .{ .method = "unknown", .params = .null } } } };
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"id\": 5               , \"method\": \"unknown\" , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"id\": 5               , \"params\": null        , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" , \"id\": 5               , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" , \"params\": null        , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"params\": null        , \"id\": 5               , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"jsonrpc\": \"2.0\"    , \"params\": null        , \"method\": \"unknown\" , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"jsonrpc\": \"2.0\"    , \"params\": null        , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"method\": \"unknown\" , \"params\": null        , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"params\": null        , \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"id\": 5               , \"params\": null        , \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    , \"id\": 5               , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    , \"params\": null        , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"id\": 5               , \"jsonrpc\": \"2.0\"    , \"params\": null        }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"id\": 5               , \"params\": null        , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"params\": null        , \"jsonrpc\": \"2.0\"    , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"method\": \"unknown\" , \"params\": null        , \"id\": 5               , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"jsonrpc\": \"2.0\"    , \"id\": 5               , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"id\": 5               , \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"id\": 5               , \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    , \"id\": 5               }");
    try testMessage(unknown_msg_null_params, "{ \"params\": null        , \"method\": \"unknown\" , \"id\": 5               , \"jsonrpc\": \"2.0\"    }");
}

test "Message.Request - missing 'params' field and method is unknown" {
    const unknown_msg_no_params: ExampleMessage = .{ .request = .{ .id = .{ .number = 5 }, .params = .{ .other = .{ .method = "unknown", .params = null } } } };
    try testMessage(unknown_msg_no_params, "{ \"jsonrpc\": \"2.0\"    , \"id\": 5               , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_no_params, "{ \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" , \"id\": 5               }");
    try testMessage(unknown_msg_no_params, "{ \"id\": 5               , \"jsonrpc\": \"2.0\"    , \"method\": \"unknown\" }");
    try testMessage(unknown_msg_no_params, "{ \"id\": 5               , \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    }");
    try testMessage(unknown_msg_no_params, "{ \"method\": \"unknown\" , \"jsonrpc\": \"2.0\"    , \"id\": 5               }");
    try testMessage(unknown_msg_no_params, "{ \"method\": \"unknown\" , \"id\": 5               , \"jsonrpc\": \"2.0\"    }");
}

test "Message.Request - missing 'params' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"id\": 5                , \"method\": \"textDocument/completion\" }");
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"     , \"method\": \"textDocument/completion\" , \"id\": 5                }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"jsonrpc\": \"2.0\"     , \"method\": \"textDocument/completion\" }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5                , \"method\": \"textDocument/completion\" , \"jsonrpc\": \"2.0\"     }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"textDocument/completion\" , \"jsonrpc\": \"2.0\"     , \"id\": 5                }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"textDocument/completion\" , \"id\": 5                , \"jsonrpc\": \"2.0\"     }");
}

test "Message.Notification" {
    try testMessage(.{
        .notification = .{
            .params = .{ .initialized = .{} },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "method": "initialized",
        \\    "params": {}
        \\}
    );
}

test "Message.Notification - null params" {
    try testMessage(.{
        .notification = .{
            .params = .exit,
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "method": "exit",
        \\    "params": null
        \\}
    );
    try testMessage(.{
        .notification = .{
            .params = .{ .other = .{ .method = "boo", .params = null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "method": "boo"
        \\}
    );
}

test "Message.Notification - unknown method" {
    try testMessage(.{
        .notification = .{
            .params = .{ .other = .{ .method = "some/method", .params = .null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "params": null,
        \\    "method": "some/method"
        \\}
    );
    try testMessage(.{
        .notification = .{
            .params = .{ .other = .{ .method = "boo", .params = null } },
        },
    },
        \\{
        \\    "jsonrpc": "2.0",
        \\    "method": "boo"
        \\}
    );
}

test "Message.Notification - random field order" {
    const msg: ExampleMessage = .{ .notification = .{ .params = .exit } };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" }");
    try testMessage(msg, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessage(msg, "{ \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessage(msg, "{ \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");

    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessage(msg, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - invalid 'params' field" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": {}       }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": {}       , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": {}       }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": {}       , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": {}       , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": {}       , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - missing 'params' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\"                    , \"method\": \"textDocument/completion\" }");
    try testMessageExpectError(error.MissingField, "{ \"method\": \"textDocument/completion\" , \"jsonrpc\": \"2.0\"                    }");
}

test "Message.Notification - missing 'jsonrpc' field" {
    try testMessageExpectError(error.MissingField, "{ \"method\": \"shutdown\" , \"params\": null         }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null         , \"method\": \"shutdown\" }");
}

test "Message.Notification - missing 'method' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.MissingField, "{ \"params\": null     , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - unexpected 'id' field" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"id\": 5            , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"id\": 5            , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"id\": 5            , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"id\": 5            , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - unexpected 'result' field" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": 5        , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": 5        , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"result\": 5        , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"result\": 5        , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"result\": 5        , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"result\": 5        , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"result\": 5        , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"result\": 5        , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"result\": 5        , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"result\": 5        , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"result\": 5        , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"result\": 5        , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - unexpected 'error' field" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"exit\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"method\": \"exit\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"exit\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - unexpected 'result' field with unknown method" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": 5        , \"method\": \"foo\"  , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": 5        , \"params\": null     , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"result\": 5        , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"params\": null     , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"result\": 5        , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"foo\"  , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"method\": \"foo\"  , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": 5        , \"params\": null     , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"result\": 5        , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"params\": null     , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"result\": 5        , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"result\": 5        , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"params\": null     , \"jsonrpc\": \"2.0\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"params\": null     , \"result\": 5        , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"result\": 5        , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"result\": 5        , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"result\": 5        , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"result\": 5        }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"foo\"  , \"result\": 5        , \"jsonrpc\": \"2.0\" }");
}

test "Message.Notification - unexpected 'error' field with unknown method" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"params\": null     , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"params\": null     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} , \"params\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"params\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"method\": \"foo\"  , \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"method\": \"foo\"  }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"foo\"  , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"params\": null     , \"method\": \"foo\"  , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - with 'result' field" {
    const msg: ExampleMessage = .{
        .response = .{
            .id = .{ .number = 5 },
            .result_or_error = .{ .result = .null },
        },
    };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"result\": null     }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"result\": null     , \"id\": 5            }");
    try testMessage(msg, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"result\": null     }");
    try testMessage(msg, "{ \"id\": 5            , \"result\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"result\": null     , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessage(msg, "{ \"result\": null     , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - with 'error' field" {
    const msg: ExampleMessage = .{
        .response = .{
            .id = .{ .number = 5 },
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(0), .message = "" } },
        },
    };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            }");
    try testMessage(msg, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - no 'id' field with 'result' field" {
    const msg: ExampleMessage = .{
        .response = .{
            .id = null,
            .result_or_error = .{ .result = .null },
        },
    };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"result\": null    }");
    try testMessage(msg, "{ \"result\": null     , \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - no 'id' field with 'error' field" {
    const msg: ExampleMessage = .{
        .response = .{
            .id = null,
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(0), .message = "" } },
        },
    };
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\"                        , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\"                        }");
}

test "Message.Response - missing 'result' field" {
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessageExpectError(error.MissingField, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.MissingField, "{ \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - 'result' is null with 'error' field" {
    const msg: ExampleMessage = .{
        .response = .{
            .id = .{ .number = 5 },
            .result_or_error = .{ .@"error" = .{ .code = @enumFromInt(0), .message = "" } },
        },
    };

    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"result\": null     , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"result\": null     }");
    try testMessage(msg, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     , \"id\": 5            }");
    try testMessage(msg, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     }");
    try testMessage(msg, "{ \"id\": 5            , \"result\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"id\": 5            , \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"result\": null     }");
    try testMessage(msg, "{ \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"result\": null     , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"result\": null     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            }");
    try testMessage(msg, "{ \"result\": null     , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessage(msg, "{ \"result\": null     , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessage(msg, "{ \"result\": null     , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"result\": null     }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"result\": null     , \"id\": 5            }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"result\": null     }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"result\": null     , \"jsonrpc\": \"2.0\" }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessage(msg, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": null     , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
}

test "Message.Response - 'result' and 'error' field" {
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": true     , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"result\": true     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"error\": {\"code\": 0, \"message\": \"\"} }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"id\": 5            , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"result\": true     , \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"id\": 5            , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"jsonrpc\": \"2.0\" , \"result\": true     , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"jsonrpc\": \"2.0\" , \"result\": true     }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"id\": 5            , \"result\": true     , \"jsonrpc\": \"2.0\" }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     , \"jsonrpc\": \"2.0\" , \"id\": 5            }");
    try testMessageExpectError(error.UnexpectedToken, "{ \"error\": {\"code\": 0, \"message\": \"\"} , \"result\": true     , \"id\": 5            , \"jsonrpc\": \"2.0\" }");
}

pub fn ResultType(comptime method: []const u8) type {
    if (types.getRequestMetadata(method)) |meta| return meta.Result;
    if (isNotificationMethod(method)) return void;
    @compileError("unknown method '" ++ method ++ "'");
}

test ResultType {
    comptime {
        std.debug.assert(ResultType("textDocument/hover") == ?types.Hover);
        std.debug.assert(ResultType("textDocument/inlayHint") == ?[]const types.InlayHint);
    }
}

pub fn ParamsType(comptime method: []const u8) type {
    if (types.getRequestMetadata(method)) |meta| return meta.Params orelse void;
    if (types.getNotificationMetadata(method)) |meta| return meta.Params orelse void;
    @compileError("unknown method '" ++ method ++ "'");
}

test ParamsType {
    comptime {
        std.debug.assert(ParamsType("textDocument/hover") == types.HoverParams);
        std.debug.assert(ParamsType("textDocument/inlayHint") == types.InlayHintParams);
    }
}

const request_method_set: parser.StaticStringMap(void) = blk: {
    var kvs_list: [types.request_metadata.len]struct { []const u8 } = undefined;
    for (types.request_metadata, &kvs_list) |meta, *kv| {
        kv.* = .{meta.method};
    }
    break :blk parser.staticStringMapInitComptime(void, kvs_list);
};

const notification_method_set: parser.StaticStringMap(void) = blk: {
    var kvs_list: [types.notification_metadata.len]struct { []const u8 } = undefined;
    for (types.notification_metadata, &kvs_list) |meta, *kv| {
        kv.* = .{meta.method};
    }
    break :blk parser.staticStringMapInitComptime(void, kvs_list);
};

/// Return whether there is a request with the given method name.
pub fn isRequestMethod(method: []const u8) bool {
    return request_method_set.has(method);
}

test isRequestMethod {
    try std.testing.expect(!isRequestMethod("initialized"));
    try std.testing.expect(!isRequestMethod("textDocument/didOpen"));

    try std.testing.expect(isRequestMethod("initialize"));
    try std.testing.expect(isRequestMethod("textDocument/completion"));

    try std.testing.expect(!isRequestMethod("foo"));
    try std.testing.expect(!isRequestMethod(""));
}

/// Return whether there is a notification with the given method name.
pub fn isNotificationMethod(method: []const u8) bool {
    return notification_method_set.has(method);
}

test isNotificationMethod {
    try std.testing.expect(isNotificationMethod("initialized"));
    try std.testing.expect(isNotificationMethod("textDocument/didOpen"));

    try std.testing.expect(!isNotificationMethod("initialize"));
    try std.testing.expect(!isNotificationMethod("textDocument/completion"));

    try std.testing.expect(!isNotificationMethod("foo"));
    try std.testing.expect(!isNotificationMethod(""));
}

comptime {
    @setEvalBranchQuota(10_000);
    std.testing.refAllDeclsRecursive(@This());
}
