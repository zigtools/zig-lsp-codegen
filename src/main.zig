const std = @import("std");
const MetaModel = @import("MetaModel.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();

    const gpa = general_purpose_allocator.allocator();

    var arg_it = try std.process.ArgIterator.initWithAllocator(gpa);

    _ = arg_it.skip(); // skip self exe

    const out_file_path = try gpa.dupe(u8, arg_it.next() orelse std.debug.panic("second argument must be the output path to the generated zig code", .{}));
    defer gpa.free(out_file_path);

    const parsed_meta_model = try std.json.parseFromSlice(MetaModel, gpa, @embedFile("meta-model"), .{});
    defer parsed_meta_model.deinit();

    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(gpa);

    @setEvalBranchQuota(100_000);
    try writeMetaModel(buffer.writer(gpa), parsed_meta_model.value);

    const source = try buffer.toOwnedSliceSentinel(gpa, 0);
    defer gpa.free(source);

    var zig_tree = try std.zig.Ast.parse(gpa, source, .zig);
    defer zig_tree.deinit(gpa);

    const output_source = if (zig_tree.errors.len != 0) blk: {
        std.log.warn("generated file contains syntax errors! (cannot format file)", .{});
        break :blk source;
    } else try zig_tree.render(gpa);
    defer if (zig_tree.errors.len == 0) gpa.free(output_source);

    std.fs.cwd().makePath(std.fs.path.dirname(out_file_path) orelse ".") catch {};

    var out_file = try std.fs.cwd().createFile(out_file_path, .{});
    defer out_file.close();

    try out_file.writeAll(output_source);
}

fn formatDocs(
    text: []const u8,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = options;
    if (fmt.len != 1) std.fmt.invalidFmtError(fmt, text);
    const prefix = switch (fmt[0]) {
        'n' => "// ",
        'd' => "/// ",
        '!' => "//! ",
        else => std.fmt.invalidFmtError(fmt, text),
    };
    var iterator = std.mem.splitScalar(u8, text, '\n');
    while (iterator.next()) |line| try writer.print("{s}{s}\n", .{ prefix, line });
}

/// The format specifier must be one of:
///  * `{n}` writes normal (`//`) comments.
///  * `{d}` writes doc-comments (`///`) comments.
///  * `{!}` writes top-level-doc-comments (`//!`) comments.
fn fmtDocs(text: []const u8) std.fmt.Formatter(formatDocs) {
    return .{ .data = text };
}

fn formatQuotedString(
    string: []const u8,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = options;
    if (fmt.len == 0) {
        try writer.print("\"{}\"", .{std.zig.fmtEscapes(string)});
    } else if (std.mem.eql(u8, fmt, "'")) {
        try writer.print("\'{'}\'", .{std.zig.fmtEscapes(string)});
    } else {
        std.fmt.invalidFmtError(fmt, string);
    }
}

/// The format specifier must be one of:
///  * `{}` writes a double-quoted string.
///  * `{''}` writes a single-quoted string.
fn fmtQuotedString(string: []const u8) std.fmt.Formatter(formatQuotedString) {
    return .{ .data = string };
}

fn messageDirectionName(message_direction: MetaModel.MessageDirection) []const u8 {
    return switch (message_direction) {
        .clientToServer => "client_to_server",
        .serverToClient => "server_to_client",
        .both => "both",
    };
}

fn guessTypeName(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type, i: usize) @TypeOf(writer).Error!void {
    switch (typ) {
        .base => |base| switch (base.name) {
            .URI => try writer.writeAll("uri"),
            .DocumentUri => try writer.writeAll("document_uri"),
            .integer => try writer.writeAll("integer"),
            .uinteger => try writer.writeAll("uinteger"),
            .decimal => try writer.writeAll("decimal"),
            .RegExp => try writer.writeAll("regexp"),
            .string => try writer.writeAll("string"),
            .boolean => try writer.writeAll("bool"),
            .null => try writer.writeAll("@\"null\""),
        },
        .reference => |ref| try writer.print("{p}", .{std.zig.fmtId(ref.name)}),
        .array => |arr| {
            try writer.writeAll("array_of_");
            try guessTypeName(meta_model, writer, arr.element.*, 0);
        },
        .map => try writer.print("map_{d}", .{i}),
        .@"and" => try writer.print("and_{d}", .{i}),
        .@"or" => try writer.print("or_{d}", .{i}),
        .tuple => try writer.print("tuple_{d}", .{i}),
        .literal,
        .stringLiteral,
        .integerLiteral,
        .booleanLiteral,
        => try writer.print("literal_{d}", .{i}),
    }
}

fn isOrActuallyEnum(ort: MetaModel.OrType) bool {
    for (ort.items) |t| {
        if (t != .stringLiteral) return false;
    }
    return true;
}

fn isTypeNull(typ: MetaModel.Type) bool {
    if (typ != .@"or") return false;
    const ort = typ.@"or";
    return (ort.items.len == 2 and ort.items[1] == .base and ort.items[1].base.name == .null) or (ort.items[ort.items.len - 1] == .base and ort.items[ort.items.len - 1].base.name == .null);
}

fn formatType(
    data: struct { meta_model: *const MetaModel, ty: MetaModel.Type },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    _ = options;
    if (fmt.len != 0) std.fmt.invalidFmtError(fmt, data.ty);
    switch (data.ty) {
        .base => |base| switch (base.name) {
            .URI => try writer.writeAll("URI"),
            .DocumentUri => try writer.writeAll("DocumentUri"),
            .integer => try writer.writeAll("i32"),
            .uinteger => try writer.writeAll("u32"),
            .decimal => try writer.writeAll("f32"),
            .RegExp => try writer.writeAll("RegExp"),
            .string => try writer.writeAll("[]const u8"),
            .boolean => try writer.writeAll("bool"),
            .null => try writer.writeAll("?void"),
        },
        .reference => |ref| try writer.print("{p}", .{std.zig.fmtId(ref.name)}),
        .array => |arr| try writer.print("[]const {}", .{fmtType(arr.element.*, data.meta_model)}),
        .map => |map| {
            try writer.writeAll("parser.Map(");
            switch (map.key) {
                .base => |base| try switch (base.name) {
                    .Uri => writer.writeAll("Uri"),
                    .DocumentUri => writer.writeAll("DocumentUri"),
                    .integer => writer.writeAll("i32"),
                    .string => writer.writeAll("[]const u8"),
                },
                .reference => |ref| try writer.print("{}", .{fmtType(.{ .reference = ref }, data.meta_model)}),
            }
            try writer.print(", {})", .{fmtType(map.value.*, data.meta_model)});
        },
        .@"and" => |andt| {
            try writer.writeAll("struct {\n");
            for (andt.items) |item| {
                if (item != .reference) @panic("Unimplemented and subject encountered!");
                try writer.print("// And {s}\n{}\n\n", .{
                    item.reference.name,
                    fmtReference(item.reference, null, data.meta_model),
                });
            }
            try writer.writeAll("}");
        },
        .@"or" => |ort| {
            // NOTE: Hack to get optionals working
            // There are no triple optional ors (I believe),
            // so this should work every time
            if (ort.items.len == 2 and ort.items[1] == .base and ort.items[1].base.name == .null) {
                try writer.print("?{}", .{fmtType(ort.items[0], data.meta_model)});
            } else if (isOrActuallyEnum(ort)) {
                try writer.writeAll("enum {");
                for (ort.items) |sub_type| {
                    try writer.print("{s},\n", .{sub_type.stringLiteral.value});
                }
                try writer.writeByte('}');
            } else {
                const has_null = ort.items[ort.items.len - 1] == .base and ort.items[ort.items.len - 1].base.name == .null;

                if (has_null) try writer.writeByte('?');

                try writer.writeAll("union(enum) {\n");
                for (ort.items[0..if (has_null) ort.items.len - 1 else ort.items.len], 0..) |sub_type, i| {
                    try guessTypeName(data.meta_model.*, writer, sub_type, i);
                    try writer.print(": {},\n", .{fmtType(sub_type, data.meta_model)});
                }
                try writer.writeAll(
                    \\pub const jsonParse = parser.UnionParser(@This()).jsonParse;
                    \\pub const jsonParseFromValue = parser.UnionParser(@This()).jsonParseFromValue;
                    \\pub const jsonStringify = parser.UnionParser(@This()).jsonStringify;
                    \\}
                );
            }
        },
        .tuple => |tup| {
            try writer.writeAll("struct {");
            for (tup.items, 0..) |ty, i| {
                if (i != 0) try writer.writeByte(',');
                try writer.print(" {}", .{fmtType(ty, data.meta_model)});
            }
            try writer.writeAll(" }");
        },
        .literal => |lit| {
            try writer.writeAll("struct {");
            if (lit.value.properties.len != 0) {
                for (lit.value.properties) |property| {
                    try writer.print("\n{}", .{fmtProperty(property, data.meta_model)});
                }
                try writer.writeByte('\n');
            }
            try writer.writeByte('}');
        },
        .stringLiteral => |lit| try writer.print("[]const u8 = \"{}\"", .{std.zig.fmtEscapes(lit.value)}),
        .integerLiteral => |lit| try writer.print("i32 = {d}", .{lit.value}),
        .booleanLiteral => |lit| try writer.print("bool = {}", .{lit.value}),
    }
}

fn fmtType(ty: MetaModel.Type, meta_model: *const MetaModel) std.fmt.Formatter(formatType) {
    return .{ .data = .{ .meta_model = meta_model, .ty = ty } };
}

fn formatProperty(
    data: struct { meta_model: *const MetaModel, property: MetaModel.Property },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len != 0) std.fmt.invalidFmtError(fmt, data.value);
    _ = options;

    const isUndefinedable = data.property.optional orelse false;
    const isNull = isTypeNull(data.property.type);
    // WORKAROUND: recursive SelectionRange
    const isSelectionRange = data.property.type == .reference and std.mem.eql(u8, data.property.type.reference.name, "SelectionRange");

    if (data.property.documentation) |docs| try writer.print("{d}", .{fmtDocs(docs)});

    try writer.print("{p}: {s}{}{s},", .{
        std.zig.fmtId(data.property.name),
        if (isSelectionRange) "?*" else if (isUndefinedable and !isNull) "?" else "",
        fmtType(data.property.type, data.meta_model),
        if (isNull or isUndefinedable) " = null" else "",
    });
}

fn fmtProperty(property: MetaModel.Property, meta_model: *const MetaModel) std.fmt.Formatter(formatProperty) {
    return .{ .data = .{ .meta_model = meta_model, .property = property } };
}

fn formatProperties(
    data: struct {
        meta_model: *const MetaModel,
        structure: MetaModel.Structure,
        maybe_extender: ?MetaModel.Structure,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len != 0) std.fmt.invalidFmtError(fmt, data.value);
    _ = options;

    const properties: []MetaModel.Property = data.structure.properties;
    const extends: []MetaModel.Type = data.structure.extends orelse &.{};
    const mixins: []MetaModel.Type = data.structure.mixins orelse &.{};

    skip: for (properties) |property| {
        if (data.maybe_extender) |ext| {
            for (ext.properties) |ext_property| {
                if (std.mem.eql(u8, property.name, ext_property.name)) {
                    // std.log.info("Skipping implemented field emission: {s}", .{property.name});
                    continue :skip;
                }
            }
        }
        try writer.print("\n{}", .{fmtProperty(property, data.meta_model)});
    }

    for (extends) |ext| {
        if (ext != .reference) @panic("Expected reference for extends!");
        try writer.print("\n\n// Extends `{s}`{}", .{
            ext.reference.name,
            fmtReference(ext.reference, data.structure, data.meta_model),
        });
    }

    for (mixins) |ext| {
        if (ext != .reference) @panic("Expected reference for mixin!");
        try writer.print("\n\n// Uses mixin `{s}`{}", .{
            ext.reference.name,
            fmtReference(ext.reference, data.structure, data.meta_model),
        });
    }
}

fn fmtProperties(structure: MetaModel.Structure, maybe_extender: ?MetaModel.Structure, meta_model: *const MetaModel) std.fmt.Formatter(formatProperties) {
    return .{ .data = .{ .meta_model = meta_model, .structure = structure, .maybe_extender = maybe_extender } };
}

fn formatReference(
    data: struct {
        meta_model: *const MetaModel,
        reference: MetaModel.ReferenceType,
        maybe_extender: ?MetaModel.Structure,
    },
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) @TypeOf(writer).Error!void {
    if (fmt.len != 0) std.fmt.invalidFmtError(fmt, data.reference);
    _ = options;

    for (data.meta_model.structures) |s| {
        if (std.mem.eql(u8, s.name, data.reference.name)) {
            try writer.print("{}", .{fmtProperties(s, data.maybe_extender, data.meta_model)});
            return;
        }
    }
}

fn fmtReference(reference: MetaModel.ReferenceType, maybe_extender: ?MetaModel.Structure, meta_model: *const MetaModel) std.fmt.Formatter(formatReference) {
    return .{ .data = .{ .meta_model = meta_model, .reference = reference, .maybe_extender = maybe_extender } };
}

fn writeRequest(writer: anytype, meta_model: MetaModel, request: MetaModel.Request) @TypeOf(writer).Error!void {
    if (request.documentation) |docs| try writer.print("{n}", .{fmtDocs(docs)});

    try writer.print(
        \\.{{
        \\  .method = "{s}",
        \\  .documentation = {?},
        \\  .direction = .{s},
        \\  .Params = {?},
        \\  .Result = {},
        \\  .PartialResult = {?},
        \\  .ErrorData = {?},
        \\  .registration = .{{ .method = {?}, .Options = {?} }},
        \\}},
        \\
    , .{
        request.method,
        if (request.documentation) |documentation| fmtQuotedString(documentation) else null,
        messageDirectionName(request.messageDirection),
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        if (request.params) |params| fmtType(params.Type, &meta_model) else null,
        fmtType(request.result, &meta_model),
        if (request.partialResult) |ty| fmtType(ty, &meta_model) else null,
        if (request.errorData) |ty| fmtType(ty, &meta_model) else null,
        if (request.registrationMethod) |method| fmtQuotedString(method) else null,
        if (request.registrationOptions) |ty| fmtType(ty, &meta_model) else null,
    });
}

fn writeNotification(writer: anytype, meta_model: MetaModel, notification: MetaModel.Notification) @TypeOf(writer).Error!void {
    if (notification.documentation) |docs| try writer.print("{n}", .{fmtDocs(docs)});

    try writer.print(
        \\.{{
        \\  .method = "{s}",
        \\  .documentation = {?},
        \\  .direction = .{s},
        \\  .Params = {?},
        \\  .registration = .{{ .method = {?}, .Options = {?} }},
        \\}},
        \\
    , .{
        notification.method,
        if (notification.documentation) |documentation| fmtQuotedString(documentation) else null,
        messageDirectionName(notification.messageDirection),
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        if (notification.params) |params| fmtType(params.Type, &meta_model) else null,
        if (notification.registrationMethod) |method| fmtQuotedString(method) else null,
        if (notification.registrationOptions) |ty| fmtType(ty, &meta_model) else null,
    });
}

fn writeStructure(writer: anytype, meta_model: MetaModel, structure: MetaModel.Structure) @TypeOf(writer).Error!void {
    if (std.mem.eql(u8, structure.name, "LSPObject")) return;

    if (structure.documentation) |docs| try writer.print("{d}", .{fmtDocs(docs)});
    try writer.print("pub const {p} = struct {{{}\n}};\n\n", .{
        std.zig.fmtId(structure.name),
        fmtProperties(structure, null, &meta_model),
    });
}

fn writeEnumeration(writer: anytype, meta_model: MetaModel, enumeration: MetaModel.Enumeration) @TypeOf(writer).Error!void {
    _ = meta_model;

    if (enumeration.documentation) |docs| try writer.print("{d}", .{fmtDocs(docs)});

    const container_kind = switch (enumeration.type.name) {
        .string => "union(enum)",
        .integer => "enum(i32)",
        .uinteger => "enum(u32)",
    };
    try writer.print("pub const {p} = {s} {{\n", .{ std.zig.fmtId(enumeration.name), container_kind });

    // WORKAROUND: the enumeration value `pascal` appears twice in LanguageKind
    var found_pascal = false;

    var contains_empty_enum = false;
    for (enumeration.values) |entry| {
        if (entry.documentation) |docs| try writer.print("{d}", .{fmtDocs(docs)});
        switch (entry.value) {
            .string => |value| {
                if (std.mem.eql(u8, value, "pascal")) {
                    if (found_pascal) continue;
                    found_pascal = true;
                }
                if (value.len == 0) contains_empty_enum = true;
                const name = if (value.len == 0) "empty" else value;
                try writer.print("{p},\n", .{std.zig.fmtId(name)});
            },
            .number => |value| try writer.print("{p} = {d},\n", .{ std.zig.fmtId(entry.name), value }),
        }
    }

    const supportsCustomValues = enumeration.supportsCustomValues orelse false;

    const field_name, const docs = if (supportsCustomValues) .{ "custom_value", "Custom Value" } else .{ "unknown_value", "Unknown Value" };
    switch (enumeration.type.name) {
        .string => {
            try writer.print(
                \\{s}: []const u8,
                \\pub const eql = parser.EnumCustomStringValues(@This(), {1}).eql;
                \\pub const jsonParse = parser.EnumCustomStringValues(@This(), {1}).jsonParse;
                \\pub const jsonParseFromValue = parser.EnumCustomStringValues(@This(), {1}).jsonParseFromValue;
                \\pub const jsonStringify = parser.EnumCustomStringValues(@This(), {1}).jsonStringify;
                \\
            , .{ field_name, contains_empty_enum });
        },
        .integer, .uinteger => {
            try writer.print(
                \\/// {s}
                \\_,
                \\pub const jsonStringify = parser.EnumStringifyAsInt(@This()).jsonStringify;
                \\
            , .{docs});
        },
    }

    try writer.writeAll("};\n\n");
}

fn writeTypeAlias(writer: anytype, meta_model: MetaModel, type_alias: MetaModel.TypeAlias) @TypeOf(writer).Error!void {
    if (std.mem.startsWith(u8, type_alias.name, "LSP")) return;

    if (type_alias.documentation) |docs| try writer.print("{d}", .{fmtDocs(docs)});
    try writer.print("pub const {p} = {};\n\n", .{ std.zig.fmtId(type_alias.name), fmtType(type_alias.type, &meta_model) });
}

fn writeMetaModel(writer: anytype, meta_model: MetaModel) !void {
    try writer.writeAll(@embedFile("lsp_types_base.zig") ++ "\n");

    try writer.writeAll("// Type Aliases\n\n");
    for (meta_model.typeAliases) |type_alias| {
        try writeTypeAlias(writer, meta_model, type_alias);
    }

    try writer.writeAll("// Enumerations\n\n");
    for (meta_model.enumerations) |enumeration| {
        try writeEnumeration(writer, meta_model, enumeration);
    }

    try writer.writeAll("// Structures\n\n");
    for (meta_model.structures) |structure| {
        try writeStructure(writer, meta_model, structure);
    }

    try writer.writeAll("const notification_metadata_generated = [_]NotificationMetadata{\n");
    for (meta_model.notifications) |notification| {
        try writeNotification(writer, meta_model, notification);
    }
    try writer.writeAll("\n};");

    try writer.writeAll("const request_metadata_generated = [_]RequestMetadata{\n");
    for (meta_model.requests) |request| {
        try writeRequest(writer, meta_model, request);
    }
    try writer.writeAll("};\n");
}
