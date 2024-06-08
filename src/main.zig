const std = @import("std");
const MetaModel = @import("MetaModel.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();

    const gpa = general_purpose_allocator.allocator();

    var arg_it = try std.process.ArgIterator.initWithAllocator(gpa);

    _ = arg_it.skip(); // skip self exe

    const model_file = try std.fs.cwd().openFile(
        arg_it.next() orelse std.debug.panic("first argument must be the path to the metaModel.json", .{}),
        .{},
    );
    defer model_file.close();

    const out_file_path = try gpa.dupe(u8, arg_it.next() orelse std.debug.panic("second argument must be the output path to the generated zig code", .{}));
    defer gpa.free(out_file_path);

    var json_reader = std.json.reader(gpa, model_file.reader());
    defer json_reader.deinit();

    const parsed_meta_model = try std.json.parseFromTokenSource(MetaModel, gpa, &json_reader, .{});
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

fn writeDocs(writer: anytype, docs: []const u8) @TypeOf(writer).Error!void {
    var iterator = std.mem.splitScalar(u8, docs, '\n');
    while (iterator.next()) |line| try writer.print("/// {s}\n", .{line});
}

fn writeDocsAsNormal(writer: anytype, docs: []const u8) @TypeOf(writer).Error!void {
    var iterator = std.mem.splitScalar(u8, docs, '\n');
    while (iterator.next()) |line| try writer.print("// {s}\n", .{line});
}

fn writeMessageDirection(writer: anytype, message_direction: MetaModel.MessageDirection) @TypeOf(writer).Error!void {
    try writer.print(".{s}", .{switch (message_direction) {
        .clientToServer => "client_to_server",
        .serverToClient => "server_to_client",
        .both => "both",
    }});
}

fn guessTypeName(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type, i: usize) @TypeOf(writer).Error!void {
    switch (typ) {
        .base => |base| try switch (base.name) {
            .URI => writer.writeAll("uri"),
            .DocumentUri => writer.writeAll("document_uri"),
            .integer => writer.writeAll("integer"),
            .uinteger, .decimal => writer.writeAll("uinteger"),
            .RegExp => writer.writeAll("regexp"),
            .string => writer.writeAll("string"),
            .boolean => writer.writeAll("bool"),
            .null => writer.writeAll("@\"null\""),
        },
        .reference => |ref| try writer.print("{}", .{std.zig.fmtId(ref.name)}),
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

fn writeType(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type) @TypeOf(writer).Error!void {
    switch (typ) {
        .base => |base| try switch (base.name) {
            .URI => writer.writeAll("URI"),
            .DocumentUri => writer.writeAll("DocumentUri"),
            .integer => writer.writeAll("i32"),
            .uinteger => writer.writeAll("u32"),
            .decimal => writer.writeAll("f32"),
            .RegExp => writer.writeAll("RegExp"),
            .string => writer.writeAll("[]const u8"),
            .boolean => writer.writeAll("bool"),
            .null => writer.writeAll("?void"),
        },
        .reference => |ref| try writer.print("{}", .{std.zig.fmtId(ref.name)}),
        .array => |arr| {
            try writer.writeAll("[]const ");
            try writeType(meta_model, writer, arr.element.*);
        },
        .map => |map| {
            try writer.writeAll("Map(");
            switch (map.key) {
                .base => |base| try switch (base.name) {
                    .Uri => writer.writeAll("Uri"),
                    .DocumentUri => writer.writeAll("DocumentUri"),
                    .integer => writer.writeAll("i32"),
                    .string => writer.writeAll("[]const u8"),
                },
                .reference => |ref| try writeType(meta_model, writer, .{ .reference = ref }),
            }
            try writer.writeAll(", ");
            try writeType(meta_model, writer, map.value.*);
            try writer.writeByte(')');
        },
        .@"and" => |andt| {
            try writer.writeAll("struct {\n");
            for (andt.items) |item| {
                switch (item) {
                    .reference => |ref| {
                        try writer.print("// And {s}\n", .{ref.name});

                        for (meta_model.structures) |s| {
                            if (std.mem.eql(u8, s.name, ref.name)) {
                                try writeProperties(meta_model, writer, s, null);
                                break;
                            }
                        }

                        try writer.writeAll("\n\n");
                    },
                    else => @panic("Unimplemented and subject encountered!"),
                }
            }
            try writer.writeAll("}");
        },
        .@"or" => |ort| {
            // NOTE: Hack to get optionals working
            // There are no triple optional ors (I believe),
            // so this should work every time
            if (ort.items.len == 2 and ort.items[1] == .base and ort.items[1].base.name == .null) {
                try writer.writeByte('?');
                try writeType(meta_model, writer, ort.items[0]);
            } else if (isOrActuallyEnum(ort)) {
                try writer.writeAll("enum {");
                for (ort.items) |sub_type| {
                    try writer.print("{s},\n", .{sub_type.stringLiteral.value});
                }
                try writer.writeByte('}');
            } else {
                const has_null = ort.items[ort.items.len - 1] == .base and ort.items[ort.items.len - 1].base.name == .null;

                if (has_null) try writer.writeByte('?');

                try writer.writeAll("union(enum) {");
                for (ort.items[0..if (has_null) ort.items.len - 1 else ort.items.len], 0..) |sub_type, i| {
                    try guessTypeName(meta_model, writer, sub_type, i);
                    try writer.writeAll(": ");
                    try writeType(meta_model, writer, sub_type);
                    try writer.writeAll(",\n");
                }
                try writer.writeAll("pub usingnamespace UnionParser(@This());");
                try writer.writeByte('}');
            }
        },
        .tuple => |tup| {
            try writer.writeAll("struct{");
            for (tup.items, 0..) |t, i| {
                try writeType(meta_model, writer, t);
                if (tup.items.len - 1 != i) try writer.writeAll(", ");
            }
            try writer.writeAll("}");
        },
        .literal => |lit| {
            try writer.writeAll("struct {\n");
            for (lit.value.properties) |property| try writeProperty(meta_model, writer, property);
            try writer.writeAll("\n}");
        },
        .stringLiteral => |lit| {
            try writer.print("[]const u8 = \"{s}\"", .{lit.value});
        },
        .integerLiteral => |lit| {
            try writer.print("i32 = {d}", .{lit.value});
        },
        .booleanLiteral => |lit| {
            try writer.print("bool = {}", .{lit.value});
        },
    }
}

fn writeProperty(meta_model: MetaModel, writer: anytype, property: MetaModel.Property) @TypeOf(writer).Error!void {
    const isUndefinedable = property.optional orelse false;
    const isNull = isTypeNull(property.type);

    if (property.documentation) |docs| try writeDocs(writer, docs);
    // switch (property.type) {
    //     .stringLiteral,
    //     .integerLiteral,
    //     .booleanLiteral,
    //     => try writer.writeAll("comptime "),
    //     else => {},
    // }

    try writer.print("{}: ", .{std.zig.fmtId(property.name)});
    if (isUndefinedable and !isNull) try writer.writeAll("?");
    // WORKAROUND: recursive SelectionRange
    if (property.type == .reference and std.mem.eql(u8, property.type.reference.name, "SelectionRange")) {
        try writer.writeByte('*');
    }
    try writeType(meta_model, writer, property.type);
    if (isNull or isUndefinedable)
        try writer.writeAll("= null");
    try writer.writeAll(",\n");
}

fn writeProperties(
    meta_model: MetaModel,
    writer: anytype,
    structure: MetaModel.Structure,
    maybe_extender: ?MetaModel.Structure,
) @TypeOf(writer).Error!void {
    z: for (structure.properties) |property| {
        if (maybe_extender) |ext| {
            for (ext.properties) |ext_property| {
                if (std.mem.eql(u8, property.name, ext_property.name)) {
                    std.log.info("Skipping implemented field emission: {s}", .{property.name});
                    continue :z;
                }
            }
        }
        try writeProperty(meta_model, writer, property);
    }

    if (structure.extends) |extends| {
        for (extends) |ext| {
            switch (ext) {
                .reference => |ref| {
                    try writer.print("\n\n// Extends {s}\n", .{ref.name});

                    for (meta_model.structures) |s| {
                        if (std.mem.eql(u8, s.name, ref.name)) {
                            try writeProperties(meta_model, writer, s, structure);
                            break;
                        }
                    }

                    try writer.writeAll("\n\n");
                },
                else => @panic("Expected reference for extends!"),
            }
        }
    }

    if (structure.mixins) |mixes| {
        for (mixes) |ext| {
            switch (ext) {
                .reference => |ref| {
                    try writer.print("\n\n// Uses mixin {s}\n", .{ref.name});

                    for (meta_model.structures) |s| {
                        if (std.mem.eql(u8, s.name, ref.name)) {
                            try writeProperties(meta_model, writer, s, structure);
                            break;
                        }
                    }

                    try writer.writeAll("\n\n");
                },
                else => @panic("Expected reference for mixin!"),
            }
        }
    }
}

fn writeRequest(writer: anytype, meta_model: MetaModel, request: MetaModel.Request) @TypeOf(writer).Error!void {
    if (request.documentation) |docs| try writeDocsAsNormal(writer, docs);
    try writer.print(".{{.method = \"{s}\", .documentation =", .{request.method});
    if (request.documentation) |value|
        try writer.print("\"{}\"", .{std.zig.fmtEscapes(value)})
    else
        try writer.writeAll("null");

    try writer.writeAll(", .direction = ");
    try writeMessageDirection(writer, request.messageDirection);

    try writer.writeAll(", .Params =");
    if (request.params) |params|
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        try writeType(meta_model, writer, params.Type)
    else
        try writer.writeAll("null");

    try writer.writeAll(", .Result =");
    try writeType(meta_model, writer, request.result);

    try writer.writeAll(", .PartialResult =");
    if (request.partialResult) |pr|
        try writeType(meta_model, writer, pr)
    else
        try writer.writeAll("null");

    try writer.writeAll(", .ErrorData =");
    if (request.errorData) |erd|
        try writeType(meta_model, writer, erd)
    else
        try writer.writeAll("null");

    try writer.writeAll(", .registration = .{.method = ");
    if (request.registrationMethod) |method|
        try writer.print("\"{}\"", .{std.zig.fmtEscapes(method)})
    else
        try writer.writeAll("null");
    try writer.writeAll(", .Options =");
    if (request.registrationOptions) |options|
        try writeType(meta_model, writer, options)
    else
        try writer.writeAll("null");
    try writer.writeAll("},},\n");
}

fn writeNotification(writer: anytype, meta_model: MetaModel, notification: MetaModel.Notification) @TypeOf(writer).Error!void {
    if (notification.documentation) |docs| try writeDocsAsNormal(writer, docs);
    try writer.print(".{{.method = \"{s}\", .documentation =", .{notification.method});
    if (notification.documentation) |value|
        try writer.print("\"{}\"", .{std.zig.fmtEscapes(value)})
    else
        try writer.writeAll("null");

    try writer.writeAll(", .direction = ");
    try writeMessageDirection(writer, notification.messageDirection);

    try writer.writeAll(", .Params =");
    if (notification.params) |params|
        // NOTE: Multiparams not used here, so we dont have to implement them :)
        try writeType(meta_model, writer, params.Type)
    else
        try writer.writeAll("null");
    try writer.writeAll(", .registration = .{.method = ");
    if (notification.registrationMethod) |method|
        try writer.print("\"{}\"", .{std.zig.fmtEscapes(method)})
    else
        try writer.writeAll("null");
    try writer.writeAll(", .Options =");
    if (notification.registrationOptions) |options|
        try writeType(meta_model, writer, options)
    else
        try writer.writeAll("null");
    try writer.writeAll("},},\n");
}

fn writeStructure(writer: anytype, meta_model: MetaModel, structure: MetaModel.Structure) @TypeOf(writer).Error!void {
    if (std.mem.eql(u8, structure.name, "LSPObject")) return;

    if (structure.documentation) |docs| try writeDocs(writer, docs);
    try writer.print("pub const {} = struct {{\n", .{std.zig.fmtId(structure.name)});

    try writeProperties(meta_model, writer, structure, null);

    try writer.writeAll("};\n\n");
}

fn writeEnumeration(writer: anytype, meta_model: MetaModel, enumeration: MetaModel.Enumeration) @TypeOf(writer).Error!void {
    _ = meta_model;
    const supportsCustomValues = enumeration.supportsCustomValues orelse false;
    const supportsCustomStringValues = supportsCustomValues and enumeration.type.name == .string;

    if (enumeration.documentation) |docs| try writeDocs(writer, docs);
    switch (enumeration.type.name) {
        .string => try writer.print("pub const {} = {s} {{\n", .{
            std.zig.fmtId(enumeration.name),
            if (supportsCustomStringValues) "union(enum)" else "enum",
        }),
        .integer => try writer.print("pub const {} = enum(i32) {{\n", .{std.zig.fmtId(enumeration.name)}),
        .uinteger => try writer.print("pub const {} = enum(u32) {{\n", .{std.zig.fmtId(enumeration.name)}),
    }

    // WORKAROUND: the enumeration value `pascal` appears twice in LanguageKind
    var found_pascal = false;

    var contains_empty_enum = false;
    for (enumeration.values) |entry| {
        if (entry.documentation) |docs| try writeDocs(writer, docs);
        switch (entry.value) {
            .string => |value| {
                if (std.mem.eql(u8, value, "pascal")) {
                    if (found_pascal) continue;
                    found_pascal = true;
                }
                if (value.len == 0) contains_empty_enum = true;
                const name = if (value.len == 0) "empty" else value;
                try writer.print("{},\n", .{std.zig.fmtId(name)});
            },
            .number => |value| try writer.print("{} = {d},\n", .{ std.zig.fmtId(entry.name), value }),
        }
    }

    if (supportsCustomValues) {
        switch (enumeration.type.name) {
            .string => try writer.print("custom_value: []const u8,", .{}),
            .integer, .uinteger => try writer.print("_,", .{}),
        }
    } else if (enumeration.values.len == 1) {
        try writer.writeAll("placeholder__, // fixes alignment issue\n");
    }

    if (supportsCustomStringValues) {
        try writer.print("pub usingnamespace EnumCustomStringValues(@This(), {s});\n", .{if (contains_empty_enum) "true" else "false"});
    } else if (enumeration.type.name != .string) {
        try writer.writeAll("pub usingnamespace EnumStringifyAsInt(@This());\n");
    }

    try writer.writeAll("};\n\n");
}

fn writeTypeAlias(writer: anytype, meta_model: MetaModel, type_alias: MetaModel.TypeAlias) @TypeOf(writer).Error!void {
    if (std.mem.startsWith(u8, type_alias.name, "LSP")) return;

    if (type_alias.documentation) |docs| try writeDocs(writer, docs);
    try writer.print("pub const {} = ", .{std.zig.fmtId(type_alias.name)});
    try writeType(meta_model, writer, type_alias.type);
    try writer.writeAll(";\n\n");
}

fn writeMetaModel(writer: anytype, meta_model: MetaModel) !void {
    try writer.writeAll(@embedFile("base.zig") ++ "\n");

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

    try writer.writeAll("pub const notification_metadata = [_]NotificationMetadata{\n");
    for (meta_model.notifications) |notification| {
        try writeNotification(writer, meta_model, notification);
    }
    try writer.writeAll("\n};");

    try writer.writeAll("pub const request_metadata = [_]RequestMetadata{\n");
    for (meta_model.requests) |request| {
        try writeRequest(writer, meta_model, request);
    }
    try writer.writeAll("\n};");
}
