const std = @import("std");
const tres = @import("tres");
const MetaModel = @import("MetaModel.zig");

pub fn main() !void {
    @setEvalBranchQuota(100_000);

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = general_purpose_allocator.deinit();

    var gpa = general_purpose_allocator.allocator();

    var input_model_file = try std.fs.cwd().openFile("metaModel.json", .{});

    var model_file_source = try input_model_file.readToEndAlloc(gpa, std.math.maxInt(usize));
    defer gpa.free(model_file_source);

    var parser = std.json.Parser.init(gpa, true);
    defer parser.deinit();

    var tree = try parser.parse(model_file_source);
    defer tree.deinit();

    var meta_model = try tres.parse(MetaModel, tree.root, tree.arena.allocator());

    var buffer = std.ArrayListUnmanaged(u8){};
    defer buffer.deinit(gpa);

    try writeMetaModel(buffer.writer(gpa), meta_model);

    var source = try buffer.toOwnedSliceSentinel(gpa, 0);
    defer gpa.free(source);

    var zig_tree = try std.zig.parse(gpa, source);
    defer zig_tree.deinit(gpa);

    const output_source = if (zig_tree.errors.len != 0) blk: {
        std.log.warn("generated file contains syntax errors! (cannot format file)", .{});
        break :blk source;
    } else try zig_tree.render(gpa);
    defer if (zig_tree.errors.len == 0) gpa.free(output_source);

    var out_file = try std.fs.cwd().createFile("lsp.zig", .{});
    defer out_file.close();

    try out_file.writeAll(output_source);
}

fn writeDocs(writer: anytype, docs: []const u8) @TypeOf(writer).Error!void {
    var iterator = std.mem.split(u8, docs, "\n");
    while (iterator.next()) |line| try writer.print("/// {s}\n", .{line});
}

fn writeDocsAsNormal(writer: anytype, docs: []const u8) @TypeOf(writer).Error!void {
    var iterator = std.mem.split(u8, docs, "\n");
    while (iterator.next()) |line| try writer.print("// {s}\n", .{line});
}

fn guessTypeName(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type, i: usize) @TypeOf(writer).Error!void {
    switch (typ) {
        .BaseType => |base| try switch (base.name) {
            .URI => writer.writeAll("uri"),
            .DocumentUri => writer.writeAll("document_uri"),
            .integer => writer.writeAll("integer"),
            .uinteger, .decimal => writer.writeAll("uinteger"),
            .RegExp => writer.writeAll("regexp"),
            .string => writer.writeAll("string"),
            .boolean => writer.writeAll("bool"),
            .null => writer.writeAll("@\"null\""),
        },
        .ReferenceType => |ref| try writer.print("{s}", .{std.zig.fmtId(ref.name)}),
        .ArrayType => |arr| {
            try writer.writeAll("array_of_");
            try guessTypeName(meta_model, writer, arr.element.*, 0);
        },
        .MapType => try writer.print("map_{d}", .{i}),
        .OrType => try writer.print("or_{d}", .{i}),
        .TupleType => try writer.print("tuple_{d}", .{i}),
        .StructureLiteralType,
        .StringLiteralType,
        .IntegerLiteralType,
        .BooleanLiteralType,
        => try writer.print("literal_{d}", .{i}),
        else => @panic("Impossible: Unhandled name guess!"),
    }
}

fn isOrActuallyEnum(ort: MetaModel.OrType) bool {
    for (ort.items) |t| {
        if (t != .StringLiteralType) return false;
    }
    return true;
}

fn isTypeNull(typ: MetaModel.Type) bool {
    if (typ != .OrType) return false;
    var ort = typ.OrType;
    return (ort.items.len == 2 and ort.items[1] == .BaseType and ort.items[1].BaseType.name == .null) or (ort.items[ort.items.len - 1] == .BaseType and ort.items[ort.items.len - 1].BaseType.name == .null);
}

fn writeType(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type) anyerror!void {
    switch (typ) {
        .BaseType => |base| try switch (base.name) {
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
        .ReferenceType => |ref| try writer.print("{s}", .{std.zig.fmtId(ref.name)}),
        .ArrayType => |arr| {
            try writer.writeAll("[]const ");
            try writeType(meta_model, writer, arr.element.*);
        },
        .MapType => |map| {
            try writer.writeAll("Map(");
            switch (map.key) {
                .base => |base| try switch (base.name) {
                    .Uri => writer.writeAll("Uri"),
                    .DocumentUri => writer.writeAll("DocumentUri"),
                    .integer => writer.writeAll("i32"),
                    .string => writer.writeAll("[]const u8"),
                },
                .ReferenceType => |ref| try writeType(meta_model, writer, .{ .ReferenceType = ref }),
            }
            try writer.writeAll(", ");
            try writeType(meta_model, writer, map.value.*);
            try writer.writeByte(')');
        },
        .AndType => |andt| {
            try writer.writeAll("struct {\n");
            for (andt.items) |item| {
                switch (item) {
                    .ReferenceType => |ref| {
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
        .OrType => |ort| {
            // NOTE: Hack to get optionals working
            // There are no triple optional ors (I believe),
            // so this should work every time
            if (ort.items.len == 2 and ort.items[1] == .BaseType and ort.items[1].BaseType.name == .null) {
                try writer.writeByte('?');
                try writeType(meta_model, writer, ort.items[0]);
            } else if (isOrActuallyEnum(ort)) {
                try writer.writeAll("enum {");
                try writer.writeAll("pub const tres_string_enum = {};\n");
                for (ort.items) |sub_type| {
                    try writer.print("{s},\n", .{sub_type.StringLiteralType.value});
                }
                try writer.writeByte('}');
            } else {
                var has_null = ort.items[ort.items.len - 1] == .BaseType and ort.items[ort.items.len - 1].BaseType.name == .null;

                if (has_null) try writer.writeByte('?');

                try writer.writeAll("union(enum) {");
                for (ort.items[0..if (has_null) ort.items.len - 1 else ort.items.len]) |sub_type, i| {
                    try guessTypeName(meta_model, writer, sub_type, i);
                    try writer.writeAll(": ");
                    try writeType(meta_model, writer, sub_type);
                    try writer.writeAll(",\n");
                }
                try writer.writeByte('}');
            }
        },
        .TupleType => |tup| {
            try writer.writeAll("struct{");
            for (tup.items) |t, i| {
                try writeType(meta_model, writer, t);
                if (tup.items.len - 1 != i) try writer.writeAll(", ");
            }
            try writer.writeAll("}");
        },
        .StructureLiteralType => |lit| {
            try writer.writeAll("struct {\n");
            try writer.writeAll("pub const tres_null_meaning = .{");
            for (lit.value.properties) |property| try writeNullMeaning(meta_model, writer, property);
            try writer.writeAll("};\n\n");
            for (lit.value.properties) |property| try writeProperty(meta_model, writer, property);
            try writer.writeAll("\n}");
        },
        .StringLiteralType => |lit| {
            try writer.print("[]const u8 = \"{s}\"", .{lit.value});
        },
        .IntegerLiteralType => |lit| {
            try writer.print("i32 = {d}", .{lit.value});
        },
        .BooleanLiteralType => |lit| {
            try writer.print("bool = {}", .{lit.value});
        },
    }
}

fn writeProperty(meta_model: MetaModel, writer: anytype, property: MetaModel.Property) anyerror!void {
    var isUndefinedable = property.optional.asOptional() orelse false;

    if (property.documentation.asOptional()) |docs| try writeDocs(writer, docs);
    switch (property.type) {
        .StringLiteralType,
        .IntegerLiteralType,
        .BooleanLiteralType,
        => try writer.writeAll("comptime "),
        else => {},
    }

    try writer.print("{s}: ", .{std.zig.fmtId(property.name)});
    if (isUndefinedable) try writer.writeAll("?");
    try writeType(meta_model, writer, property.type);
    if (isTypeNull(property.type) or isUndefinedable)
        try writer.writeAll("= null");
    try writer.writeAll(",\n");
}

fn writeNullMeaning(_: MetaModel, writer: anytype, property: MetaModel.Property) anyerror!void {
    const isUndefinedable = property.optional.asOptional() orelse false;
    const isNullable = isTypeNull(property.type);

    if (!isUndefinedable and !isNullable) return;

    try writer.print(".{s} = ", .{std.zig.fmtId(property.name)});

    if (isUndefinedable and isNullable)
        try writer.writeAll(".dual")
    else if (isUndefinedable and !isNullable)
        try writer.writeAll(".field")
    else if (!isUndefinedable and isNullable)
        try writer.writeAll(".value");

    try writer.writeAll(",\n");
}

fn writeNullMeanings(
    meta_model: MetaModel,
    writer: anytype,
    structure: MetaModel.Structure,
    maybe_extender: ?MetaModel.Structure,
) anyerror!void {
    z: for (structure.properties) |property| {
        if (maybe_extender) |ext| {
            for (ext.properties) |ext_property| {
                if (std.mem.eql(u8, property.name, ext_property.name)) {
                    std.log.info("Skipping implemented field emission: {s}", .{property.name});
                    continue :z;
                }
            }
        }
        try writeNullMeaning(meta_model, writer, property);
    }

    if (structure.extends.asOptional()) |extends| {
        for (extends) |ext| {
            switch (ext) {
                .ReferenceType => |ref| {
                    try writer.print("\n\n// Extends {s}\n", .{ref.name});

                    for (meta_model.structures) |s| {
                        if (std.mem.eql(u8, s.name, ref.name)) {
                            try writeNullMeanings(meta_model, writer, s, structure);
                            break;
                        }
                    }

                    try writer.writeAll("\n\n");
                },
                else => @panic("Expected reference for extends!"),
            }
        }
    }

    if (structure.mixins.asOptional()) |mixes| {
        for (mixes) |ext| {
            switch (ext) {
                .ReferenceType => |ref| {
                    try writer.print("\n\n// Uses mixin {s}\n", .{ref.name});

                    for (meta_model.structures) |s| {
                        if (std.mem.eql(u8, s.name, ref.name)) {
                            try writeNullMeanings(meta_model, writer, s, structure);
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

fn writeProperties(
    meta_model: MetaModel,
    writer: anytype,
    structure: MetaModel.Structure,
    maybe_extender: ?MetaModel.Structure,
) anyerror!void {
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

    if (structure.extends.asOptional()) |extends| {
        for (extends) |ext| {
            switch (ext) {
                .ReferenceType => |ref| {
                    try writer.print("// Extends {s}\n", .{ref.name});

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

    if (structure.mixins.asOptional()) |mixes| {
        for (mixes) |ext| {
            switch (ext) {
                .ReferenceType => |ref| {
                    try writer.print("// Uses mixin {s}\n", .{ref.name});

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

pub fn writeMetaModel(writer: anytype, meta_model: MetaModel) !void {
    try writer.writeAll(@embedFile("base.zig") ++ "\n");

    try writer.writeAll("// Type Aliases\n\n");
    for (meta_model.typeAliases) |alias| {
        if (std.mem.startsWith(u8, alias.name, "LSP")) continue;

        if (alias.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("pub const {s} = ", .{std.zig.fmtId(alias.name)});
        try writeType(meta_model, writer, alias.type);
        try writer.writeAll(";\n\n");
    }

    try writer.writeAll("// Enumerations\n\n");
    for (meta_model.enumerations) |enumeration| {
        if (enumeration.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        switch (enumeration.type.name) {
            .string => try writer.print("pub const {s} = enum {{pub const tres_string_enum = {{}};\n\n", .{std.zig.fmtId(enumeration.name)}),
            .integer => try writer.print("pub const {s} = enum(i32) {{\n", .{std.zig.fmtId(enumeration.name)}),
            .uinteger => try writer.print("pub const {s} = enum(u32) {{\n", .{std.zig.fmtId(enumeration.name)}),
        }

        var contains_empty_enum = false;
        for (enumeration.values) |entry| {
            if (entry.documentation.asOptional()) |docs| try writeDocs(writer, docs);
            switch (entry.value) {
                .string => |value| {
                    if (value.len == 0) contains_empty_enum = true;
                    const name = if (value.len == 0) "empty" else value;
                    try writer.print("{s},\n", .{std.zig.fmtId(name)});
                },
                .number => |value| try writer.print("{s} = {d},\n", .{ std.zig.fmtId(entry.name), value }),
            }
        }

        if (enumeration.values.len == 1) {
            try writer.writeAll("placeholder__, // fixes alignment issue\n");
        }

        if (contains_empty_enum) {
            try writer.writeAll(
                \\
                \\pub fn tresParse(json_value: std.json.Value, maybe_allocator: ?std.mem.Allocator) error{InvalidEnumTag}!@This() {
                \\    _ = maybe_allocator;
                \\    if (json_value != .String) return error.InvalidEnumTag;
                \\    if (json_value.String.len == 0) return .empty;
                \\    return std.meta.stringToEnum(@This(), json_value.String) orelse return error.InvalidEnumTag;
                \\}
                \\
            );
        }

        try writer.writeAll("};\n\n");
    }

    try writer.writeAll("// Structures\n\n");
    for (meta_model.structures) |structure| {
        if (std.mem.eql(u8, structure.name, "LSPObject")) continue;

        if (structure.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("pub const {s} = struct {{\n", .{std.zig.fmtId(structure.name)});

        try writer.writeAll("pub const tres_null_meaning = .{");
        try writeNullMeanings(meta_model, writer, structure, null);
        try writer.writeAll("};\n\n");
        try writeProperties(meta_model, writer, structure, null);

        try writer.writeAll("};\n\n");
    }

    try writer.writeAll("pub const notification_metadata = [_]NotificationMetadata{\n");
    for (meta_model.notifications) |notification| {
        if (notification.documentation.asOptional()) |docs| try writeDocsAsNormal(writer, docs);
        try writer.print(".{{.method = \"{s}\", .documentation =", .{notification.method});
        if (notification.documentation.asOptional()) |value|
            try writer.print("\"{}\"", .{std.zig.fmtEscapes(value)})
        else
            try writer.writeAll("null");
        try writer.print(", .direction = .{s}, .Params =", .{notification.messageDirection});

        if (notification.params.asOptional()) |params|
            // NOTE: Multiparams not used here, so we dont have to implement them :)
            try writeType(meta_model, writer, params.Type)
        else
            try writer.writeAll("null");
        try writer.writeAll(", .registration = .{.method = ");
        if (notification.registrationMethod.asOptional()) |method|
            try writer.print("\"{}\"", .{std.zig.fmtEscapes(method)})
        else
            try writer.writeAll("null");
        try writer.writeAll(", .Options =");
        if (notification.registrationOptions.asOptional()) |options|
            try writeType(meta_model, writer, options)
        else
            try writer.writeAll("null");
        try writer.writeAll("},},\n");
    }
    try writer.writeAll("\n};");

    // REQUESTS

    try writer.writeAll("pub const request_metadata = [_]RequestMetadata{\n");
    for (meta_model.requests) |request| {
        if (request.documentation.asOptional()) |docs| try writeDocsAsNormal(writer, docs);
        try writer.print(".{{.method = \"{s}\", .documentation =", .{request.method});
        if (request.documentation.asOptional()) |value|
            try writer.print("\"{}\"", .{std.zig.fmtEscapes(value)})
        else
            try writer.writeAll("null");
        try writer.print(", .direction = .{s}, .Params =", .{request.messageDirection});

        if (request.params.asOptional()) |params|
            // NOTE: Multiparams not used here, so we dont have to implement them :)
            try writeType(meta_model, writer, params.Type)
        else
            try writer.writeAll("null");

        try writer.writeAll(", .Result =");
        try writeType(meta_model, writer, request.result);

        try writer.writeAll(", .PartialResult =");
        if (request.partialResult.asOptional()) |pr|
            // NOTE: Multiparams not used here, so we dont have to implement them :)
            try writeType(meta_model, writer, pr)
        else
            try writer.writeAll("null");

        try writer.writeAll(", .ErrorData =");
        if (request.errorData.asOptional()) |erd|
            // NOTE: Multiparams not used here, so we dont have to implement them :)
            try writeType(meta_model, writer, erd)
        else
            try writer.writeAll("null");

        try writer.writeAll(", .registration = .{.method = ");
        if (request.registrationMethod.asOptional()) |method|
            try writer.print("\"{}\"", .{std.zig.fmtEscapes(method)})
        else
            try writer.writeAll("null");
        try writer.writeAll(", .Options =");
        if (request.registrationOptions.asOptional()) |options|
            try writeType(meta_model, writer, options)
        else
            try writer.writeAll("null");
        try writer.writeAll("},},\n");
    }
    try writer.writeAll("\n};");
}
