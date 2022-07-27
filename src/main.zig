const std = @import("std");
const tres = @import("tres");
const MetaModel = @import("MetaModel.zig");

pub var a = 123;

pub fn main() anyerror!void {
    @setEvalBranchQuota(100_000);

    const allocator = std.heap.page_allocator;

    var input_model_file = try std.fs.cwd().openFile("metaModel.json", .{});
    var data = try allocator.alloc(u8, (try input_model_file.stat()).size);
    defer input_model_file.close();
    defer allocator.free(data);

    _ = try input_model_file.reader().readAll(data);

    var parser = std.json.Parser.init(allocator, true);
    var tree = try parser.parse(data);

    defer parser.deinit();
    defer tree.deinit();

    var meta_model = try tres.parse(MetaModel, tree.root, tree.arena.allocator());

    var out_file = try std.fs.cwd().createFile("lsp.zig", .{});
    defer out_file.close();

    var bufw = std.io.bufferedWriter(out_file.writer());
    try writeMetaModel(bufw.writer(), meta_model);
    try bufw.flush();
}

fn writeDocs(writer: anytype, docs: []const u8) !void {
    var iterator = std.mem.split(u8, docs, "\n");
    while (iterator.next()) |line| try writer.print("/// {s}\n", .{line});
}

fn writeDocsAsNormal(writer: anytype, docs: []const u8) !void {
    var iterator = std.mem.split(u8, docs, "\n");
    while (iterator.next()) |line| try writer.print("// {s}\n", .{line});
}

fn guessTypeName(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type, i: usize) anyerror!void {
    switch (typ) {
        .BaseType => |base| try switch (base.name) {
            .Uri => writer.writeAll("uri"),
            .DocumentUri => writer.writeAll("document_uri"),
            .integer => writer.writeAll("integer"),
            .uinteger, .decimal => writer.writeAll("uinteger"),
            .RegExp => writer.writeAll("regexp"),
            .string => writer.writeAll("string"),
            .boolean => writer.writeAll("bool"),
            .@"null" => writer.writeAll("@\"null\""),
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

fn writeType(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type) anyerror!void {
    switch (typ) {
        .BaseType => |base| try switch (base.name) {
            .Uri => writer.writeAll("URI"),
            .DocumentUri => writer.writeAll("DocumentUri"),
            .integer => writer.writeAll("i64"),
            .uinteger, .decimal => writer.writeAll("u64"),
            .RegExp => writer.writeAll("RegExp"),
            .string => writer.writeAll("[]const u8"),
            .boolean => writer.writeAll("bool"),
            .@"null" => writer.writeAll("?void"),
        },
        .ReferenceType => |ref| try writer.print("{s}", .{std.zig.fmtId(ref.name)}),
        .ArrayType => |arr| {
            try writer.writeAll("[]");
            try writeType(meta_model, writer, arr.element.*);
        },
        .MapType => |map| {
            try writer.writeAll("Map(");
            switch (map.key) {
                .base => |base| try switch (base.name) {
                    .Uri => writer.writeAll("Uri"),
                    .DocumentUri => writer.writeAll("DocumentUri"),
                    .integer => writer.writeAll("i64"),
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
            if (ort.items.len == 2 and ort.items[1] == .BaseType and ort.items[1].BaseType.name == .@"null") {
                try writer.writeByte('?');
                try writeType(meta_model, writer, ort.items[0]);
            } else if (isOrActuallyEnum(ort)) {
                try writer.writeAll("enum {");
                for (ort.items) |sub_type| {
                    try writer.print("{s},\n", .{sub_type.StringLiteralType.value});
                }
                try writer.writeByte('}');
            } else {
                var has_null = ort.items[ort.items.len - 1] == .BaseType and ort.items[ort.items.len - 1].BaseType.name == .@"null";

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
            try writer.writeAll("std.meta.Tuple(&[_]type {");
            for (tup.items) |t, i| {
                try writeType(meta_model, writer, t);
                if (tup.items.len - 1 != i) try writer.writeAll(", ");
            }
            try writer.writeAll("})");
        },
        .StructureLiteralType => |lit| {
            try writer.writeAll("struct {\n");
            for (lit.value.properties) |property| try writeProperty(meta_model, writer, property);
            try writer.writeAll("\n}");
        },
        .StringLiteralType => |lit| {
            try writer.print("[]const u8 = \"{s}\"", .{lit.value});
        },
        .IntegerLiteralType => |lit| {
            try writer.print("i64 = {d}", .{lit.value});
        },
        .BooleanLiteralType => |lit| {
            try writer.print("bool = {s}", .{lit.value});
        },
    }
}

fn writeProperty(meta_model: MetaModel, writer: anytype, property: MetaModel.Property) anyerror!void {
    if (property.documentation.asOptional()) |docs| try writeDocs(writer, docs);
    if (property.optional.asOptional()) |op| if (op) try writer.writeAll("/// field can be undefined, but this possible state is non-critical\n");
    switch (property.type) {
        .StringLiteralType,
        .IntegerLiteralType,
        .BooleanLiteralType,
        => try writer.writeAll("comptime "),
        else => {},
    }
    try writer.print("{s}: ", .{std.zig.fmtId(property.name)});
    try writeType(meta_model, writer, property.type);
    try writer.writeAll(",\n");
}

fn writeProperties(meta_model: MetaModel, writer: anytype, structure: MetaModel.Structure, maybe_extender: ?MetaModel.Structure) anyerror!void {
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
            .string => try writer.print("pub const {s} = enum {{\n", .{std.zig.fmtId(enumeration.name)}),
            .integer => try writer.print("pub const {s} = enum(i64) {{\n", .{std.zig.fmtId(enumeration.name)}),
            .uinteger => try writer.print("pub const {s} = enum(u64) {{\n", .{std.zig.fmtId(enumeration.name)}),
        }

        for (enumeration.values) |entry| {
            if (entry.documentation.asOptional()) |docs| try writeDocs(writer, docs);
            switch (entry.value) {
                .string => |value| try writer.print("{s},\n", .{std.zig.fmtId(value)}),
                .number => |value| try writer.print("{s} = {d},\n", .{ std.zig.fmtId(entry.name), value }),
            }
        }

        if (enumeration.values.len == 1) {
            try writer.writeAll("placeholder__, // fixes alignment issue\n");
        }

        try writer.writeAll("};\n\n");
    }

    try writer.writeAll("// Structures\n\n");
    for (meta_model.structures) |structure| {
        if (std.mem.eql(u8, structure.name, "LSPObject")) continue;

        if (structure.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("pub const {s} = struct {{\n", .{std.zig.fmtId(structure.name)});

        try writeProperties(meta_model, writer, structure, null);

        try writer.writeAll("};\n\n");
    }

    try writer.writeAll("const methods = [_]Method{\n// Notifications\n");
    for (meta_model.notifications) |notification| {
        if (notification.documentation.asOptional()) |docs| try writeDocsAsNormal(writer, docs);
        try writer.print(".{{.method = \"{s}\", .RequestType =", .{notification.method});
        if (notification.params.asOptional()) |params|
            // NOTE: Multiparams not used here, so we dont have to implement them :)
            try writeType(meta_model, writer, params.Type)
        else
            try writer.writeAll("void");
        try writer.writeAll(", .ResponseType = null, .RegistrationType = ");
        if (notification.registrationOptions.asOptional()) |options|
            try writeType(meta_model, writer, options)
        else
            try writer.writeAll("void");
        try writer.writeAll("},\n");
    }
    try writer.writeAll("\n};");

    // try writer.writeAll(
    //     \\// Notifications
    //     \\
    //     \\pub const NotificationParams = union(enum) {
    //     \\
    //     \\pub const registration_options = .{
    //     \\
    // );
    // for (meta_model.notifications) |notification| {
    //     try writer.print(".{{\"{s}\", ", .{notification.method});
    //     if (notification.registrationOptions.asOptional()) |options|
    //         try writeType(meta_model, writer, options)
    //     else
    //         try writer.writeAll("void");
    //     try writer.writeAll("},\n");
    // }
    // try writer.writeAll("};\n\n");
    // for (meta_model.notifications) |notification| {
    //     if (notification.documentation.asOptional()) |docs| try writeDocs(writer, docs);
    //     try writer.print("{s}: ", .{std.zig.fmtId(notification.method)});
    //     if (notification.params.asOptional()) |params|
    //         // NOTE: Multiparams not used here, so we dont have to implement them :)
    //         try writeType(meta_model, writer, params.Type)
    //     else
    //         try writer.writeAll("void");
    //     try writer.writeAll(",\n");
    // }
    // try writer.writeAll("};\n\n");

    // try writer.writeAll(
    //     \\// Requests
    //     \\
    //     \\pub const RequestParams = union(enum) {
    //     \\
    //     \\pub const registration_options = .{
    //     \\
    // );
    // for (meta_model.requests) |request| {
    //     try writer.print(".{{\"{s}\", ", .{request.method});
    //     if (request.registrationOptions.asOptional()) |options|
    //         try writeType(meta_model, writer, options)
    //     else
    //         try writer.writeAll("void");
    //     try writer.writeAll("},\n");
    // }
    // try writer.writeAll("};\n\n");
    // for (meta_model.requests) |request| {
    //     if (request.documentation.asOptional()) |docs| try writeDocs(writer, docs);
    //     try writer.print("{s}: ", .{std.zig.fmtId(request.method)});
    //     if (request.params.asOptional()) |params|
    //         // NOTE: Multiparams not used here, so we dont have to implement them :)
    //         try writeType(meta_model, writer, params.Type)
    //     else
    //         try writer.writeAll("void");
    //     try writer.writeAll(",\n");
    // }
    // try writer.writeAll("};\n\n");

    // try writer.writeAll(
    //     \\// Results
    //     \\
    //     \\pub const Result = union(enum) {
    //     \\
    // );
    // for (meta_model.requests) |request| {
    //     if (request.documentation.asOptional()) |docs| try writeDocs(writer, docs);
    //     try writer.print("{s}: ", .{std.zig.fmtId(request.method)});
    //     try writeType(meta_model, writer, request.result);
    //     try writer.writeAll(",\n");
    // }
    // try writer.writeAll("};\n\n");
}
