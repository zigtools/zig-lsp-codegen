const std = @import("std");
const tres = @import("tres");
const MetaModel = @import("MetaModel.zig");

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

fn guessTypeName(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type) anyerror!void {
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
            try writer.writeAll("array_of");
            try guessTypeName(meta_model, writer, arr.element.*);
        },
        .MapType => try writer.writeAll("map"),
        .OrType => try writer.writeAll("ort"),
        .TupleType => try writer.writeAll("tuple"),
        .StructureLiteralType,
        .StringLiteralType,
        .IntegerLiteralType,
        .BooleanLiteralType,
        => try writer.writeAll("literal"),
        else => @panic("Impossible!"),
    }
}

fn writeType(meta_model: MetaModel, writer: anytype, typ: MetaModel.Type) anyerror!void {
    switch (typ) {
        .BaseType => |base| try switch (base.name) {
            .Uri => writer.writeAll("Uri"),
            .DocumentUri => writer.writeAll("DocumentUri"),
            .integer => writer.writeAll("i64"),
            .uinteger, .decimal => writer.writeAll("u64"),
            .RegExp => writer.writeAll("RegExp"),
            .string => writer.writeAll("[]const u8"),
            .boolean => writer.writeAll("bool"),
            // TODO: Handle this indicating an optional
            .@"null" => writer.writeAll("NullType"),
        },
        .ReferenceType => |ref| try writer.print("{s}", .{std.zig.fmtId(ref.name)}),
        .ArrayType => |arr| {
            try writer.writeAll("[]");
            try writeType(meta_model, writer, arr.element.*);
        },
        .MapType => |map| {
            try writer.writeAll("std.AutoHashMap(");
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
        // This doesn't appear anywhere so it doesn't need to be implemented!
        .AndType => @panic("Impossible!"),
        .OrType => |ort| {
            try writer.writeAll("union(enum) {");
            for (ort.items) |sub_type| {
                try guessTypeName(meta_model, writer, sub_type);
                try writer.writeAll(": ");
                try writeType(meta_model, writer, sub_type);
                try writer.writeAll(",\n");
            }
            try writer.writeByte('}');
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
    switch (property.type) {
        .StringLiteralType,
        .IntegerLiteralType,
        .BooleanLiteralType,
        => try writer.writeAll("comptime "),
        else => {},
    }
    try writer.print("{s}: ", .{std.zig.fmtId(property.name)});
    if (property.optional.asOptional()) |op| if (op) try writer.writeAll("Undefinedable(");
    try writeType(meta_model, writer, property.type);
    if (property.optional.asOptional()) |op| if (op) try writer.writeAll(")");
    try writer.writeAll(",\n");
}

fn writeProperties(meta_model: MetaModel, writer: anytype, structure: MetaModel.Structure) anyerror!void {
    for (structure.properties) |property| try writeProperty(meta_model, writer, property);

    if (structure.extends.asOptional()) |extends| {
        for (extends) |ext| {
            switch (ext) {
                .ReferenceType => |ref| {
                    try writer.print("// Extends {s}\n", .{ref.name});

                    for (meta_model.structures) |s| {
                        if (std.mem.eql(u8, s.name, ref.name)) {
                            try writeProperties(meta_model, writer, s);
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
                            try writeProperties(meta_model, writer, s);
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

    for (meta_model.enumerations) |enumeration| {
        if (enumeration.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("pub const {s} = enum {{\n", .{std.zig.fmtId(enumeration.name)});

        for (enumeration.values) |entry| {
            if (entry.documentation.asOptional()) |docs| try writeDocs(writer, docs);
            switch (entry.value) {
                .string => try writer.print("{s},\n", .{std.zig.fmtId(entry.name)}), // TODO: Actually save the string value
                .number => |value| try writer.print("{s} = {d},\n", .{ std.zig.fmtId(entry.name), value }),
            }
        }

        try writer.writeAll("};\n\n");
    }

    for (meta_model.structures) |structure| {
        if (structure.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("pub const {s} = struct {{\n", .{std.zig.fmtId(structure.name)});

        try writeProperties(meta_model, writer, structure);

        try writer.writeAll("};\n\n");
    }
}
