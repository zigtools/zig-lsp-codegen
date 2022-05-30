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

    try writeMetaModel(out_file.writer(), meta_model);
}

fn writeDocs(writer: anytype, docs: []const u8) !void {
    var iterator = std.mem.split(u8, docs, "\n");
    while (iterator.next()) |line| try writer.print("/// {s}\n", .{line});
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
        .AndType => try writer.writeAll("UnimplementedAndType"),
        .OrType => |ort| {
            try writer.writeAll("union(enum) {");
            for (ort.items) |sub_type, i| {
                try writer.print("unnamed_{d}: ", .{i});
                try writeType(meta_model, writer, sub_type);
                try writer.writeAll(",\n");
            }
            try writer.writeByte('}');
        },
        else => try writer.writeAll("UnimplementedType"),
    }
}

fn writeProperties(meta_model: MetaModel, writer: anytype, structure: MetaModel.Structure) anyerror!void {
    for (structure.properties) |property| {
        if (property.documentation.asOptional()) |docs| try writeDocs(writer, docs);
        try writer.print("{s}: ", .{std.zig.fmtId(property.name)});
        try writeType(meta_model, writer, property.type);
        try writer.writeAll(",\n");
    }

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
