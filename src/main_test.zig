const std = @import("std");
const builtin = @import("builtin");

const mem = std.mem;
const testing = std.testing;
const assert = std.debug.assert;

const main = @import("main.zig");

const StringifyOptions = main.StringifyOptions;
const TokenStream = main.TokenStream;
const ParseOptions = main.ParseOptions;
const StreamingParser = main.StreamingParser;
const stringify = main.stringify;
const parse = main.parse;
const parseFree = main.parseFree;

/// Checks to see if a string matches what it would be as a json5-encoded string
/// Assumes that `encoded` is a well-formed json5 string
fn encodesTo(decoded: []const u8, encoded: []const u8) bool {
    var i: usize = 0;
    var j: usize = 0;
    while (i < decoded.len) {
        if (j >= encoded.len) return false;
        if (encoded[j] != '\\') {
            if (decoded[i] != encoded[j]) return false;
            j += 1;
            i += 1;
        } else {
            const escape_type = encoded[j + 1];
            if (escape_type != 'u') {
                const t: u8 = switch (escape_type) {
                    '\\' => '\\',
                    '/' => '/',
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    'f' => 12,
                    'b' => 8,
                    '"' => '"',
                    else => unreachable,
                };
                if (decoded[i] != t) return false;
                j += 2;
                i += 1;
            } else {
                var codepoint = std.fmt.parseInt(u21, encoded[j + 2 .. j + 6], 16) catch unreachable;
                j += 6;
                if (codepoint >= 0xD800 and codepoint < 0xDC00) {
                    // surrogate pair
                    assert(encoded[j] == '\\');
                    assert(encoded[j + 1] == 'u');
                    const low_surrogate = std.fmt.parseInt(u21, encoded[j + 2 .. j + 6], 16) catch unreachable;
                    codepoint = 0x10000 + (((codepoint & 0x03ff) << 10) | (low_surrogate & 0x03ff));
                    j += 6;
                }
                var buf: [4]u8 = undefined;
                const len = std.unicode.utf8Encode(codepoint, &buf) catch unreachable;
                if (i + len > decoded.len) return false;
                if (!mem.eql(u8, decoded[i .. i + len], buf[0..len])) return false;
                i += len;
            }
        }
    }
    assert(i == decoded.len);
    assert(j == encoded.len);
    return true;
}

const SkipValueError = error{UnexpectedJson5Depth} || TokenStream.Error;

fn skipValue(tokens: *TokenStream) SkipValueError!void {
    const original_depth = tokens.stackUsed();

    // Return an error if no value is found
    _ = try tokens.next();
    if (tokens.stackUsed() < original_depth) return error.UnexpectedJson5Depth;
    if (tokens.stackUsed() == original_depth) return;

    while (try tokens.next()) |_| {
        if (tokens.stackUsed() == original_depth) return;
    }
}

/// Returns if a value returned by `parse` is deep-equal to another value
fn parsedEqual(a: anytype, b: @TypeOf(a)) bool {
    switch (@typeInfo(@TypeOf(a))) {
        .Optional => {
            if (a == null and b == null) return true;
            if (a == null or b == null) return false;
            return parsedEqual(a.?, b.?);
        },
        .Union => |info| {
            if (info.tag_type) |UnionTag| {
                const tag_a = std.meta.activeTag(a);
                const tag_b = std.meta.activeTag(b);
                if (tag_a != tag_b) return false;

                inline for (info.fields) |field_info| {
                    if (@field(UnionTag, field_info.name) == tag_a) {
                        return parsedEqual(@field(a, field_info.name), @field(b, field_info.name));
                    }
                }
                return false;
            } else {
                unreachable;
            }
        },
        .Array => {
            for (a, 0..) |e, i|
                if (!parsedEqual(e, b[i])) return false;
            return true;
        },
        .Struct => |info| {
            inline for (info.fields) |field_info| {
                if (!parsedEqual(@field(a, field_info.name), @field(b, field_info.name))) return false;
            }
            return true;
        },
        .Pointer => |ptrInfo| switch (ptrInfo.size) {
            .One => return parsedEqual(a.*, b.*),
            .Slice => {
                if (a.len != b.len) return false;
                for (a, 0..) |e, i|
                    if (!parsedEqual(e, b[i])) return false;
                return true;
            },
            .Many, .C => unreachable,
        },
        else => return a == b,
    }
    unreachable;
}

/// parse tokens from a stream, returning `false` if they do not decode to `value`
fn parsesTo(comptime T: type, value: T, tokens: *TokenStream, options: ParseOptions) !bool {
    // TODO: should be able to write this function to not require an allocator
    const tmp = try parse(T, tokens, options);
    defer parseFree(T, tmp, options);

    return parsedEqual(tmp, value);
}

test "stringify null optional fields" {
    const MyStruct = struct {
        optional: ?[]const u8 = null,
        required: []const u8 = "something",
        another_optional: ?[]const u8 = null,
        another_required: []const u8 = "something else",
    };
    try teststringify(
        \\{"optional":null,"required":"something","another_optional":null,"another_required":"something else"}
    ,
        MyStruct{},
        StringifyOptions{},
    );
    try teststringify(
        \\{"required":"something","another_required":"something else"}
    ,
        MyStruct{},
        StringifyOptions{ .emit_null_optional_fields = false },
    );

    var ts = TokenStream.init(
        \\{"required":"something","another_required":"something else"}
    );
    try std.testing.expect(try parsesTo(MyStruct, MyStruct{}, &ts, .{
        .allocator = std.testing.allocator,
    }));
}

test "skipValue" {
    var ts = TokenStream.init("false");
    try skipValue(&ts);
    ts = TokenStream.init("true");
    try skipValue(&ts);
    ts = TokenStream.init("null");
    try skipValue(&ts);
    ts = TokenStream.init("42");
    try skipValue(&ts);
    ts = TokenStream.init("42.0");
    try skipValue(&ts);
    ts = TokenStream.init("\"foo\"");
    try skipValue(&ts);
    ts = TokenStream.init("[101, 111, 121]");
    try skipValue(&ts);
    ts = TokenStream.init("{}");
    try skipValue(&ts);
    ts = TokenStream.init("{/**/\"foo\": \"bar\"}");
    try skipValue(&ts);

    { // An absurd number of nestings
        const nestings = StreamingParser.default_max_nestings + 1;

        ts = TokenStream.init("[" ** nestings ++ "]" ** nestings);
        try testing.expectError(error.TooManyNestedItems, skipValue(&ts));
    }

    { // Would a number token cause problems in a deeply-nested array?
        const nestings = StreamingParser.default_max_nestings;
        const deeply_nested_array = "[" ** nestings ++ "0.118, 999, 881.99, 911.9, 725, 3" ++ "]" ** nestings;

        ts = TokenStream.init(deeply_nested_array);
        try skipValue(&ts);

        ts = TokenStream.init("[" ++ deeply_nested_array ++ "]");
        try testing.expectError(error.TooManyNestedItems, skipValue(&ts));
    }

    // Mismatched brace/square bracket
    ts = TokenStream.init("[102, 111, 111}");
    try testing.expectError(error.UnexpectedClosingBrace, skipValue(&ts));

    { // should fail if no value found (e.g. immediate close of object)
        var empty_object = TokenStream.init("{}");
        assert(.ObjectBegin == (try empty_object.next()).?);
        try testing.expectError(error.UnexpectedJson5Depth, skipValue(&empty_object));

        var empty_array = TokenStream.init("[]");
        assert(.ArrayBegin == (try empty_array.next()).?);
        try testing.expectError(error.UnexpectedJson5Depth, skipValue(&empty_array));
    }
}

test "stringify basic types" {
    try teststringify("false", false, StringifyOptions{});
    try teststringify("true", true, StringifyOptions{});
    try teststringify("null", @as(?u8, null), StringifyOptions{});
    try teststringify("null", @as(?*u32, null), StringifyOptions{});
    try teststringify("42", 42, StringifyOptions{});
    try teststringify("4.2e1", 42.0, StringifyOptions{});
    try teststringify("42", @as(u8, 42), StringifyOptions{});
    try teststringify("42", @as(u128, 42), StringifyOptions{});
    try teststringify("4.2e1", @as(f32, 42), StringifyOptions{});
    try teststringify("4.2e1", @as(f64, 42), StringifyOptions{});
    try teststringify("\"ItBroke\"", @as(anyerror, error.ItBroke), StringifyOptions{});
}

test "stringify string" {
    try teststringify("\"hello\"", "hello", StringifyOptions{});
    try teststringify("\"with\\nescapes\\r\"", "with\nescapes\r", StringifyOptions{});
    try teststringify("\"with\\nescapes\\r\"", "with\nescapes\r", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\\u0001\"", "with unicode\u{1}", StringifyOptions{});
    try teststringify("\"with unicode\\u0001\"", "with unicode\u{1}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{80}\"", "with unicode\u{80}", StringifyOptions{});
    try teststringify("\"with unicode\\u0080\"", "with unicode\u{80}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{FF}\"", "with unicode\u{FF}", StringifyOptions{});
    try teststringify("\"with unicode\\u00ff\"", "with unicode\u{FF}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{100}\"", "with unicode\u{100}", StringifyOptions{});
    try teststringify("\"with unicode\\u0100\"", "with unicode\u{100}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{800}\"", "with unicode\u{800}", StringifyOptions{});
    try teststringify("\"with unicode\\u0800\"", "with unicode\u{800}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{8000}\"", "with unicode\u{8000}", StringifyOptions{});
    try teststringify("\"with unicode\\u8000\"", "with unicode\u{8000}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{D799}\"", "with unicode\u{D799}", StringifyOptions{});
    try teststringify("\"with unicode\\ud799\"", "with unicode\u{D799}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{10000}\"", "with unicode\u{10000}", StringifyOptions{});
    try teststringify("\"with unicode\\ud800\\udc00\"", "with unicode\u{10000}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"with unicode\u{10FFFF}\"", "with unicode\u{10FFFF}", StringifyOptions{});
    try teststringify("\"with unicode\\udbff\\udfff\"", "with unicode\u{10FFFF}", StringifyOptions{ .string = .{ .String = .{ .escape_unicode = true } } });
    try teststringify("\"/\"", "/", StringifyOptions{});
    try teststringify("\"\\/\"", "/", StringifyOptions{ .string = .{ .String = .{ .escape_solidus = true } } });
}

test "stringify tagged unions" {
    try teststringify("42", union(enum) {
        Foo: u32,
        Bar: bool,
    }{ .Foo = 42 }, StringifyOptions{});
}

test "stringify struct" {
    try teststringify("{\"foo\":42}", struct {
        foo: u32,
    }{ .foo = 42 }, StringifyOptions{});
}

test "stringify struct with string as array" {
    try teststringify("{\"foo\":\"bar\"}", .{ .foo = "bar" }, StringifyOptions{});
    try teststringify("{\"foo\":[98,97,114]}", .{ .foo = "bar" }, StringifyOptions{ .string = .Array });
}

test "stringify struct with indentation" {
    try teststringify(
        \\{
        \\    "foo": 42,
        \\    "bar": [
        \\        1,
        \\        2,
        \\        3
        \\    ]
        \\}
    ,
        struct {
            foo: u32,
            bar: [3]u32,
        }{
            .foo = 42,
            .bar = .{ 1, 2, 3 },
        },
        StringifyOptions{
            .whitespace = .{},
        },
    );
    try teststringify(
        "{\n\t\"foo\":42,\n\t\"bar\":[\n\t\t1,\n\t\t2,\n\t\t3\n\t]\n}",
        struct {
            foo: u32,
            bar: [3]u32,
        }{
            .foo = 42,
            .bar = .{ 1, 2, 3 },
        },
        StringifyOptions{
            .whitespace = .{
                .indent = .Tab,
                .separator = false,
            },
        },
    );
    try teststringify(
        \\{"foo":42,"bar":[1,2,3]}
    ,
        struct {
            foo: u32,
            bar: [3]u32,
        }{
            .foo = 42,
            .bar = .{ 1, 2, 3 },
        },
        StringifyOptions{
            .whitespace = .{
                .indent = .None,
                .separator = false,
            },
        },
    );
}

test "stringify struct with void field" {
    try teststringify("{\"foo\":42}", struct {
        foo: u32,
        bar: void = {},
    }{ .foo = 42 }, StringifyOptions{});
}

test "stringify array of structs" {
    const MyStruct = struct {
        foo: u32,
    };
    try teststringify("[{\"foo\":42},{\"foo\":100},{\"foo\":1000}]", [_]MyStruct{
        MyStruct{ .foo = 42 },
        MyStruct{ .foo = 100 },
        MyStruct{ .foo = 1000 },
    }, StringifyOptions{});
}

test "stringify struct with custom stringifier" {
    try teststringify("[\"something special\",42]", struct {
        foo: u32,
        const Self = @This();
        pub fn json5Stringify(
            value: Self,
            options: StringifyOptions,
            out_stream: anytype,
        ) !void {
            _ = value;
            try out_stream.writeAll("[\"something special\",");
            try stringify(42, options, out_stream);
            try out_stream.writeByte(']');
        }
    }{ .foo = 42 }, StringifyOptions{});
}

test "stringify vector" {
    const vector: @Vector(2, u32) = @splat(@as(u32, 1));
    try teststringify("[1,1]", vector, StringifyOptions{});
}

fn teststringify(expected: []const u8, value: anytype, options: StringifyOptions) !void {
    const ValidationWriter = struct {
        const Self = @This();
        pub const Writer = std.io.Writer(*Self, Error, write);
        pub const Error = error{
            TooMuchData,
            DifferentData,
        };

        expected_remaining: []const u8,

        fn init(exp: []const u8) Self {
            return .{ .expected_remaining = exp };
        }

        pub fn writer(self: *Self) Writer {
            return .{ .context = self };
        }

        fn write(self: *Self, bytes: []const u8) Error!usize {
            if (self.expected_remaining.len < bytes.len) {
                std.debug.print(
                    \\====== expected this output: =========
                    \\{s}
                    \\======== instead found this: =========
                    \\{s}
                    \\======================================
                , .{
                    self.expected_remaining,
                    bytes,
                });
                return error.TooMuchData;
            }
            if (!mem.eql(u8, self.expected_remaining[0..bytes.len], bytes)) {
                std.debug.print(
                    \\====== expected this output: =========
                    \\{s}
                    \\======== instead found this: =========
                    \\{s}
                    \\======================================
                , .{
                    self.expected_remaining[0..bytes.len],
                    bytes,
                });
                return error.DifferentData;
            }
            self.expected_remaining = self.expected_remaining[bytes.len..];
            return bytes.len;
        }
    };

    var vos = ValidationWriter.init(expected);
    try stringify(value, options, vos.writer());
    if (vos.expected_remaining.len > 0) return error.NotEnoughData;
}

test "encodesTo" {
    // same
    try testing.expectEqual(true, encodesTo("false", "false"));
    // totally different
    try testing.expectEqual(false, encodesTo("false", "true"));
    // different lengths
    try testing.expectEqual(false, encodesTo("false", "other"));
    // with escape
    try testing.expectEqual(true, encodesTo("\\", "\\\\"));
    try testing.expectEqual(true, encodesTo("with\nescape", "with\\nescape"));
    // with unicode
    try testing.expectEqual(true, encodesTo("ą", "\\u0105"));
    try testing.expectEqual(true, encodesTo("😂", "\\ud83d\\ude02"));
    try testing.expectEqual(true, encodesTo("withąunicode😂", "with\\u0105unicode\\ud83d\\ude02"));
}

/// Parses `input` skipping comments and whitespaces, then stringifies them to JSON and then tests the
/// stringified value to `output`
fn parseStringifyAndTest(input: []const u8, expected: []const u8) !void {
    const last_char = expected[expected.len - 1];
    const expected_new = if (last_char == 0 or last_char == 10) expected[0..(expected.len - 1)] else expected;

    const Parser = main.Parser;
    const a = testing.allocator;

    var string = std.ArrayList(u8).init(a);
    defer string.deinit();
    var parser = Parser.init(a, false);
    defer parser.deinit();
    var tree = try parser.parse(input);
    defer tree.deinit();
    try tree.root.json5Stringify(.{}, string.writer());

    testing.expect(mem.eql(u8, string.items, expected_new)) catch |err| {
        std.debug.print("\n===Expected:\n{s}\n===Got:\n{s}\n", .{ expected_new, string.items });
        return err;
    };
}

const JSON5TestStruct = struct {
    source: []const u8,
    output: []const u8,
};
const json5_tests = &[_]JSON5TestStruct{
    .{ .source = "/**/{}", .output = "{}" },
    .{ .source = "/**/null", .output = "null" },
    .{ .source = "/**/[]", .output = "[]" },
    .{ .source = "[/**/]", .output = "[]" },
    .{ .source = "{/**/}", .output = "{}" },
    .{ .source =
    \\ // comment1
    \\ {// comment2
    \\ "hello" // comment3
    \\ : // comment4
    \\ "world" // comment 5
    \\ }
    \\ // comment 6
    , .output =
    \\{"hello":"world"}
    },
    .{ .source = "'he\\'llo'", .output = "\"he'llo\"" },
    .{ .source = "{ 'he\\'llo': 'world' }", .output = "{\"he'llo\":\"world\"}" },
    .{ .source = "['hello', { 'hello': \"world\" }]", .output = "[\"hello\",{\"hello\":\"world\"}]" },
    .{ .source = "{hello: 'world'}", .output = "{\"hello\":\"world\"}" },
    .{ .source = "{hello: 'world', yo: ['yo',]}", .output = "{\"hello\":\"world\",\"yo\":[\"yo\"]}" },

    .{ .source =
    \\ { hello: "\
    \\world \n 2" }
    , .output = "{\"hello\":\"\\nworld \\n 2\"}" },

    .{ .source = "0.1 ", .output = "1e-1" },
    .{ .source = "{true:true,false:[true,false],null:1}", .output = "{\"true\":true,\"false\":[true,false],\"null\":1}" },

    .{ .source = "{true:true, null:[true, null]}", .output = "{\"true\":true,\"null\":[true,null]}" },

    .{
        .source =
        \\ {true:true,false:1,null:{null:null,true:false}}
        ,
        .output = "{\"true\":true,\"false\":1,\"null\":{\"null\":null,\"true\":false}}",
    },
    .{ .source =
    \\{"required":"something","another_required":"something else"}
    , .output =
    \\{"required":"something","another_required":"something else"}
    },

    .{ .source =
    \\{required:"something",another_required:"something else",null:1}
    , .output =
    \\{"required":"something","another_required":"something else","null":1}
    },
    .{ .source =
    \\{
    \\ 		// name: "import"
    \\ 		p: null,
    \\      // hee
    \\      a: [null,],
    \\}
    , .output =
    \\{"p":null,"a":[null]}
    },
    .{ .source = @embedFile("./fixtures/test1.json5"), .output = @embedFile("./fixtures/test1.json") },
    .{ .source = @embedFile("./fixtures/test2.json5"), .output = @embedFile("./fixtures/test2.json") },
    .{ .source = @embedFile("./fixtures/test3.json5"), .output = @embedFile("./fixtures/test3.json") },
    .{ .source = @embedFile("./fixtures/test4.json5"), .output = @embedFile("./fixtures/test4.json") },
};

test "JSON5 tests" {
    for (json5_tests, 0..) |json_test, i| {
        parseStringifyAndTest(json_test.source, json_test.output) catch |err| {
            std.debug.print(
                "\n====ERROR in Comparison: Error:{}\n i={d}====\nSOURCE =====>\n{s}\n======\nOUTPUT =====>\n{s}\n",
                .{ err, i, json_test.source, json_test.output },
            );
            return err;
        };
    }
}

test "JSON5 Deep Equality Test" {
    const a = testing.allocator;
    const Parser = main.Parser;

    for (json5_tests, 0..) |json_test, i| {
        var source_parser = Parser.init(a, false);
        var source_tree = try source_parser.parse(json_test.source);
        var output_parser = Parser.init(a, false);
        var output_tree = try output_parser.parse(json_test.source);

        if (i == 5) {
            const is_equal = try main.json5Equal(a, source_tree.root, output_tree.root);
            std.testing.expect(is_equal) catch |err| {
                std.debug.print(
                    "\n====ERROR in Deep equality: i={d}====\nEXPECTED =====>\n{s}\n======\nGOT =====>\n{s}\n",
                    .{ i, json_test.source, json_test.output },
                );

                source_parser.deinit();
                source_tree.deinit();
                output_parser.deinit();
                output_tree.deinit();
                return err;
            };
        }

        source_parser.deinit();
        source_tree.deinit();
        output_parser.deinit();
        output_tree.deinit();
    }
}

test "json5 write stream" {
    var out_buf: [1024]u8 = undefined;
    var slice_stream = std.io.fixedBufferStream(&out_buf);
    const out = slice_stream.writer();

    var arena_allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena_allocator.deinit();

    var w = main.writeStream(out, 10);

    try w.beginObject();

    try w.objectField("object");
    try w.emitJson(try getJsonObject(arena_allocator.allocator()));

    try w.objectField("string");
    try w.emitString("This is a string");

    try w.objectField("array");
    try w.beginArray();
    try w.arrayElem();
    try w.emitString("Another string");
    try w.arrayElem();
    try w.emitNumber(@as(i32, 1));
    try w.arrayElem();
    try w.emitNumber(@as(f32, 3.5));
    try w.endArray();

    try w.objectField("int");
    try w.emitNumber(@as(i32, 10));

    try w.objectField("float");
    try w.emitNumber(@as(f32, 3.5));

    try w.endObject();

    const result = slice_stream.getWritten();
    const expected =
        \\{
        \\ "object": {
        \\  "one": 1,
        \\  "two": 2e0
        \\ },
        \\ "string": "This is a string",
        \\ "array": [
        \\  "Another string",
        \\  1,
        \\  3.5e0
        \\ ],
        \\ "int": 10,
        \\ "float": 3.5e0
        \\}
    ;
    try std.testing.expectEqualSlices(u8, expected, result);
}

fn getJsonObject(allocator: std.mem.Allocator) !main.Value {
    var value = main.Value{ .Object = main.ObjectMap.init(allocator) };
    try value.Object.put("one", main.Value{ .Integer = @as(i64, @intCast(1)) });
    try value.Object.put("two", main.Value{ .Float = 2.0 });
    return value;
}
