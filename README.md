# [zig-json5](https://github.com/Himujjal/zig-json5)

A [JSON5](https://json5.org) parser/stringifier for Zig that resembles the `std.json` API from the standard library

## Installation 

Simply get the `src/main.zig` file using [curl](https://curl.se/) or [wget](https://www.gnu.org/software/wget/):

CURL:
```sh
curl https://raw.githubusercontent.com/Himujjal/zig-json5/master/src/main.zig --output json5.zig 
```

WGET:
```sh
wget https://raw.githubusercontent.com/Himujjal/zig-json5/master/src/main.zig -O json5.zig
```

## Docs

The API is similar to the `std.json` library. Just replace `json` with `json5` wherever possible

For a short tutorial see: [Zig JSON in 5 minutes](https://www.huy.rocks/everyday/01-09-2022-zig-json-in-5-minutes)

A simple example:

```zig
fn parseStringifyAndTest(input: []const u8, expected: []const u8) !void {
    const a = std.testing.allocator;

    var string = std.ArrayList(u8).init(a);
    defer string.deinit();

    var parser = json5.Parser.init(a, false);
    defer parser.deinit();

    var tree = try parser.parse(input);
    defer tree.deinit();

    try tree.root.json5Stringify(.{}, string.writer());

    std.testing.expect(mem.eql(u8, string.items, expected)) catch |err| {
        std.debug.print("\n == Expected: {s}\n Got: {s} ==\n", .{ expected, string.items });
        return err;
    };
}
```
