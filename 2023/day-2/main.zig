const std = @import("std");
const File = std.fs.File;
const CubeColors = enum { red, blue, green };

const GamePick = struct { r: u32, g: u32, b: u32 };

const Game = struct { game_id: u32, picks: std.ArrayList(GamePick) };

fn solve(input: []const u8) anyerror!u32 {
    var file = try std.fs.cwd().openFile(input, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    var in_stream = buf_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("{s}\n", .{line});
    }
    return 0;
}

const testing = std.testing;
const expect = testing.expect;
test "solve example" {
    const received = try solve("example.csv");
    try expect(received == 8);
}
