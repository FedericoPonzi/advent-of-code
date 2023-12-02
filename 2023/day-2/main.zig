const std = @import("std");
const File = std.fs.File;
const CubeColor = enum { red, blue, green };
const mem = std.mem;

const GamePick = struct { r: u32, g: u32, b: u32 };

const Game = struct {
    game_id: u32,
    picks: std.ArrayList(GamePick),

    pub fn format(
        game: Game,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.writeAll("Game{\n");
        _ = try writer.print("game_id: {x},\n picks: \n", .{game.game_id});
        for (game.picks.items) |p| _ = try writer.print("[r:{x},g:{x},b:{x}], ", .{ p.r, p.g, p.b });
        try writer.writeAll("}\n");
    }
};
const Temp = struct { color: CubeColor, quantity: u32 };

fn parse_single_pick(input: []const u8) anyerror!Temp {
    var it = std.mem.split(u8, input, " ");
    const next = it.next().?;
    const quantity = try std.fmt.parseInt(u32, next, 10);
    const color = it.next().?;
    const cubecolor = if (std.mem.eql(u8, color, "green")) CubeColor.green else if (std.mem.eql(u8, color, "red")) CubeColor.red else CubeColor.blue;

    return Temp{ .color = cubecolor, .quantity = quantity };
}

// Sample input: "3 blue, 4 red" or "1 red, 2 green, 6 blue"
fn parse_pick(_: mem.Allocator, input: []const u8) anyerror!GamePick {
    // try std.fmt.parseInt(u32, foo, 10);

    var it = std.mem.split(u8, input, ", ");
    var game_pick = GamePick{ .r = 0, .g = 0, .b = 0 };
    while (it.next()) |single_pick| {
        const temp = try parse_single_pick(single_pick);
        if (std.meta.eql(temp.color, CubeColor.red)) {
            game_pick.r = temp.quantity;
        }
        if (std.meta.eql(temp.color, CubeColor.green)) {
            game_pick.g = temp.quantity;
        }
        if (std.meta.eql(temp.color, CubeColor.blue)) {
            game_pick.b = temp.quantity;
        }
    }
    return game_pick;
}
// parses "3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
fn parse_picks(allocator: mem.Allocator, input: []const u8) anyerror!std.ArrayList(GamePick) {
    var it = std.mem.split(u8, input, "; ");
    var ret = std.ArrayList(GamePick).init(allocator);
    while (it.next()) |str_pick| {
        try ret.append(try parse_pick(allocator, str_pick));
    }
    return ret;
}

// parses "Game 1"
fn parse_game(allocator: mem.Allocator, input: []const u8) anyerror!Game {
    const game_picks = std.ArrayList(GamePick).init(allocator);
    var it = std.mem.split(u8, input, " ");
    _ = it.next(); // throw away "Game ";
    const game_id = try std.fmt.parseInt(u32, it.next().?, 10);
    const game = Game{ .game_id = game_id, .picks = game_picks };
    return game;
}
fn parse_line(allocator: mem.Allocator, input: []const u8) anyerror!Game {
    var it = std.mem.split(u8, input, ": ");
    var game = try parse_game(allocator, it.next().?);
    const picks = try parse_picks(allocator, it.next().?);
    game.picks = picks;
    return game;
}

fn solve(input: []const u8, allocator: mem.Allocator) anyerror!u32 {
    var file = try std.fs.cwd().openFile(input, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;
    var games = std.ArrayList(Game).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("line: {s}\n", .{line});
        const game = try parse_line(allocator, line);
        try games.append(game);
        std.debug.print("game: {s}\n", .{game});
    }

    defer games.deinit();
    defer for (games.items) |game| game.picks.deinit();

    return 0;
}

const testing = std.testing;
const expect = testing.expect;
const test_allocator = std.testing.allocator;
test "solve example" {
    const received = try solve("example.csv", test_allocator);
    try expect(received == 8);
}
test "parse_single_pick" {
    try expect(std.meta.eql(try parse_single_pick("3 green"), Temp{ .color = CubeColor.green, .quantity = 3 }));
    try expect(std.meta.eql(try parse_single_pick("3 red"), Temp{ .color = CubeColor.red, .quantity = 3 }));
    try expect(std.meta.eql(try parse_single_pick("3 blue"), Temp{ .color = CubeColor.blue, .quantity = 3 }));
}

test "parse pick" {
    try expect(std.meta.eql(try parse_pick(test_allocator, "3 blue, 4 red"), GamePick{ .r = 4, .g = 0, .b = 3 }));
    try expect(std.meta.eql(try parse_pick(test_allocator, "1 red, 2 green, 6 blue"), GamePick{ .r = 1, .g = 2, .b = 6 }));
}
