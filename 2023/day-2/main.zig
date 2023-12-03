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

fn count_ids(games: std.ArrayList(Game)) u32 {
    var ret: u32 = 0;
    for (games.items) |g| {
        ret += g.game_id;
    }
    return ret;
}

fn filter_possible_games(allocator: mem.Allocator, request: Request, games: std.ArrayList(Game)) anyerror!std.ArrayList(Game) {
    var ret = std.ArrayList(Game).init(allocator);
    for (games.items) |g| {
        var possible = true;
        for (g.picks.items) |p| {
            const is_possible = p.r <= request.r and p.g <= request.g and p.b <= request.b;
            if (!is_possible) {
                possible = false;
                break;
            }
        }
        if (possible) {
            try ret.append(g);
        }
    }
    return ret;
}

const Request = GamePick;

fn solve(input: []const u8, allocator: mem.Allocator, request: Request) anyerror!u32 {
    var file = try std.fs.cwd().openFile(input, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;
    var games = std.ArrayList(Game).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const game = try parse_line(allocator, line);
        try games.append(game);
    }
    defer games.deinit();
    defer for (games.items) |game| game.picks.deinit();

    const possible_games = try filter_possible_games(allocator, request, games);
    defer possible_games.deinit();

    //defer for (possible_games.items) |game| game.picks.deinit();
    const result = count_ids(possible_games);
    return result;
}

fn solve2(input: []const u8, allocator: mem.Allocator) anyerror!u32 {
    var file = try std.fs.cwd().openFile(input, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const in_stream = buf_reader.reader();
    var buf: [2048]u8 = undefined;
    var games = std.ArrayList(Game).init(allocator);
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        const game = try parse_line(allocator, line);
        try games.append(game);
    }
    defer games.deinit();
    defer for (games.items) |game| game.picks.deinit();

    var ret: u32 = 0;
    for (games.items) |game| {
        ret += compute_cube(game);
    }
    return ret;
}

fn compute_cube(game: Game) u32 {
    var r: u32 = 0;
    var g: u32 = 0;
    var b: u32 = 0;
    for (game.picks.items) |pick| {
        r = @max(pick.r, r);
        g = @max(pick.g, g);
        b = @max(pick.b, b);
    }
    return r * g * b;
}

const testing = std.testing;
const expect = testing.expect;
const expectEqual = testing.expectEqual;
const test_allocator = std.testing.allocator;

test "solve part1" {
    const received = try solve("input.csv", test_allocator, Request{ .r = 12, .g = 13, .b = 14 });
    try expectEqual(received, 2720); // todo: not sure why swapping won't compile.
}
test "solve part2" {
    const received = try solve2("input.csv", test_allocator);
    try expectEqual(received, 71535); // todo: not sure why swapping won't compile.
}
test "compute cube" {
    var picks = std.ArrayList(GamePick).init(test_allocator);
    try picks.append(GamePick{ .r = 4, .g = 4, .b = 2 });
    const game = Game{ .game_id = 1, .picks = picks };
    defer picks.deinit();

    const received = compute_cube(game);
    try expectEqual(received, 4 * 4 * 2);
}

test "solve example" {
    const received = try solve("example.csv", test_allocator, Request{ .r = 12, .g = 13, .b = 14 });
    try expectEqual(received, 8); // todo: not sure why swapping won't compile.
}
test "parse_single_pick" {
    try expect(std.meta.eql(try parse_single_pick("3 green"), Temp{ .color = CubeColor.green, .quantity = 3 }));
    try expect(std.meta.eql(try parse_single_pick("3 red"), Temp{ .color = CubeColor.red, .quantity = 3 }));
    try expect(std.meta.eql(try parse_single_pick("3 blue"), Temp{ .color = CubeColor.blue, .quantity = 3 }));
}

test "test count ids" {
    var games = std.ArrayList(Game).init(test_allocator);
    try games.append(Game{ .game_id = 1, .picks = std.ArrayList(GamePick).init(test_allocator) });
    try games.append(Game{ .game_id = 1, .picks = std.ArrayList(GamePick).init(test_allocator) });
    try games.append(Game{ .game_id = 2, .picks = std.ArrayList(GamePick).init(test_allocator) });
    defer games.deinit();
    defer for (games.items) |game| game.picks.deinit();
    const res = count_ids(games);
    try expect(res == 4);
}

test "parse pick" {
    try expect(std.meta.eql(try parse_pick(test_allocator, "3 blue, 4 red"), GamePick{ .r = 4, .g = 0, .b = 3 }));
    try expect(std.meta.eql(try parse_pick(test_allocator, "1 red, 2 green, 6 blue"), GamePick{ .r = 1, .g = 2, .b = 6 }));
}
