// Advent of Code 2021, Day 11
// https://adventofcode.com/2021/day/11
// Solution by Darren Stone <dstone@bitmason.com>

const std = @import("std");

const dim = 10;
const Grid = [dim][dim]u8;
const flash_processed = dim * dim + dim; // > inc possibility, < u8.max
const XY = struct { x: isize, y: isize };
const neighbs = [_]XY{
    .{ .x = -1, .y = -1 }, .{ .x = 0, .y = -1 }, .{ .x = 1, .y = -1 },
    .{ .x = -1, .y = 0 },  .{ .x = 1, .y = 0 },  .{ .x = -1, .y = 1 },
    .{ .x = 0, .y = 1 },   .{ .x = 1, .y = 1 },
};
const part_1_steps = 100;
const input_path = "day-11-input.txt";

pub fn read() !Grid {
    var grid: Grid = undefined;
    var file = try std.fs.cwd().openFile(input_path, .{ .read = true });
    for (grid) |row, y| {
        for (row) |_, x| {
            grid[y][x] = (try file.reader().readByte()) - '0';
        }
        _ = try file.reader().readByte(); // skip newline
    }
    return grid;
}

pub fn flash(grid: *Grid) usize { // propogate flashes, return count
    var count_total: usize = 0;
    var count_pass: usize = 1; // enter loop first time
    var grid_new: Grid = undefined;
    grid_new = grid.*;
    while (count_pass > 0) { // until quiet
        count_pass = 0;
        for (grid) |row, y| {
            for (row) |e, x| {
                if (e > 9 and e < flash_processed) { // just hit flash thresh
                    for (neighbs) |nxy| {
                        const nx: isize = @intCast(isize, x) + nxy.x;
                        const ny: isize = @intCast(isize, y) + nxy.y;
                        if (nx >= 0 and ny >= 0 and nx < dim and ny < dim) {
                            var ne = grid_new[@intCast(usize, ny)][@intCast(usize, nx)] + 1;
                            grid_new[@intCast(usize, ny)][@intCast(usize, nx)] = ne;
                        }
                    }
                    grid_new[y][x] = flash_processed;
                    count_pass += 1;
                }
            }
        }
        grid.* = grid_new;
        count_total += count_pass;
    }
    for (grid) |row, y| {
        for (row) |e, x| {
            if (e > 9) grid[y][x] = 0; // post flash reset
        }
    }
    return count_total;
}

pub fn is_synced(grid: *Grid) bool {
    for (grid) |row, y| {
        for (row) |e, x| {
            if (e != 0) return false;
        }
    }
    return true;
}

pub fn inc(grid: *Grid) void {
    for (grid) |row, y| {
        for (row) |e, x| {
            grid[y][x] = e + 1;
        }
    }
}

pub fn main() !void {
    var grid: Grid = try read();
    var steps: usize = 0;

    var flash_count: usize = 0;
    while (steps < part_1_steps) : (steps += 1) {
        inc(&grid);
        flash_count += flash(&grid);
    }
    std.debug.print("Day 11 Part 1: {}\n", .{flash_count});

    grid = try read();
    steps = 0;
    while (!is_synced(&grid)) : (steps += 1) {
        inc(&grid);
        _ = flash(&grid);
    }
    std.debug.print("Day 11 Part 2: {}\n", .{steps});
}
