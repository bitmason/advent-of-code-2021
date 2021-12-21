// Advent of Code 2021, Day 20
// https://adventofcode.com/2021/day/20
// Solution by Darren Stone <dstone@bitmason.com>

const std = @import("std");

const dim = 100; // img width & height
const part_steps = [_]usize{ 2, 50 }; // part 1 & 2 applications required
const margin = std.mem.max(usize, part_steps[0..part_steps.len]) + 5; // generous margin
const Img = [dim + 2 * margin][dim + 2 * margin]u8;
const Algo = [512]u8;
const input_path = "day-20-input.txt";
const XY = struct { x: isize, y: isize };
const neighbs = [_]XY{ // bottom right to top left for easier u9 parsing
    .{ .x = 1, .y = 1 },  .{ .x = 0, .y = 1 },  .{ .x = -1, .y = 1 },
    .{ .x = 1, .y = 0 },  .{ .x = 0, .y = 0 },  .{ .x = -1, .y = 0 },
    .{ .x = 1, .y = -1 }, .{ .x = 0, .y = -1 }, .{ .x = -1, .y = -1 },
};

pub fn read(algo: *Algo, img: *Img) !void {
    var file = try std.fs.cwd().openFile(input_path, .{ .read = true });
    _ = try file.reader().readUntilDelimiterOrEof(algo, '\n');
    _ = try file.reader().readByte();
    for (img) |*row, y| {
        row.* = [_]u8{'.'} ** row.len; // init with all dark for margins
        if (y >= margin and y <= dim + margin) {
            _ = try file.reader().readUntilDelimiterOrEof(img[y][margin..(margin + dim)], '\n');
        }
    }
}

// apply algorithm to image
pub fn apply(algo: *Algo, img: *Img) void {
    var img_new: Img = img.*;
    for (img) |row, y| {
        for (row) |pix, x| {
            var hood: usize = 0;
            if (x == 0 or x == img.len - 1 or y == 0 or y == img.len - 1) { // infinite extend
                hood = if (pix == '.') 0 else algo.len - 1;
            } else {
                for (neighbs) |nxy, n| {
                    const nx: isize = @intCast(isize, x) + nxy.x;
                    const ny: isize = @intCast(isize, y) + nxy.y;
                    if (img[@intCast(usize, ny)][@intCast(usize, nx)] == '#') {
                        hood |= @intCast(usize, 1) << @intCast(u6, n);
                    }
                }
            }
            img_new[y][x] = algo[hood];
        }
    }
    img.* = img_new;
}

// return # lit pixels
pub fn lit_count(img: Img) usize {
    var count: usize = 0;
    for (img) |row| {
        count += std.mem.count(u8, row[0..], "#");
    }
    return count;
}

pub fn main() !void {
    var algo: Algo = undefined;
    var img: Img = undefined;
    for (part_steps) |steps, part| {
        try read(&algo, &img);
        var s: usize = steps;
        while (s > 0) : (s -= 1) apply(&algo, &img);
        std.debug.print("Day 20 Part {}: {}\n", .{ part + 1, lit_count(img) });
    }
}
