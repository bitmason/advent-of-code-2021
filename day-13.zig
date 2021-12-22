// Advent of Code 2021, Day 13
// https://adventofcode.com/2021/day/13
// Solution by Darren Stone <dstone@bitmason.com>

const std = @import("std");

const input_path = "day-13-input.txt";

const Dot = struct { x: usize, y: usize };
const DotList = std.ArrayList(Dot);

const Fold = struct { x: ?usize, y: ?usize };
const FoldList = std.ArrayList(Fold);

/// Read input, filling pre-initialized dot and fold lists.
pub fn read(dots: *DotList, folds: *FoldList) !void {
    var buf: [80]u8 = undefined;
    var file = try std.fs.cwd().openFile(input_path, .{ .read = true });
    while (try file.reader().readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
        if (std.mem.indexOf(u8, line[0..], ",")) |pos| { // dot
            try dots.append(.{
                .x = try std.fmt.parseUnsigned(usize, line[0..pos], 10),
                .y = try std.fmt.parseUnsigned(usize, line[pos + 1 ..], 10),
            });
        } else if (std.mem.indexOf(u8, line[0..], "=")) |pos| { // fold
            const axis: u8 = line[pos - 1];
            const val: usize = try std.fmt.parseUnsigned(usize, line[pos + 1 ..], 10);
            try folds.append(.{
                .x = if (axis == 'x') val else null,
                .y = if (axis == 'y') val else null,
            });
        }
    }
}

/// Apply given Fold, modifying DotList.
pub fn apply(dots: *DotList, fold: Fold) void {
    if (fold.x) |x| { // fold to the left
        for (dots.items) |*d| {
            if (d.x > x) d.x = x - (d.x - x);
        }
    } else if (fold.y) |y| { // fold up
        for (dots.items) |*d| {
            if (d.y > y) d.y = y - (d.y - y);
        }
    }
    dedupe(dots);
}

/// Remove duplicate dots.
pub fn dedupe(dots: *DotList) void {
    outer: while (dots.items.len > 0) {
        for (dots.items[0 .. dots.items.len - 1]) |d1, i| {
            for (dots.items[i + 1 ..]) |d2| {
                if (d1.x == d2.x and d1.y == d2.y) {
                    _ = dots.swapRemove(i);
                    continue :outer;
                }
            }
        }
        break;
    }
}

/// Print dots (2D grid, origin @ upper left).
pub fn print(dots: DotList, x_min: usize, y_min: usize) void {
    var y: usize = 0;
    while (y < y_min) : (y += 1) {
        var x: usize = 0;
        x_loop: while (x < x_min) : (x += 1) {
            for (dots.items) |d| {
                if (d.x == x and d.y == y) {
                    std.debug.print("#", .{});
                    continue :x_loop;
                }
            }
            std.debug.print(" ", .{});
        }
        std.debug.print("\n", .{});
    }
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var dots = DotList.init(alloc);
    defer dots.deinit();
    var folds = FoldList.init(alloc);
    defer folds.deinit();
    try read(&dots, &folds);
    apply(&dots, folds.items[0]); // first instruction
    std.debug.print("Day 13 Part 1: {}\n", .{dots.items.len});
    for (folds.items[1..]) |f| { // rest of instructions
        apply(&dots, f);
    }
    std.debug.print("Day 13 Part 2:\n", .{});
    // calc bounding box based on final paper size
    var x_min: usize = std.math.maxInt(usize);
    var y_min: usize = std.math.maxInt(usize);
    for (folds.items) |f| {
        if (f.x) |x| x_min = @minimum(x_min, x);
        if (f.y) |y| y_min = @minimum(y_min, y);
    }
    print(dots, x_min, y_min);
}
