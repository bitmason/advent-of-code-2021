// Advent of Code 2021, Day 22
// https://adventofcode.com/2021/day/22
// Solution by Darren Stone <dstone@bitmason.com>

const std = @import("std");

const input_path = "day-22-input.txt";

const Cuboid = struct { x0: isize, y0: isize, z0: isize, x1: isize, y1: isize, z1: isize };
const Step = struct { on: bool, cuboid: Cuboid };
const StepList = std.ArrayList(Step);

/// Read input, filling pre-initialized StepList.
fn read(steps: *StepList) !void {
    var buf: [80]u8 = undefined;
    var file = try std.fs.cwd().openFile(input_path, .{ .read = true });
    while (try file.reader().readUntilDelimiterOrEof(buf[0..], '\n')) |line| {
        const x0_0 = std.mem.indexOf(u8, line[0..], "=").? + 1;
        const x0_1 = x0_0 + std.mem.indexOf(u8, line[x0_0..], ".").?;
        const x1_0 = x0_1 + 2;
        const x1_1 = x1_0 + std.mem.indexOf(u8, line[x1_0..], ",").?;
        const y0_0 = x1_1 + 3;
        const y0_1 = y0_0 + std.mem.indexOf(u8, line[y0_0..], ".").?;
        const y1_0 = y0_1 + 2;
        const y1_1 = y1_0 + std.mem.indexOf(u8, line[y1_0..], ",").?;
        const z0_0 = y1_1 + 3;
        const z0_1 = z0_0 + std.mem.indexOf(u8, line[z0_0..], ".").?;
        const z1_0 = z0_1 + 2;
        try steps.append(.{ .on = line[1] == 'n', .cuboid = .{
            .x0 = try std.fmt.parseInt(isize, line[x0_0..x0_1], 10),
            .x1 = try std.fmt.parseInt(isize, line[x1_0..x1_1], 10),
            .y0 = try std.fmt.parseInt(isize, line[y0_0..y0_1], 10),
            .y1 = try std.fmt.parseInt(isize, line[y1_0..y1_1], 10),
            .z0 = try std.fmt.parseInt(isize, line[z0_0..z0_1], 10),
            .z1 = try std.fmt.parseInt(isize, line[z1_0..], 10),
        } });
    }
}

/// Apply step to cuboid in given state, returning new state.
fn execute(x: isize, y: isize, z: isize, on: bool, step: Step) bool {
    if (contains(step.cuboid, x, y, z)) {
        return step.on;
    } else {
        return on;
    }
}

/// For each init region cube, apply each step, return new state.
fn part_1(steps: StepList) usize {
    var count: usize = 0;
    var x: isize = -50;
    while (x <= 50) : (x += 1) {
        var y: isize = -50;
        while (y <= 50) : (y += 1) {
            var z: isize = -50;
            while (z <= 50) : (z += 1) {
                var on: bool = false; // start OFF
                for (steps.items) |s| {
                    on = execute(x, y, z, on, s); // apply each step to each Cubiod
                }
                if (on) count += 1;
            }
        }
    }
    return count;
}

/// Return true iff cuboid contains given cube.
fn contains(cuboid: Cuboid, x: isize, y: isize, z: isize) bool {
    return x >= cuboid.x0 and x <= cuboid.x1 and
        y >= cuboid.y0 and y <= cuboid.y1 and
        z >= cuboid.z0 and z <= cuboid.z1;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    var alloc = gpa.allocator();
    var steps = StepList.init(alloc);
    defer steps.deinit();
    try read(&steps);
    std.debug.print("Day 22 Part 1: {}\n", .{part_1(steps)});
}
