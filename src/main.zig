const std = @import("std");
const builtin = @import("builtin");
const scanner = @import("scanner.zig");
const token = @import("token.zig");

fn run(stdout: std.fs.File.Writer, input: []const u8, allocator: std.mem.Allocator) !void {
    const lexer = scanner.Scanner{ .input = input, .allocator = allocator };
    const tokens = lexer.scanTokens();
    defer tokens.deinit();

    for (tokens.items) |t| {
        try stdout.print("{any}", .{t});
    }
}

fn runRepl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut();
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    while (true) {
        _ = try stdout.write("> ");
        buffer.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null); //Does not contain the \n!
        if (builtin.os.tag == .windows) {
            _ = buffer.pop();
        }
        if (buffer.items.len == 0) break;
        try run(stdout.writer(), buffer.items, allocator);
    }
}

fn runFile(path: [:0]const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    try run(std.io.getStdOut().writer(), bytes, allocator);
}

pub fn main() !void {
    var gba = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gba.deinit();
    var args = try std.process.argsWithAllocator(gba.allocator());
    defer args.deinit();

    //skip over our own programm name.
    _ = args.skip();

    const path = args.next();
    if (path == null) {
        try runRepl(gba.allocator());
    } else if (args.next() != null) {
        _ = try std.io.getStdOut().write("Usage: zhlox [script]");
    } else {
        try runFile(path.?, gba.allocator());
    }
}
