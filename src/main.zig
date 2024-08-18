const std = @import("std");
const builtin = @import("builtin");
const interpreter = @import("interpreter.zig");

pub fn reportError(line: u32, messages: []const []const u8) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[line {d}] ", .{line}) catch {};
    for (messages) |message| {
        stderr.print("{s}", .{message}) catch {};
    }
    _ = stderr.write("\n") catch {};
}

fn runRepl(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut();
    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var interpret = try interpreter.Interpreter.new(allocator);
    defer interpret.deinit();

    while (true) {
        _ = try stdout.write("> ");
        buffer.clearRetainingCapacity();
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null); //Does not contain the \n!
        if (builtin.os.tag == .windows) {
            _ = buffer.pop();
        }
        if (buffer.items.len == 0) break;
        // In interactive mode we don't want to bring the whole programm down just because the user mistyped something.
        interpret.run(stdout.writer(), buffer.items) catch {};
    }
}

fn runFile(path: [:0]const u8, allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const bytes = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(bytes);

    var interpret = try interpreter.Interpreter.new(allocator);
    defer interpret.deinit();

    try interpret.run(std.io.getStdOut().writer(), bytes);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();

    //skip over our own programm name.
    _ = args.skip();

    const path = args.next();
    if (path == null) {
        try runRepl(gpa.allocator());
    } else if (args.next() != null) {
        _ = try std.io.getStdOut().write("Usage: zhlox [script]");
    } else {
        try runFile(path.?, gpa.allocator());
    }
}
