const std = @import("std");
const builtin = @import("builtin");
const interpreter = @import("interpreter.zig");

const scratch_size = 100 * 1024 * 1024; // 100 MiB of scratch for input and AST.

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

    const input_buffer = try allocator.alloc(u8, scratch_size);
    defer allocator.free(input_buffer);
    var input_scratch = std.heap.FixedBufferAllocator.init(input_buffer);
    const input_allocator = input_scratch.allocator();

    var interpret = try interpreter.Interpreter.new(allocator);
    defer interpret.deinit();

    while (true) {
        var buffer = std.ArrayList(u8).init(input_allocator);
        _ = try stdout.write("> ");
        try stdin.streamUntilDelimiter(buffer.writer(), '\n', null); //Does not contain the \n!
        if (builtin.os.tag == .windows) {
            _ = buffer.pop();
        }
        if (buffer.items.len == 0) break;
        // In interactive mode we don't want to bring the whole program down
        // just because the user mistyped something.
        interpret.run(stdout.writer(), buffer.items, input_allocator) catch {};
    }
}

fn runFile(path: [:0]const u8, allocator: std.mem.Allocator) !void {
    const input_buffer = try allocator.alloc(u8, scratch_size);
    defer allocator.free(input_buffer);
    var input_scratch = std.heap.FixedBufferAllocator.init(input_buffer);
    const input_allocator = input_scratch.allocator();

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const bytes = try file.readToEndAlloc(input_allocator, std.math.maxInt(usize));

    var interpret = try interpreter.Interpreter.new(allocator);
    defer interpret.deinit();

    try interpret.run(std.io.getStdOut().writer(), bytes, input_allocator);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var args = try std.process.argsWithAllocator(gpa.allocator());
    defer args.deinit();

    //skip over our own program name.
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
