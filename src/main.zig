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
        var buffer: std.ArrayListUnmanaged(u8) = .empty;
        _ = try stdout.write("> ");
        try stdin.streamUntilDelimiter(buffer.writer(input_allocator), '\n', null); //Does not contain the \n!
        if (builtin.os.tag == .windows) {
            _ = buffer.pop(); // Pop the \r on windows.
        }
        if (buffer.items.len == 0) break;
        interpret.run(std.io.getStdOut().writer(), buffer.items, input_allocator) catch {
            const stderr = std.io.getStdErr().writer();
            stderr.print("The interpreter ran out of memory.", .{}) catch {};
            return;
        };
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

    interpret.run(std.io.getStdOut().writer(), bytes, input_allocator) catch {
        const stderr = std.io.getStdErr().writer();
        stderr.print("The interpreter ran out of memory.", .{}) catch {};
    };
}

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
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
