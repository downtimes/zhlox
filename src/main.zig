const std = @import("std");
const builtin = @import("builtin");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const parser = @import("parser.zig");
const interpreter = @import("interpreter.zig");

pub fn reportError(line: u32, messages: []const []const u8) void {
    const stderr = std.io.getStdErr().writer();
    stderr.print("[line {d}] Error: ", .{line}) catch {};
    for (messages) |message| {
        stderr.print("{s}", .{message}) catch {};
    }
    _ = stderr.write("\n") catch {};
}

fn run(stdout: std.fs.File.Writer, input: []const u8, allocator: std.mem.Allocator) !void {
    var lexer = scanner.Scanner{ .source = input, .allocator = allocator };
    const tokens = try lexer.scanTokens();
    defer tokens.deinit();

    var p = parser.Parser{ .tokens = tokens.items, .allocator = allocator };
    const expr = p.expression() catch {
        const diagnostic = p.diagnostic.?;
        reportError(diagnostic.found.line, &[_][]const u8{ "found ", diagnostic.found.lexeme, "; ", diagnostic.message });
        return;
    };
    defer expr.destroySelf(allocator);
    var interpret = interpreter.Interpreter{};

    const value = interpret.execute(expr.*) catch |err| {
        if (err == interpreter.RuntimError.Unimplemented) {
            _ = try stdout.write("Hit unimplemented part of the interpreter.");
        } else {
            const diagnostic = interpret.diagnostic.?;
            reportError(diagnostic.token_.line, &[_][]const u8{ diagnostic.token_.lexeme, " ", diagnostic.message });
        }
        return;
    };

    switch (value) {
        .number => |n| try stdout.print("{d}", .{n}),
        .string => |s| try stdout.print("{s}", .{s}),
        .bool_ => |b| try stdout.print("{}", .{b}),
        .nil => _ = try stdout.write("nil"),
    }

    _ = try stdout.write("\n");
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
