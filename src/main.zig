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

// TODO probably move this function to the interpreter later so that the interpreters state can be
//      persistet easily after each run command. Especially important for the REPL.
fn run(stdout: std.fs.File.Writer, input: []const u8, allocator: std.mem.Allocator) !void {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var scan = scanner.Scanner{ .source = input };
    const tokens = try scan.scanTokens(arena.allocator());

    var parse = parser.Parser{ .tokens = tokens.items };
    const parse_tree = parse.parseInto(arena.allocator()) catch {
        const diagnostic = parse.diagnostic.?;
        reportError(diagnostic.found.line, &[_][]const u8{ "found ", diagnostic.found.lexeme, "; ", diagnostic.message });
        return;
    };
    var interpret = interpreter.Interpreter{ .allocator = allocator };

    var value = interpret.execute(parse_tree.expr) catch |err| {
        if (err == interpreter.RuntimError.Unimplemented) {
            _ = try stdout.write("Hit unimplemented part of the interpreter.");
        } else {
            const diagnostic = interpret.diagnostic.?;
            reportError(diagnostic.token_.line, &[_][]const u8{ diagnostic.token_.lexeme, " ", diagnostic.message });
        }
        // TODO possibly return an error value here to set a nonzero exit code when main exits to tell script
        //      users in their shell that something went wrong.
        return;
    };
    defer value.deinit(allocator);

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
