const std = @import("std");
const token = @import("token.zig");

pub const Binary = struct {
    left: *const Expr,
    operator: token.Token,
    right: *const Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: *const Expr,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *const Expr,
    literal: token.Literal,
    unary: Unary,
};

pub fn printExpr(ast: Expr, writer: anytype) void {
    switch (ast) {
        .binary => |b| {
            writer.print("({s} ", .{b.operator.lexeme.?}) catch {};
            printExpr(b.left.*, writer);
            _ = writer.write(" ") catch {};
            printExpr(b.right.*, writer);
            _ = writer.write(")") catch {};
        },
        .grouping => |g| {
            _ = writer.write("(group ") catch {};
            printExpr(g.*, writer);
            _ = writer.write(")") catch {};
        },
        .literal => |l| {
            switch (l) {
                .number => |n| {
                    writer.print("{d}", .{n}) catch {};
                },
                .string => |s| {
                    writer.print("{s}", .{s}) catch {};
                },
            }
        },
        .unary => |u| {
            writer.print("({s} ", .{u.operator.lexeme.?}) catch {};
            printExpr(u.right.*, writer);
            _ = writer.write(")") catch {};
        },
    }
}
