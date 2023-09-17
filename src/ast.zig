const std = @import("std");
const token = @import("token.zig");
const parser = @import("parser.zig");

pub const Binary = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: *Expr,
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    bool_: bool,
    nil: void,
};

pub const Ast = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    statements: std.ArrayListUnmanaged(Stmt),

    pub fn deinit(self: Self) void {
        const parent_alloc = self.arena.child_allocator;
        self.arena.deinit();
        parent_alloc.destroy(self.arena);
    }
};

pub const VariableDeclaration = struct {
    name: token.Token,
    initializer: ?*Expr,
};

pub const Stmt = union(enum) {
    expr: *Expr,
    print: *Expr,
    var_decl: VariableDeclaration,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *Expr,
    literal: Literal,
    unary: Unary,
    variable: token.Token,
};

pub fn printExpr(expr: Expr, writer: anytype) void {
    switch (expr) {
        .binary => |b| {
            writer.print("({s} ", .{b.operator.lexeme}) catch {};
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
                    writer.print("\"{s}\"", .{s}) catch {};
                },
                .bool_ => |b| {
                    const text = if (b) "true" else "false";
                    _ = writer.write(text) catch {};
                },
                .nil => {
                    _ = writer.write("nil") catch {};
                },
            }
        },
        .unary => |u| {
            writer.print("({s} ", .{u.operator.lexeme}) catch {};
            printExpr(u.right.*, writer);
            _ = writer.write(")") catch {};
        },
    }
}
