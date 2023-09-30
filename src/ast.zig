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

pub const Assignment = struct {
    name: token.Token,
    value: *Expr,
};

pub const VariableDeclaration = struct {
    name: token.Token,
    initializer: ?*Expr,
};

pub const Conditional = struct {
    condition: *Expr,
    then: *Stmt,
    els: ?*Stmt,
};

pub const Logical = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const WhileStmt = struct {
    condition: *Expr,
    body: *Stmt,
};

pub const Stmt = union(enum) {
    expr: *Expr,
    cond: Conditional,
    print: *Expr,
    while_: WhileStmt,
    var_decl: VariableDeclaration,
    block: std.ArrayListUnmanaged(Stmt),
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *Expr,
    literal: Literal,
    logical: Logical,
    unary: Unary,
    variable: token.Token,
    assign: Assignment,
};

pub const Ast = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    // Expectation is that all sub things of statements are heap allocated into the arena above.
    statements: std.ArrayListUnmanaged(Stmt),

    pub fn init(allocator: std.mem.Allocator) !Self {
        var result = Ast{
            .arena = try allocator.create(std.heap.ArenaAllocator),
            .statements = std.ArrayListUnmanaged(Stmt){},
        };
        result.arena.* = std.heap.ArenaAllocator.init(allocator);

        return result;
    }

    pub fn deinit(self: Self) void {
        const parent_alloc = self.arena.child_allocator;
        self.arena.deinit();
        parent_alloc.destroy(self.arena);
    }

    // Statement must be allocated with the allocator found in this ast.
    // TODO: is it a more Zig style interface to have an alloc method on the ast instead which gives you a
    //       pointer to the statement you can fill? I honestly am still pretty lost when it comes to managing memory
    //       in Zig programs that are more than basic functions.
    pub fn append(self: *Self, statement: Stmt) !void {
        try self.statements.append(self.arena.allocator(), statement);
    }
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
