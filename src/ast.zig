const std = @import("std");
const token = @import("token.zig");
const parser = @import("parser.zig");
const Allocator = std.mem.Allocator;

pub const Binary = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,

    pub fn clone(self: Binary, allocator: Allocator) Allocator.Error!Binary {
        return Binary{
            .left = try self.left.clone(allocator),
            .operator = try self.operator.clone(allocator),
            .right = try self.right.clone(allocator),
        };
    }
};

pub const Unary = struct {
    operator: token.Token,
    right: *Expr,

    pub fn clone(self: Unary, allocator: Allocator) Allocator.Error!Unary {
        return Unary{
            .operator = try self.operator.clone(allocator),
            .right = try self.right.clone(allocator),
        };
    }
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    bool_: bool,
    nil: void,

    pub fn clone(self: Literal, allocator: Allocator) Allocator.Error!Literal {
        return switch (self) {
            .string => |s| Literal{ .string = try allocator.dupe(u8, s) },
            else => self,
        };
    }
};

pub const Assignment = struct {
    name: token.Token,
    value: *Expr,

    pub fn clone(self: Assignment, allocator: Allocator) Allocator.Error!Assignment {
        return Assignment{
            .name = try self.name.clone(allocator),
            .value = try self.value.clone(allocator),
        };
    }
};

pub const VariableDeclaration = struct {
    name: token.Token,
    initializer: ?*Expr,

    pub fn clone(self: VariableDeclaration, allocator: Allocator) Allocator.Error!VariableDeclaration {
        var new_initializer: ?*Expr = null;
        if (self.initializer) |initializer| {
            new_initializer = try initializer.clone(allocator);
        }
        return VariableDeclaration{
            .name = try self.name.clone(allocator),
            .initializer = new_initializer,
        };
    }
};

pub const Conditional = struct {
    condition: *Expr,
    then: *Stmt,
    els: ?*Stmt,

    pub fn clone(self: Conditional, allocator: Allocator) Allocator.Error!Conditional {
        const new_then = try allocator.create(Stmt);
        new_then.* = try self.then.clone(allocator);
        var new_els: ?*Stmt = null;
        if (self.els) |els| {
            new_els = try allocator.create(Stmt);
            new_els.?.* = try els.clone(allocator);
        }
        return Conditional{
            .condition = try self.condition.clone(allocator),
            .then = new_then,
            .els = new_els,
        };
    }
};

pub const Logical = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,

    pub fn clone(self: Logical, allocator: Allocator) Allocator.Error!Logical {
        return Logical{
            .left = try self.left.clone(allocator),
            .operator = try self.operator.clone(allocator),
            .right = try self.right.clone(allocator),
        };
    }
};

pub const WhileStmt = struct {
    condition: *Expr,
    body: *Stmt,

    pub fn clone(self: WhileStmt, allocator: Allocator) Allocator.Error!WhileStmt {
        const new_body = try allocator.create(Stmt);
        new_body.* = try self.body.clone(allocator);
        return WhileStmt{ .condition = try self.condition.clone(allocator), .body = new_body };
    }
};

pub const Stmt = union(enum) {
    expr: *Expr,
    cond: Conditional,
    print: *Expr,
    while_: WhileStmt,
    var_decl: VariableDeclaration,
    block: std.ArrayListUnmanaged(Stmt),

    pub fn clone(self: Stmt, allocator: Allocator) Allocator.Error!Stmt {
        return switch (self) {
            .expr => |inner| Stmt{ .expr = try inner.clone(allocator) },
            .cond => |inner| Stmt{ .cond = try inner.clone(allocator) },
            .print => |inner| Stmt{ .print = try inner.clone(allocator) },
            .while_ => |inner| Stmt{ .while_ = try inner.clone(allocator) },
            .var_decl => |inner| Stmt{ .var_decl = try inner.clone(allocator) },
            .block => |inner| blk: {
                var new_block = try inner.clone(allocator);
                for (new_block.items, inner.items) |*new_item, old_item| {
                    new_item.* = try old_item.clone(allocator);
                }
                break :blk Stmt{ .block = new_block };
            },
        };
    }
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *Expr,
    literal: Literal,
    logical: Logical,
    unary: Unary,
    variable: token.Token,
    assign: Assignment,

    pub fn clone(self: Expr, allocator: Allocator) Allocator.Error!*Expr {
        var new_expr = try allocator.create(Expr);
        new_expr.* = switch (self) {
            .binary => |inner| Expr{ .binary = try inner.clone(allocator) },
            .grouping => |inner| Expr{ .grouping = try inner.clone(allocator) },
            .literal => |inner| Expr{ .literal = try inner.clone(allocator) },
            .logical => |inner| Expr{ .logical = try inner.clone(allocator) },
            .unary => |inner| Expr{ .unary = try inner.clone(allocator) },
            .variable => |inner| Expr{ .variable = try inner.clone(allocator) },
            .assign => |inner| Expr{ .assign = try inner.clone(allocator) },
        };
        return new_expr;
    }
};

pub const Ast = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    // Expectation is that all sub things of statements are heap allocated into the arena above.
    statements: std.ArrayListUnmanaged(Stmt),

    pub fn init(allocator: Allocator) !Self {
        var result = Ast{
            .arena = try allocator.create(std.heap.ArenaAllocator),
            .statements = std.ArrayListUnmanaged(Stmt){},
        };
        result.arena.* = std.heap.ArenaAllocator.init(allocator);

        return result;
    }

    pub fn clone(self: Self, allocator: Allocator) !Self {
        var new_ast = try Ast.init(allocator);
        errdefer new_ast.deinit();
        new_ast.statements = try self.statements.clone(new_ast.arena.allocator());
        for (new_ast.statements.items, self.statements.items) |*new_item, old_item| {
            new_item.* = try old_item.clone(new_ast.arena.allocator());
        }
        return new_ast;
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
