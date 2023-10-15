const std = @import("std");
const token = @import("token.zig");
const parser = @import("parser.zig");
const Allocator = std.mem.Allocator;

pub const Binary = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,

    fn equals(self: Binary, other: Binary) bool {
        return self.left.equals(other.left.*) and self.operator.type_ == other.operator.type_ and self.right.equals(other.right.*);
    }

    fn clone(self: Binary, allocator: Allocator) Allocator.Error!Binary {
        const new_left = try allocator.create(Expr);
        new_left.* = try self.left.clone(allocator);
        const new_right = try allocator.create(Expr);
        new_right.* = try self.right.clone(allocator);
        return Binary{
            .left = new_left,
            .operator = try self.operator.clone(allocator),
            .right = new_right,
        };
    }
};

pub const Unary = struct {
    operator: token.Token,
    right: *Expr,

    fn equals(self: Unary, other: Unary) bool {
        return self.operator.type_ == other.operator.type_ and self.right.equals(other.right.*);
    }

    fn clone(self: Unary, allocator: Allocator) Allocator.Error!Unary {
        const new_right = try allocator.create(Expr);
        new_right.* = try self.right.clone(allocator);
        return Unary{
            .operator = try self.operator.clone(allocator),
            .right = new_right,
        };
    }
};

pub const Function = struct {
    name: token.Token,
    params: std.ArrayListUnmanaged(token.Token),
    body: std.ArrayListUnmanaged(Stmt),

    fn equals(self: Function, other: Function) bool {
        if (self.params.items.len != other.params.items.len) {
            return false;
        }
        if (self.body.items.len != other.body.items.len) {
            return false;
        }
        for (self.params.items, other.params.items) |s, o| {
            if (!std.mem.eql(u8, s.lexeme, o.lexeme)) {
                return false;
            }
        }
        for (self.body.items, other.body.items) |s, o| {
            if (!s.equals(o)) {
                return false;
            }
        }
        return std.mem.eql(u8, self.name.lexeme, other.name.lexeme);
    }

    fn clone(self: Function, allocator: Allocator) Allocator.Error!Function {
        var new_params = try self.params.clone(allocator);
        for (new_params.items, self.params.items) |*new, old| {
            new.* = try old.clone(allocator);
        }
        var new_body = try self.body.clone(allocator);
        for (new_body.items, self.body.items) |*new, old| {
            new.* = try old.clone(allocator);
        }

        return Function{
            .name = try self.name.clone(allocator),
            .params = new_params,
            .body = new_body,
        };
    }
};

pub const Call = struct {
    callee: *Expr,
    closing_paren: token.Token, // Used for error reporting line numbers.
    arguments: std.ArrayListUnmanaged(Expr),

    fn equals(self: Call, other: Call) bool {
        if (self.arguments.items.len != other.arguments.items.len) {
            return false;
        }
        for (self.arguments.items, other.arguments.items) |s, o| {
            if (!s.equals(o)) {
                return false;
            }
        }
        return self.callee.equals(other.callee.*) and self.closing_paren.type_ == other.closing_paren.type_;
    }

    fn clone(self: Call, allocator: Allocator) Allocator.Error!Call {
        const new_callee = try allocator.create(Expr);
        new_callee.* = try self.callee.clone(allocator);
        var new_arguments = try self.arguments.clone(allocator);
        for (new_arguments.items, self.arguments.items) |*new, old| {
            new.* = try old.clone(allocator);
        }
        return Call{
            .callee = new_callee,
            .closing_paren = try self.closing_paren.clone(allocator),
            .arguments = new_arguments,
        };
    }
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8,
    bool_: bool,
    nil: void,

    fn equals(self: Literal, other: Literal) bool {
        return switch (self) {
            .number => |n| switch (other) {
                .number => |n2| return n == n2,
                else => return false,
            },
            .bool_ => |b| switch (other) {
                .bool_ => |b2| return b == b2,
                else => return false,
            },
            .nil => switch (other) {
                .nil => return true,
                else => return false,
            },
            .string => |s| switch (other) {
                .string => |s2| return std.mem.eql(u8, s, s2),
                else => return false,
            },
        };
    }

    fn clone(self: Literal, allocator: Allocator) Allocator.Error!Literal {
        return switch (self) {
            .string => |s| Literal{ .string = try allocator.dupe(u8, s) },
            else => self,
        };
    }
};

pub const Assignment = struct {
    name: token.Token,
    value: *Expr,

    fn equals(self: Assignment, other: Assignment) bool {
        return std.mem.eql(u8, self.name.lexeme, other.name.lexeme) and self.value.equals(other.value.*);
    }

    fn clone(self: Assignment, allocator: Allocator) Allocator.Error!Assignment {
        const new_value = try allocator.create(Expr);
        new_value.* = try self.value.clone(allocator);
        return Assignment{
            .name = try self.name.clone(allocator),
            .value = new_value,
        };
    }
};

pub const VariableDeclaration = struct {
    name: token.Token,
    initializer: ?Expr,

    fn equals(self: VariableDeclaration, other: VariableDeclaration) bool {
        if (self.initializer) |sint| {
            const oint = other.initializer orelse return false;
            if (!sint.equals(oint)) {
                return false;
            }
        }
        if (self.initializer == null and other.initializer != null) {
            return false;
        }

        return std.mem.eql(u8, self.name.lexeme, other.name.lexeme);
    }

    fn clone(self: VariableDeclaration, allocator: Allocator) Allocator.Error!VariableDeclaration {
        var new_initializer: ?Expr = null;
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
    condition: Expr,
    then: *Stmt,
    els: ?*Stmt,

    fn equals(self: Conditional, other: Conditional) bool {
        // TODO can we make this check for two optionals more succinct?
        if (self.els) |sels| {
            const oels = other.els orelse return false;
            if (!sels.equals(oels.*)) {
                return false;
            }
        }
        if (self.els == null and other.els != null) {
            return false;
        }

        return self.condition.equals(other.condition) and self.then.equals(other.then.*);
    }

    fn clone(self: Conditional, allocator: Allocator) Allocator.Error!Conditional {
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

    fn equals(self: Logical, other: Logical) bool {
        return self.left.equals(other.left.*) and self.operator.type_ == other.operator.type_ and self.right.equals(other.right.*);
    }

    fn clone(self: Logical, allocator: Allocator) Allocator.Error!Logical {
        const new_left = try allocator.create(Expr);
        new_left.* = try self.left.clone(allocator);
        const new_right = try allocator.create(Expr);
        new_right.* = try self.right.clone(allocator);
        return Logical{
            .left = new_left,
            .operator = try self.operator.clone(allocator),
            .right = new_right,
        };
    }
};

pub const WhileStmt = struct {
    condition: Expr,
    body: *Stmt,

    fn equals(self: WhileStmt, other: WhileStmt) bool {
        return self.condition.equals(other.condition) and self.body.equals(other.body.*);
    }

    fn clone(self: WhileStmt, allocator: Allocator) Allocator.Error!WhileStmt {
        const new_body = try allocator.create(Stmt);
        new_body.* = try self.body.clone(allocator);
        return WhileStmt{ .condition = try self.condition.clone(allocator), .body = new_body };
    }
};

pub const Return = struct {
    keyword: token.Token, // Used only for line number reporting
    value: ?Expr,

    fn equals(self: Return, other: Return) bool {
        if (self.value) |sval| {
            const oval = other.value orelse return false;
            return sval.equals(oval);
        }
        return self.value == null and other.value == null;
    }

    fn clone(self: Return, allocator: Allocator) Allocator.Error!Return {
        var new_val: ?Expr = null;
        if (self.value) |val| {
            new_val = try val.clone(allocator);
        }
        return Return{
            .keyword = try self.keyword.clone(allocator),
            .value = new_val,
        };
    }
};

pub const Stmt = union(enum) {
    expr: Expr,
    cond: Conditional,
    print: Expr,
    while_: WhileStmt,
    var_decl: VariableDeclaration,
    function: Function,
    ret: Return,
    block: std.ArrayListUnmanaged(Stmt),

    fn equals(self: Stmt, other: Stmt) bool {
        return switch (self) {
            .block => |b| {
                switch (other) {
                    .block => |other_b| {
                        if (b.items.len != other_b.items.len) {
                            return false;
                        }
                        for (b.items, other_b.items) |statement, other_statement| {
                            if (!statement.equals(other_statement)) {
                                return false;
                            }
                        }
                        return true;
                    },
                    else => return false,
                }
            },
            inline else => |s, tag| switch (other) {
                tag => |o| return s.equals(o),
                else => return false,
            },
        };
    }

    fn clone(self: Stmt, allocator: Allocator) Allocator.Error!Stmt {
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
            .function => |inner| Stmt{ .function = try inner.clone(allocator) },
            .ret => |inner| Stmt{ .ret = try inner.clone(allocator) },
        };
    }
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *Expr,
    literal: Literal,
    logical: Logical,
    unary: Unary,
    call: Call,
    variable: token.Token,
    assign: Assignment,

    fn equals(self: Expr, other: Expr) bool {
        return switch (self) {
            .grouping => |g| switch (other) {
                .grouping => |og| return g.equals(og.*),
                else => return false,
            },
            .variable => |v| switch (other) {
                .variable => |ov| return std.mem.eql(u8, v.lexeme, ov.lexeme),
                else => return false,
            },
            inline else => |s, tag| switch (other) {
                tag => |o| return s.equals(o),
                else => return false,
            },
        };
    }

    fn clone(self: Expr, allocator: Allocator) Allocator.Error!Expr {
        return switch (self) {
            .binary => |inner| Expr{ .binary = try inner.clone(allocator) },
            .grouping => |inner| {
                const new_group = try allocator.create(Expr);
                new_group.* = try inner.clone(allocator);
                return Expr{ .grouping = new_group };
            },
            .literal => |inner| Expr{ .literal = try inner.clone(allocator) },
            .logical => |inner| Expr{ .logical = try inner.clone(allocator) },
            .unary => |inner| Expr{ .unary = try inner.clone(allocator) },
            .variable => |inner| Expr{ .variable = try inner.clone(allocator) },
            .assign => |inner| Expr{ .assign = try inner.clone(allocator) },
            .call => |inner| Expr{ .call = try inner.clone(allocator) },
        };
    }
};

pub const Ast = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    // Expectation is that all sub things of statements are heap allocated into the arena above.
    statements: std.ArrayListUnmanaged(Stmt),

    pub fn from(function: Function, allocator: Allocator) !Self {
        var new_ast = try init(allocator);
        errdefer new_ast.deinit();

        const function_copy = try function.clone(new_ast.arena.allocator());
        try new_ast.append(Stmt{ .function = function_copy });
        return new_ast;
    }

    pub fn equals(self: Self, other: Self) bool {
        if (self.statements.items.len != other.statements.items.len) {
            return false;
        }
        for (self.statements.items, other.statements.items) |own_statement, other_statement| {
            if (!own_statement.equals(other_statement)) {
                return false;
            }
        }
        return true;
    }

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

    pub fn deinit(self: *Self) void {
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
