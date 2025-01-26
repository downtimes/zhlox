const std = @import("std");
const ast = @import("ast.zig");
const main = @import("main.zig");
const interpreter = @import("interpreter.zig");

// Has to work in tandem with the creation of environments in the interpreter.
// For each new environment in the interpreter we create a nested scope here as
// well. Then we record how many hops we needed to resolve the variable name
// and give this precalculated information to the interpreter. When the
// interpreter tries to resolve a name, it will henceforth jump to the environment
// that many levels up to resolve the variable.
//
// The step is introduced to get static scoping of variables. (see scripts/closure_capture.lox)
pub const Resolver = struct {
    const Self = @This();

    // Probably arena?
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(std.StringArrayHashMap(bool)),
    interp: *interpreter.Interpreter,
    found_error: bool,

    pub fn init(arena: *std.heap.ArenaAllocator, interp: *interpreter.Interpreter) Self {
        return Resolver{
            .allocator = arena.allocator(),
            .scopes = std.ArrayList(std.StringArrayHashMap(bool)).init(arena.allocator()),
            .interp = interp,
            .found_error = false,
        };
    }

    pub fn resolve(self: *Self, stmts: []ast.Stmt) std.mem.Allocator.Error!void {
        for (stmts) |stmt| {
            try self.resolveStmt(stmt);
        }
    }

    fn scope_depth(self: Self) usize {
        return self.scopes.items.len;
    }

    fn resolveStmt(self: *Self, stmt: ast.Stmt) !void {
        switch (stmt) {
            .block => |b| {
                try self.begin_scope();
                for (b.items) |s| {
                    try self.resolveStmt(s);
                }
                self.end_scope();
            },
            .var_decl => |vd| {
                try self.declare(vd.name.lexeme);
                if (vd.initializer) |i| {
                    try self.resolveExpr(i);
                }
                try self.define(vd.name.lexeme);
            },
            .function => |f| {
                try self.declare(f.name.lexeme);
                try self.define(f.name.lexeme);
                try self.resolveFunction(f);
            },
            .expr => |e| {
                try self.resolveExpr(e);
            },
            .cond => |c| {
                try self.resolveExpr(c.condition);
                try self.resolveStmt(c.then.*);
                if (c.els) |e| {
                    try self.resolveStmt(e.*);
                }
            },
            .print => |p| {
                try self.resolveExpr(p);
            },
            .ret => |r| {
                if (r.value) |v| {
                    try self.resolveExpr(v);
                }
            },
            .while_ => |w| {
                try self.resolveExpr(w.condition);
                try self.resolve(w.body.items);
            },
        }
    }

    fn resolveFunction(self: *Self, function: ast.Function) std.mem.Allocator.Error!void {
        try self.begin_scope();
        for (function.params.items) |param| {
            try self.declare(param.lexeme);
            try self.define(param.lexeme);
        }
        try self.resolve(function.body.items);
        self.end_scope();
    }

    fn resolveExpr(self: *Self, expr: ast.Expr) !void {
        switch (expr) {
            .variable => |v| {
                const scope = self.scopes.getLastOrNull();
                if (scope) |s| {
                    if (s.get(v.lexeme)) |assign_finished| {
                        if (!assign_finished) {
                            main.reportError(
                                v.line,
                                &[_][]const u8{
                                    "Resolver Error: '",
                                    v.lexeme,
                                    "' ",
                                    "Can't read local variable in its own initializer.",
                                },
                            );
                            self.found_error = true;
                        }
                    }
                    try self.resolveLocal(expr, v.lexeme);
                }
            },
            .assign => |a| {
                try self.resolveExpr(a.value.*);
                try self.resolveLocal(expr, a.name.lexeme);
            },
            .binary => |b| {
                try self.resolveExpr(b.left.*);
                try self.resolveExpr(b.right.*);
            },
            .call => |c| {
                try self.resolveExpr(c.callee.*);

                for (c.arguments.items) |arg| {
                    try self.resolveExpr(arg);
                }
            },
            .grouping => |g| {
                try self.resolveExpr(g.*);
            },
            .logical => |l| {
                try self.resolveExpr(l.left.*);
                try self.resolveExpr(l.right.*);
            },
            .unary => |u| {
                try self.resolveExpr(u.right.*);
            },
            else => {},
        }
    }

    fn resolveLocal(self: Self, expr: ast.Expr, name: []const u8) !void {
        // Walk through the scopes from back to front and see if we can resolve it.
        const len = self.scopes.items.len;
        for (1..len) |idx| {
            const scope = self.scopes.items[len - idx];
            if (scope.contains(name)) {
                // record number of hops to help interpreter to resolve value
                // correctly
                try self.interp.addResolved(expr, idx - 1);
                return;
            }
        }
    }

    fn declare(self: Self, name: []const u8) !void {
        if (self.scope_depth() == 0) return;

        const scope: *std.StringArrayHashMap(bool) = &self.scopes.items[self.scope_depth() - 1];
        try scope.put(name, false);
    }

    fn define(self: Self, name: []const u8) !void {
        if (self.scopes.items.len == 0) return;

        const scope: *std.StringArrayHashMap(bool) = &self.scopes.items[self.scope_depth() - 1];
        try scope.put(name, true);
    }

    fn begin_scope(self: *Self) !void {
        const map = std.StringArrayHashMap(bool).init(self.allocator);
        try self.scopes.append(map);
    }

    fn end_scope(self: *Self) void {
        var popped = self.scopes.pop();
        popped.deinit();
    }
};
