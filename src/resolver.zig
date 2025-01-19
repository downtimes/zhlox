const std = @import("std");
const ast = @import("ast.zig");
const interpreter = @import("interpreter.zig");

pub const Resolver = struct {
    const Self = @This();

    // Probably arena?
    allocator: std.mem.Allocator,
    scopes: std.ArrayList(std.StringArrayHashMap(bool)),
    interp: *interpreter.Interpreter,

    pub fn init(arena: std.mem.Allocator, interp: *interpreter.Interpreter) Self {
        return Resolver{
            .allocator = arena,
            .scopes = std.ArrayList(std.StringArrayHashMap(bool)).init(arena),
            .interp = interp,
        };
    }

    pub fn resolve(self: *Self, stmts: []ast.Stmt) !void {
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
            else => {},
        }
    }

    fn resolveExpr(self: Self, expr: ast.Expr) !void {
        switch (expr) {
            .variable => |v| {
                const scope = self.scopes.getLastOrNull();
                if (scope) |s| {
                    if (false == s.get(v.lexeme).?) {
                        //Report error somehow:
                        // "Can't read local variable in its own initializer."
                    }
                    try self.resolveLocal(expr, v.lexeme);
                }
            },
            .assign => |a| {
                try self.resolveExpr(a.value.*);
                try self.resolveLocal(expr, a.name.lexeme);
            },
            else => {},
        }
    }

    fn resolveLocal(self: Self, expr: ast.Expr, name: []const u8) !void {
        // Walk through the scopes from back to front and see if we can resolve it.
        const len = self.scopes.items.len;
        var idx: usize = 1;
        while (idx <= len) : (idx += 1) {
            const item = self.scopes.items[len - idx];
            if (item.contains(name)) {
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
