const std = @import("std");
const ast = @import("ast.zig");
const main = @import("main.zig");
const token = @import("token.zig");
const interpreter = @import("interpreter.zig");

const FunctionType = enum {
    none,
    function,
};

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
    found_error: bool,
    current_function_type: FunctionType,

    pub fn init(arena: *std.heap.ArenaAllocator) Self {
        return Resolver{
            .allocator = arena.allocator(),
            .scopes = std.ArrayList(std.StringArrayHashMap(bool)).init(arena.allocator()),
            .found_error = false,
            .current_function_type = FunctionType.none,
        };
    }

    pub fn resolve(self: *Self, stmts: []ast.Stmt) std.mem.Allocator.Error!void {
        for (stmts) |*stmt| {
            try self.resolveStmt(stmt);
        }
    }

    fn scope_depth(self: Self) usize {
        return self.scopes.items.len;
    }

    fn resolveStmt(self: *Self, stmt: *ast.Stmt) !void {
        switch (stmt.*) {
            .block => |b| {
                try self.begin_scope();
                for (b.items) |*s| {
                    try self.resolveStmt(s);
                }
                self.end_scope();
            },
            .var_decl => |*vd| {
                try self.declare(vd.name);
                if (vd.initializer) |*i| {
                    try self.resolveExpr(i);
                }
                try self.define(vd.name.lexeme);
            },
            .class => |*c| {
                try self.declare(c.name);
                try self.define(c.name.lexeme);
            },
            .function => |*f| {
                const enclosing_function_type = self.current_function_type;
                self.current_function_type = FunctionType.function;
                defer self.current_function_type = enclosing_function_type;
                try self.declare(f.name);
                try self.define(f.name.lexeme);
                try self.resolveFunction(f);
            },
            .expr => |*e| {
                try self.resolveExpr(e);
            },
            .cond => |*c| {
                try self.resolveExpr(&c.condition);
                try self.resolveStmt(c.then);
                if (c.els) |e| {
                    try self.resolveStmt(e);
                }
            },
            .print => |*p| {
                try self.resolveExpr(p);
            },
            .ret => |*r| {
                if (self.current_function_type == FunctionType.none) {
                    main.reportError(
                        r.line_number,
                        &[_][]const u8{
                            "Resolver Error: return statement only allowed inside functions.",
                        },
                    );
                }
                if (r.value) |*v| {
                    try self.resolveExpr(v);
                }
            },
            .while_ => |*w| {
                try self.resolveExpr(&w.condition);
                try self.resolve(w.body.items);
            },
        }
    }

    fn resolveFunction(self: *Self, function: *ast.Function) std.mem.Allocator.Error!void {
        try self.begin_scope();
        for (function.params.items) |param| {
            try self.declare(param);
            try self.define(param.lexeme);
        }
        try self.resolve(function.body.items);
        self.end_scope();
    }

    fn resolveExpr(self: *Self, expr: *ast.Expr) !void {
        switch (expr.*) {
            .variable => |*v| {
                const scope = self.scopes.getLastOrNull();
                if (scope) |s| {
                    if (s.get(v.name.lexeme)) |assign_finished| {
                        if (!assign_finished) {
                            main.reportError(
                                v.name.line,
                                &[_][]const u8{
                                    "Resolver Error: '",
                                    v.name.lexeme,
                                    "' ",
                                    "Can't read local variable in its own initializer.",
                                },
                            );
                            self.found_error = true;
                        }
                    }
                    try self.resolveLocal(v, v.name.lexeme);
                }
            },
            .assign => |*a| {
                try self.resolveExpr(a.value);
                try self.resolveLocal(&a.variable, a.variable.name.lexeme);
            },
            .binary => |*b| {
                try self.resolveExpr(b.left);
                try self.resolveExpr(b.right);
            },
            .get => |*g| {
                try self.resolveExpr(g.object);
            },
            .call => |*c| {
                try self.resolveExpr(c.callee);

                for (c.arguments.items) |*arg| {
                    try self.resolveExpr(arg);
                }
            },
            .grouping => |g| {
                try self.resolveExpr(g);
            },
            .logical => |l| {
                try self.resolveExpr(l.left);
                try self.resolveExpr(l.right);
            },
            .unary => |u| {
                try self.resolveExpr(u.right);
            },
            else => {},
        }
    }

    // Note: in contrast to the lox interpreter described by the book, this
    //       implementation chose to add the information to the ast itself.
    //       We don't have objects with stable addresses like in java, adding
    //       a separate side table would be harder to implement.
    fn resolveLocal(self: Self, expr: *ast.Variable, name: []const u8) !void {
        // Walk through the scopes from back to front and see if we can resolve it.
        //
        // If the variable is a global we will not be making an entry for it.
        // So for all variables without an entry the interpreter will assume
        // they are defined in the global environment.
        const len = self.scopes.items.len;
        for (1..len + 1) |idx| {
            const scope = self.scopes.items[len - idx];
            if (scope.contains(name)) {
                // record number of hops to help interpreter to resolve value
                // correctly
                expr.resolve_steps = @intCast(idx - 1);
                return;
            }
        }
    }

    fn declare(self: *Self, name: token.Token) !void {
        if (self.scope_depth() == 0) return;

        const scope: *std.StringArrayHashMap(bool) = &self.scopes.items[self.scope_depth() - 1];
        if (scope.contains(name.lexeme)) {
            main.reportError(name.line, &[_][]const u8{
                "Resolver Error: '",
                name.lexeme,
                "' ",
                "variable with same name in local scope already exists. ",
                "Redeclaring a variable in the same scope is not allowed.",
            });
            self.found_error = true;
        }
        try scope.put(name.lexeme, false);
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
