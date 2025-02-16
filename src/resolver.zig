const std = @import("std");
const ast = @import("ast.zig");
const main = @import("main.zig");
const token = @import("token.zig");
const scanner = @import("scanner.zig");
const constants = @import("constants.zig");

const FunctionType = enum {
    none,
    function,
    constructor,
    method,
};

const ClassType = enum {
    none,
    class,
    sublcass,
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
    current_class_type: ClassType,

    pub fn init(arena: *std.heap.ArenaAllocator) Self {
        return Resolver{
            .allocator = arena.allocator(),
            .scopes = std.ArrayList(std.StringArrayHashMap(bool)).init(arena.allocator()),
            .found_error = false,
            .current_function_type = FunctionType.none,
            .current_class_type = ClassType.none,
        };
    }

    pub fn resolve(self: *Self, stmts: []ast.Stmt) std.mem.Allocator.Error!void {
        for (stmts) |*stmt| {
            try self.resolveStmt(stmt);
        }
    }

    fn reportError(self: *Self, line: u32, message: []const u8) void {
        main.reportError(
            line,
            &[_][]const u8{
                "Resolve error: ",
                message,
            },
        );
        self.found_error = true;
    }

    fn scope_depth(self: Self) usize {
        return self.scopes.items.len;
    }

    fn resolveStmt(self: *Self, stmt: *ast.Stmt) !void {
        switch (stmt.*) {
            .block => |b| {
                try self.begin_scope();
                for (b) |*s| {
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
                const old_class = self.current_class_type;
                self.current_class_type = ClassType.class;
                defer self.current_class_type = old_class;

                try self.declare(c.name);

                if (c.super) |*s| {
                    if (std.mem.eql(u8, s.name.lexeme, c.name.lexeme)) {
                        self.reportError(
                            s.name.line,
                            try std.fmt.allocPrint(
                                self.allocator,
                                "'{s}' a class can't inherit from itself.",
                                .{c.name.lexeme},
                            ),
                        );
                    }

                    self.current_class_type = ClassType.sublcass;
                    try self.resolveVariable(s);

                    try self.begin_scope();
                    try self.define(constants.super);
                }

                try self.begin_scope();
                try self.define(constants.this);

                for (c.methods) |*m| {
                    const type_ = if (std.mem.eql(u8, m.name.lexeme, constants.constructor))
                        FunctionType.constructor
                    else
                        FunctionType.method;
                    try self.resolveFunction(m, type_);
                }

                self.end_scope();
                if (c.super != null) {
                    self.end_scope();
                }
                try self.define(c.name.lexeme);
            },
            .function => |*f| {
                try self.declare(f.name);
                try self.define(f.name.lexeme);
                try self.resolveFunction(f, FunctionType.function);
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
                    self.reportError(r.line_number, "return statement only allowed inside functions.");
                }
                if (r.value) |*v| {
                    if (self.current_function_type == FunctionType.constructor) {
                        self.reportError(r.line_number, "can't return a value from an initializer.");
                    }
                    try self.resolveExpr(v);
                }
            },
            .while_ => |*w| {
                try self.resolveExpr(&w.condition);
                try self.resolve(w.body);
            },
        }
    }

    fn resolveFunction(self: *Self, f: *ast.Function, t: FunctionType) !void {
        const enclosing_function_type = self.current_function_type;
        self.current_function_type = t;
        defer self.current_function_type = enclosing_function_type;

        try self.begin_scope();
        for (f.params) |param| {
            try self.declare(param);
            try self.define(param.lexeme);
        }
        try self.resolve(f.body);
        self.end_scope();
    }

    fn resolveVariable(self: *Self, variable: *ast.Variable) !void {
        // Check for this outside of class
        if (std.mem.eql(u8, variable.name.lexeme, constants.this) and
            self.current_class_type == ClassType.none)
        {
            self.reportError(
                variable.name.line,
                std.fmt.comptimePrint("'{s}' can't use outside of a class.", .{constants.this}),
            );
            return;
        }

        // Don't resolve global variables.
        const scope = self.scopes.getLastOrNull();
        if (scope) |s| {
            if (s.get(variable.name.lexeme)) |assign_finished| {
                if (!assign_finished) {
                    self.reportError(
                        variable.name.line,
                        try std.fmt.allocPrint(
                            self.allocator,
                            "'{s}' can't read local variable in its own initializer.",
                            .{variable.name.lexeme},
                        ),
                    );
                }
            }
            try self.resolveLocal(variable, variable.name.lexeme);
        }
    }

    fn resolveExpr(self: *Self, expr: *ast.Expr) !void {
        switch (expr.*) {
            .variable => |*v| {
                try self.resolveVariable(v);
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
            .set => |*s| {
                try self.resolveExpr(s.value);
                try self.resolveExpr(s.object);
            },
            .super => |*s| {
                if (self.current_class_type == ClassType.none) {
                    self.reportError(
                        s.keyword.name.line,
                        std.fmt.comptimePrint("can't use '{s}' outside of class.", .{constants.super}),
                    );
                } else if (self.current_class_type != ClassType.sublcass) {
                    self.reportError(
                        s.keyword.name.line,
                        std.fmt.comptimePrint("can't use '{s}' in a class without super class.", .{constants.super}),
                    );
                }
                try self.resolveLocal(&s.keyword, s.keyword.name.lexeme);
            },
            .call => |*c| {
                try self.resolveExpr(c.callee);

                for (c.arguments) |*arg| {
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
            const text =
                \\'{s}' variable with same name in local scope already exists. 
                \\ Redeclaring a variable in the same scope is not allowed.
            ;
            self.reportError(
                name.line,
                try std.fmt.allocPrint(self.allocator, text, .{name.lexeme}),
            );
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
