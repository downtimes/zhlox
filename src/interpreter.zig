const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const main = @import("main.zig");

const ValueError = error{ NonCallable, Arity };
const CallError = ValueError || RuntimeError;

const Builtin = struct {
    arity: u8,
    function: *const fn ([]Value) Value,
};

const Function = struct {
    declaration: ast.Ast,

    fn function(self: Function) ast.Function {
        // We know the only statement in the ast is the function itself. The function statement is always present.
        return self.declaration.statements.items[0].function;
    }
};

pub const Value = union(enum) {
    const Self = @This();

    string: []const u8, // We expect the memory of this slice to be owned by our interpreter or by the environment.
    number: f64,
    bool_: bool,
    nil: void,
    builtin: Builtin,
    function: Function,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            .function => |*f| {
                f.declaration.deinit();
            },
            else => {},
        }
    }

    // TODO I'm not happy with the current state of the string handling in my code.
    //      Think of a better scheme that scales better and isn't as brittle.
    pub fn clone(self: Self, allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
        var result = self;

        switch (self) {
            .string => |s| {
                const new_str = try allocator.dupe(u8, s);
                result.string = new_str;
            },
            .function => |f| {
                result.function.declaration = try f.declaration.clone(allocator);
            },
            else => {},
        }
        return result;
    }

    fn call(
        self: *Self,
        arguments: []Value,
        interpreter: *Interpreter,
        arena: std.mem.Allocator,
        output: std.fs.File.Writer,
    ) CallError!Value {
        switch (self.*) {
            .string, .number, .bool_, .nil => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arguments);
            },
            .function => |*f| {
                var function_env = environment.Environment.init_with_parent(arena, &interpreter.global_environment);

                const func = f.function();
                if (func.params.items.len != arguments.len) {
                    return ValueError.Arity;
                }

                for (func.params.items, arguments) |param, argument| {
                    try function_env.define(param.lexeme, argument);
                }

                return try interpreter.executeBlock(func.body.items, &function_env, arena, output);
            },
        }
    }

    // Strange rule according to Lex definition...
    fn isTruthy(self: Self) bool {
        switch (self) {
            .bool_ => |b| return b,
            .number => return true,
            .nil => return false,
            .string => return true,
            .builtin => return true,
            .function => return true,
        }
    }

    pub fn equal(self: Self, other: Self) bool {
        switch (self) {
            .bool_ => |b1| {
                switch (other) {
                    .bool_ => |b2| return b1 == b2,
                    else => return false,
                }
            },
            .number => |n1| {
                switch (other) {
                    .number => |n2| return n1 == n2,
                    else => return false,
                }
            },
            .nil => {
                switch (other) {
                    .nil => return true,
                    else => return false,
                }
            },
            .string => |s1| {
                switch (other) {
                    .string => |s2| return std.mem.eql(u8, s1, s2),
                    else => return false,
                }
            },
            .builtin => |b1| {
                switch (other) {
                    .builtin => |b2| return b1.function == b2.function,
                    else => return false,
                }
            },
            .function => |f1| {
                switch (other) {
                    .function => |f2| {
                        return f1.declaration.equals(f2.declaration);
                    },
                    else => return false,
                }
            },
        }
    }
};

fn builtinClock(arguments: []Value) Value {
    _ = arguments;
    const time: f64 = @floatFromInt(@divTrunc(std.time.milliTimestamp(), 1000));
    return Value{ .number = time };
}

pub const RuntimeError = error{
    MinusOperand,
    UndefinedVariable,
    IllegalBinaryOperand,
    InvalidCall,
    Unimplemented,
    OutOfMemory,
} || std.fs.File.WriteError;

pub const Diagnostic = struct {
    token_: token.Token, // Associated token to get the operation and the line number.
    message: []const u8,
};

// TODO give the user feedback when he tries to use return statement outside of function.
pub const Interpreter = struct {
    const Self = @This();

    global_environment: environment.Environment,
    active_environment: *environment.Environment,
    allocator: std.mem.Allocator,
    diagnostic: ?Diagnostic = null,

    pub fn deinit(self: *Self) void {
        self.global_environment.deinit();
    }

    pub fn new(allocator: std.mem.Allocator) !Self {
        var interpreter = Interpreter{
            .allocator = allocator,
            .global_environment = environment.Environment.init(allocator),
            .active_environment = undefined,
        };
        errdefer interpreter.deinit();

        const clock = Value{ .builtin = Builtin{ .arity = 0, .function = &builtinClock } };
        try interpreter.global_environment.define("clock", clock);

        return interpreter;
    }

    pub fn run(self: *Self, stdout: std.fs.File.Writer, input: []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var scan = scanner.Scanner{ .source = input };
        const tokens = try scan.scanTokens(arena.allocator());

        var parse = parser.Parser{ .tokens = tokens.items };
        const parse_tree = try parse.parseInto(arena.allocator());

        self.active_environment = &self.global_environment;
        try self.execute(parse_tree.statements.items, stdout);
    }

    fn execute(self: *Self, statements: []ast.Stmt, output: std.fs.File.Writer) RuntimeError!void {
        for (statements) |stmt| {
            var arena = std.heap.ArenaAllocator.init(self.allocator);
            defer arena.deinit();

            _ = self.executeStatement(stmt, arena.allocator(), output) catch |err| {
                if (err == RuntimeError.Unimplemented) {
                    _ = try output.write("Hit unimplemented part of the interpreter.");
                } else {
                    const diagnostic = self.diagnostic.?;
                    main.reportError(diagnostic.token_.line, &[_][]const u8{ "Runtime Error: ", diagnostic.token_.lexeme, " ", diagnostic.message });
                }
                return err;
            };
        }
    }

    fn executeBlock(self: *Self, statements: []ast.Stmt, env: *environment.Environment, arena: std.mem.Allocator, output: std.fs.File.Writer) RuntimeError!Value {
        var block_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer block_arena.deinit();

        const previous_environment = self.active_environment;
        defer self.active_environment = previous_environment;

        self.active_environment = env;
        for (statements) |statement| {
            const val = try self.executeStatement(statement, block_arena.allocator(), output);
            if (val != Value.nil) {
                // We delete our arena for the block before returning so we need to make sure we hoist the value out
                // to the arena outside of the current block to not get a dangling reference.
                return val.clone(arena);
            }
        }
        return Value.nil;
    }

    fn executeStatement(self: *Self, stmt: ast.Stmt, arena: std.mem.Allocator, output: std.fs.File.Writer) RuntimeError!Value {
        switch (stmt) {
            .cond => |c| {
                const cond = try self.evaluateExpression(c.condition, arena, output);
                if (cond.isTruthy()) {
                    return try self.executeStatement(c.then.*, arena, output);
                } else if (c.els != null) {
                    return try self.executeStatement(c.els.?.*, arena, output);
                }
            },
            .expr => |e| {
                _ = try self.evaluateExpression(e, arena, output);
            },
            .print => |e| {
                // TODO do I want to propagate print errors upwards?
                const value = try self.evaluateExpression(e, arena, output);
                switch (value) {
                    .number => |n| try output.print("{d}", .{n}),
                    .string => |s| try output.print("{s}", .{s}),
                    .bool_ => |b| try output.print("{}", .{b}),
                    .nil => _ = try output.write("nil"),
                    .builtin => _ = try output.write("<native fn>"),
                    .function => |f| {
                        const func = f.function();
                        try output.print("<fn {s}>", .{func.name.lexeme});
                    },
                }
                _ = try output.write("\n");
            },
            .while_ => |w| {
                var cond = try self.evaluateExpression(w.condition, arena, output);

                while (cond.isTruthy()) {
                    const value = try self.executeStatement(w.body.*, arena, output);
                    if (value != Value.nil) {
                        return value;
                    }
                    cond = try self.evaluateExpression(w.condition, arena, output);
                }
            },
            .var_decl => |decl| {
                var val: Value = Value.nil;
                if (decl.initializer) |initializer| {
                    val = try self.evaluateExpression(initializer, arena, output);
                }

                try self.active_environment.define(decl.name.lexeme, val);
            },
            .block => |statements| {
                // Open new environment for each nested block.
                var new_env = environment.Environment.init_with_parent(arena, self.active_environment);
                defer new_env.deinit();

                const value = try self.executeBlock(statements.items, &new_env, arena, output);
                if (value != Value.nil) {
                    return value;
                }
            },
            .function => |f| {
                const body = try ast.Ast.from(f, arena);
                const function = Function{ .declaration = body };
                // TODO do I also want to allow functions to be shadowed? This could potentially be confusing no?
                //      Probably needed though for proper compatibility with lox.
                try self.active_environment.define(f.name.lexeme, Value{ .function = function });
            },
            .ret => |r| {
                if (r.value) |val| {
                    return self.evaluateExpression(val, arena, output);
                }
                return Value.nil;
            },
        }
        return Value.nil;
    }

    fn evaluateExpression(self: *Self, expr: ast.Expr, arena: std.mem.Allocator, output: std.fs.File.Writer) RuntimeError!Value {
        switch (expr) {
            .literal => |l| {
                return switch (l) {
                    .bool_ => |b| Value{ .bool_ = b },
                    .number => |n| Value{ .number = n },
                    .string => |s| blk: {
                        break :blk Value{ .string = s };
                    },
                    .nil => Value.nil,
                };
            },
            .grouping => |g| {
                return self.evaluateExpression(g.*, arena, output);
            },
            .logical => |l| {
                const left = try self.evaluateExpression(l.left.*, arena, output);

                if (l.operator.type_ == token.Type.and_) {
                    if (!left.isTruthy()) return left;
                } else {
                    if (left.isTruthy()) return left;
                }

                return self.evaluateExpression(l.right.*, arena, output);
            },
            .unary => |u| {
                const right = try self.evaluateExpression(u.right.*, arena, output);

                if (u.operator.type_ == token.Type.bang) {
                    return Value{ .bool_ = !right.isTruthy() };
                }

                if (u.operator.type_ == token.Type.minus) {
                    if (@as(std.meta.Tag(Value), right) == .number) {
                        return Value{ .number = right.number };
                    }

                    self.diagnostic = Diagnostic{
                        .token_ = u.operator,
                        .message = "operand must be a number.",
                    };
                    return RuntimeError.MinusOperand;
                }
                return RuntimeError.Unimplemented;
            },
            .binary => |binary| {
                return self.evaluateBinary(binary, arena, output);
            },
            .variable => |variable| {
                return self.getVariable(variable);
            },
            .assign => |assignment| {
                return self.evaluateAssignment(assignment, arena, output);
            },
            .call => |call| {
                var callee = try self.evaluateExpression(call.callee.*, arena, output);
                var arguments = std.ArrayList(Value).init(arena);

                for (call.arguments.items) |arg| {
                    try arguments.append(try self.evaluateExpression(arg, arena, output));
                }

                if (callee.call(arguments.items, self, arena, output)) |value| {
                    return value;
                } else |err| switch (err) {
                    ValueError.NonCallable => {
                        self.diagnostic = Diagnostic{
                            .token_ = call.closing_paren,
                            .message = "can only call functions and classes.",
                        };
                        return RuntimeError.InvalidCall;
                    },
                    ValueError.Arity => {
                        self.diagnostic = Diagnostic{
                            .token_ = call.closing_paren,
                            .message = "argument count does not correspond with function declaration.",
                        };
                        return RuntimeError.InvalidCall;
                    },
                    else => |runtime_error| return runtime_error,
                }
            },
        }

        return RuntimeError.Unimplemented;
    }

    fn getVariable(self: *Self, variable: token.Token) RuntimeError!Value {
        const val = self.active_environment.get(variable);
        if (val == null) {
            self.diagnostic = Diagnostic{
                .token_ = variable,
                .message = "is an undefined variable.",
            };
            return RuntimeError.UndefinedVariable;
        }

        return val.?;
    }

    fn evaluateBinary(self: *Self, binary: ast.Binary, arena: std.mem.Allocator, output: std.fs.File.Writer) RuntimeError!Value {
        const left = try self.evaluateExpression(binary.left.*, arena, output);
        const right = try self.evaluateExpression(binary.right.*, arena, output);

        if (@as(std.meta.Tag(Value), left) == .number and @as(std.meta.Tag(Value), right) == .number) {
            switch (binary.operator.type_) {
                .minus => return Value{ .number = left.number - right.number },
                // TODO possibly create runtime error here when dividing by 0 when nominator not 0 itself,
                //      currently we return inf since zig does the same. Need to look into how other lox
                //      interpreters handle this case
                .slash => return Value{ .number = left.number / right.number },
                .star => return Value{ .number = left.number * right.number },
                .plus => return Value{ .number = left.number + right.number },
                .greater => return Value{ .bool_ = left.number > right.number },
                .greater_equal => return Value{ .bool_ = left.number >= right.number },
                .less => return Value{ .bool_ = left.number < right.number },
                .less_equal => return Value{ .bool_ = left.number <= right.number },
                else => {
                    // TODO
                },
            }
        }

        if (@as(std.meta.Tag(Value), left) == .string and @as(std.meta.Tag(Value), right) == .string) {
            if (binary.operator.type_ == .plus) {
                const combined = try std.mem.concat(arena, u8, &[_][]const u8{ left.string, right.string });
                return Value{ .string = combined };
            }
        }

        switch (binary.operator.type_) {
            .equal_equal => return Value{ .bool_ = left.equal(right) },
            .bang_equal => return Value{ .bool_ = !left.equal(right) },
            .minus, .star, .slash, .less, .less_equal, .greater, .greater_equal => {
                self.diagnostic = Diagnostic{
                    .token_ = binary.operator,
                    .message = "operands must be numbers.",
                };
                return RuntimeError.IllegalBinaryOperand;
            },
            .plus => {
                self.diagnostic = Diagnostic{
                    .token_ = binary.operator,
                    .message = "operands must be two numbers or two strings.",
                };
                return RuntimeError.IllegalBinaryOperand;
            },
            else => {
                // TODO
            },
        }

        return RuntimeError.Unimplemented;
    }

    fn evaluateAssignment(self: *Self, expr: ast.Assignment, arena: std.mem.Allocator, output: std.fs.File.Writer) RuntimeError!Value {
        const value = try self.evaluateExpression(expr.value.*, arena, output);

        self.active_environment.assign(expr.name, value) catch |err| {
            switch (err) {
                // Go up one level in the environment chain.
                environment.EnvironmentError.VariableNotFound => {
                    // No assignment possible so we report an error.
                    self.diagnostic = Diagnostic{
                        .token_ = expr.name,
                        .message = "is an undefined variable.",
                    };
                    return RuntimeError.UndefinedVariable;
                },
                environment.EnvironmentError.OutOfMemory => {
                    return RuntimeError.OutOfMemory;
                },
            }
        };

        return value;
    }
};
