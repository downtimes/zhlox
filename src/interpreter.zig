const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const main = @import("main.zig");

const ValueError = error{ NonCallable, Arity };
const CallError = ValueError || RuntimError;

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
            .function => |f| f.declaration.deinit(),
            else => {},
        }
    }

    // TODO I'm not happy with the current state of the string handling in my code. Think of a better sheme that
    //      scales better and isn't as brittle.
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

    fn call(self: Self, arguments: []Value, interpreter: *Interpreter, output: std.fs.File.Writer) CallError!Value {
        switch (self) {
            .string, .number, .bool_, .nil => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arguments);
            },
            .function => |f| {
                const func = f.function();
                if (func.params.items.len != arguments.len) {
                    return ValueError.Arity;
                }

                var new_env = environment.Environment.init_with_parent(interpreter.allocator, &interpreter.global_environment);
                defer new_env.deinit();

                for (func.params.items, arguments) |param, argument| {
                    try new_env.define(param.lexeme, argument);
                }

                try interpreter.executeBlock(func.body.items, &new_env, output);
                return Value.nil;
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

    fn equal(self: Self, other: Self) bool {
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
                    .function => |f2| return f1.declaration.equals(f2.declaration),
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

pub const RuntimError = error{
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

    fn execute(self: *Self, statements: []ast.Stmt, output: std.fs.File.Writer) RuntimError!void {
        for (statements) |stmt| {
            self.executeStatement(stmt, output) catch |err| {
                if (err == RuntimError.Unimplemented) {
                    _ = try output.write("Hit unimplemented part of the interpreter.");
                } else {
                    const diagnostic = self.diagnostic.?;
                    main.reportError(diagnostic.token_.line, &[_][]const u8{ "Runtime Error:", diagnostic.token_.lexeme, " ", diagnostic.message });
                }
                return err;
            };
        }
    }

    fn executeBlock(self: *Self, statements: []ast.Stmt, env: *environment.Environment, output: std.fs.File.Writer) RuntimError!void {
        const previous_environment = self.active_environment;
        defer self.active_environment = previous_environment;

        self.active_environment = env;
        for (statements) |statement| {
            try self.executeStatement(statement, output);
        }
    }

    fn executeStatement(self: *Self, stmt: ast.Stmt, output: std.fs.File.Writer) RuntimError!void {
        switch (stmt) {
            .cond => |c| {
                var cond = try self.evaluateExpression(c.condition, output);
                defer cond.deinit(self.allocator);
                if (cond.isTruthy()) {
                    try self.executeStatement(c.then.*, output);
                } else if (c.els != null) {
                    try self.executeStatement(c.els.?.*, output);
                }
            },
            .expr => |e| {
                var value = try self.evaluateExpression(e, output);
                defer value.deinit(self.allocator);
            },
            .print => |e| {
                // TODO do I want to propagate print errors upwards?
                var value = try self.evaluateExpression(e, output);
                defer value.deinit(self.allocator);

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
                var cond = try self.evaluateExpression(w.condition, output);
                defer cond.deinit(self.allocator);

                while (cond.isTruthy()) {
                    try self.executeStatement(w.body.*, output);
                    cond.deinit(self.allocator);
                    cond = try self.evaluateExpression(w.condition, output);
                }
            },
            .var_decl => |decl| {
                var val: Value = Value.nil;
                defer val.deinit(self.allocator);

                if (decl.initializer) |initializer| {
                    val = try self.evaluateExpression(initializer, output);
                }

                try self.active_environment.define(decl.name.lexeme, val);
            },
            .block => |statements| {
                // Open new environment for each nested block.
                var new_env = environment.Environment.init_with_parent(self.allocator, self.active_environment);
                defer new_env.deinit();

                try self.executeBlock(statements.items, &new_env, output);
            },
            .function => |f| {
                const body = try ast.Ast.from(f, self.allocator);
                defer body.deinit();

                const function = Function{ .declaration = body };
                try self.active_environment.define(f.name.lexeme, Value{ .function = function });
            },
        }
    }

    fn evaluateExpression(self: *Self, expr: ast.Expr, output: std.fs.File.Writer) RuntimError!Value {
        switch (expr) {
            .literal => |l| {
                return switch (l) {
                    .bool_ => |b| Value{ .bool_ = b },
                    .number => |n| Value{ .number = n },
                    .string => |s| blk: {
                        var owned_string = try self.allocator.dupe(u8, s);
                        break :blk Value{ .string = owned_string };
                    },
                    .nil => Value.nil,
                };
            },
            .grouping => |g| {
                return self.evaluateExpression(g.*, output);
            },
            .logical => |l| {
                var left = try self.evaluateExpression(l.left.*, output);

                if (l.operator.type_ == token.Type.and_) {
                    if (!left.isTruthy()) return left;
                } else {
                    if (left.isTruthy()) return left;
                }

                left.deinit(self.allocator);
                return self.evaluateExpression(l.right.*, output);
            },
            .unary => |u| {
                var right = try self.evaluateExpression(u.right.*, output);
                defer right.deinit(self.allocator);

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
                    return RuntimError.MinusOperand;
                }
                return RuntimError.Unimplemented;
            },
            .binary => |binary| {
                return self.evaluateBinary(binary, output);
            },
            .variable => |variable| {
                return self.getVariable(variable);
            },
            .assign => |assignment| {
                return self.evaluateAssignment(assignment, output);
            },
            .call => |call| {
                var callee = try self.evaluateExpression(call.callee.*, output);
                var arguments = std.ArrayList(Value).init(self.allocator);
                defer {
                    for (arguments.items) |*item| {
                        item.deinit(self.allocator);
                    }
                    arguments.deinit();
                    callee.deinit(self.allocator);
                }

                for (call.arguments.items) |arg| {
                    try arguments.append(try self.evaluateExpression(arg, output));
                }
                if (callee.call(arguments.items, self, output)) |value| {
                    return value;
                } else |err| switch (err) {
                    ValueError.NonCallable => {
                        self.diagnostic = Diagnostic{
                            .token_ = call.closing_paren,
                            .message = "can only call functions and classes.",
                        };
                        return RuntimError.InvalidCall;
                    },
                    ValueError.Arity => {
                        self.diagnostic = Diagnostic{
                            .token_ = call.closing_paren,
                            .message = "can only call functions and classes.",
                        };
                        return RuntimError.InvalidCall;
                    },
                    else => |runtime_error| return runtime_error,
                }
            },
        }

        return RuntimError.Unimplemented;
    }

    fn getVariable(self: *Self, variable: token.Token) RuntimError!Value {
        const val = self.active_environment.get(variable, self.allocator);
        if (val == null) {
            self.diagnostic = Diagnostic{
                .token_ = variable,
                .message = "is an undefined variable.",
            };
            return RuntimError.UndefinedVariable;
        }

        return val.?;
    }

    fn evaluateBinary(self: *Self, binary: ast.Binary, output: std.fs.File.Writer) RuntimError!Value {
        var left = try self.evaluateExpression(binary.left.*, output);
        var right = try self.evaluateExpression(binary.right.*, output);
        defer left.deinit(self.allocator);
        defer right.deinit(self.allocator);

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
                .gerater_equal => return Value{ .bool_ = left.number >= right.number },
                .less => return Value{ .bool_ = left.number < right.number },
                .less_equal => return Value{ .bool_ = left.number <= right.number },
                else => {
                    // TODO
                },
            }
        }

        if (@as(std.meta.Tag(Value), left) == .string and @as(std.meta.Tag(Value), right) == .string) {
            if (binary.operator.type_ == .plus) {
                const combined = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left.string, right.string });
                return Value{ .string = combined };
            }
        }

        switch (binary.operator.type_) {
            .equal_equal => return Value{ .bool_ = left.equal(right) },
            .bang_equal => return Value{ .bool_ = !left.equal(right) },
            .minus, .star, .slash, .less, .less_equal, .greater, .gerater_equal => {
                self.diagnostic = Diagnostic{
                    .token_ = binary.operator,
                    .message = "operands must be numbers.",
                };
                return RuntimError.IllegalBinaryOperand;
            },
            .plus => {
                self.diagnostic = Diagnostic{
                    .token_ = binary.operator,
                    .message = "operands must be two numbers or two strings.",
                };
                return RuntimError.IllegalBinaryOperand;
            },
            else => {
                // TODO
            },
        }

        return RuntimError.Unimplemented;
    }

    fn evaluateAssignment(self: *Self, expr: ast.Assignment, output: std.fs.File.Writer) RuntimError!Value {
        var value = try self.evaluateExpression(expr.value.*, output);

        self.active_environment.assign(expr.name, value) catch |err| {
            switch (err) {
                // Go up one level in the environment chain.
                environment.EnvironmentError.VariableNotFound => {
                    // No assignment possible so we report an error.
                    self.diagnostic = Diagnostic{
                        .token_ = expr.name,
                        .message = "is an undefined variable.",
                    };
                    return RuntimError.UndefinedVariable;
                },
                environment.EnvironmentError.OutOfMemory => {
                    return RuntimError.OutOfMemory;
                },
            }
        };

        return value;
    }
};
