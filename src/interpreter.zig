const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const resolver = @import("resolver.zig");
const main = @import("main.zig");

const ValueError = error{ NonCallable, Arity };
const CallError = ValueError || RuntimeError;

const Builtin = struct {
    arity: u8,
    function: *const fn ([]Value) Value,
};

const Function = struct {
    declaration: ast.Ast,
    env: *environment.Environment,

    fn function(self: Function) ast.Function {
        // We know the only statement in the ast is the function itself. The function statement is always present.
        return self.declaration.statements.items[0].function;
    }
};

const Class = struct {
    name: []const u8,
};

// TODO: how to ensure the class we are pointing to is still around when we need it?
//       create extra memory space for class definitions?
//       how does it interact with environments?
const Instance = struct {
    class: Class,
    // Fields should be allocated in the environment of the instance so
    // the pointers should always be okay?
    fields: std.StringHashMapUnmanaged(*Value),
};

pub const Value = union(enum) {
    const Self = @This();

    string: []const u8, // We expect the memory of this slice to be owned by our interpreter or by the environment.
    number: f64,
    bool_: bool,
    nil: void,
    builtin: Builtin,
    function: Function,
    class: Class,
    instance: Instance,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            .class => |c| allocator.free(c.name),
            .instance => |*i| {
                i.fields.deinit(allocator);
            },
            .function => |*f| {
                std.debug.assert(f.env.closure_refs != 0);
                f.env.closure_refs -= 1;
                f.env.clean_unused();
                f.declaration.deinit();
            },
            else => {},
        }
    }

    // TODO I'm not happy with the current state of string handling in my code.
    //      Think of a better scheme that scales better and isn't as brittle.
    pub fn clone(self: Self, allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
        var result = self;

        switch (self) {
            .string => |s| {
                result.string = try allocator.dupe(u8, s);
            },
            .class => |c| {
                result.class.name = try allocator.dupe(u8, c.name);
            },
            .instance => |i| {
                result.instance.fields = try i.fields.clone(allocator);
            },
            .function => |f| {
                result.function.declaration = try f.declaration.clone(allocator);
                result.function.env.closure_refs += 1;
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
    ) CallError!Value {
        switch (self.*) {
            .string, .number, .bool_, .nil, .instance => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arguments);
            },
            .class => |c| {
                if (arguments.len != 0) {
                    return ValueError.Arity;
                }
                return Value{ .instance = Instance{
                    .class = c,
                    .fields = std.StringHashMapUnmanaged(*Value){},
                } };
            },
            .function => |*f| {
                const func = f.function();
                if (func.params.items.len != arguments.len) {
                    return ValueError.Arity;
                }

                var function_env = try environment.Environment.create_with_parent(interpreter.allocator, f.env);
                defer function_env.clean_unused();

                for (func.params.items, arguments) |param, argument| {
                    try function_env.define(param.lexeme, argument);
                }

                return try interpreter.executeBlock(func.body.items, function_env, arena);
            },
        }
    }

    // Strange rule according to Lox definition...
    fn isTruthy(self: Self) bool {
        switch (self) {
            .bool_ => |b| return b,
            .nil => return false,
            inline else => return true,
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
                        return f1.declaration.equals(f2.declaration) and (f1.env == f2.env);
                    },
                    else => return false,
                }
            },
            .class => |c| {
                switch (other) {
                    .class => |c2| {
                        // TODO: The current implementation is simply broken.
                        return std.mem.eql(u8, c.name, c2.name);
                    },
                    else => return false,
                }
            },
            .instance => |i| {
                switch (other) {
                    .instance => |o| {
                        // TODO: The current implementation is simply broken.
                        var i_fields = i.fields.iterator();
                        while (i_fields.next()) |kv| {
                            if (o.fields.get(kv.key_ptr.*)) |v| {
                                if (!kv.value_ptr.*.equal(v.*)) {
                                    return false;
                                }
                            } else {
                                return false;
                            }
                        }
                        return std.mem.eql(u8, i.class.name, i.class.name);
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
    GetNonInstance,
    UnknownProperty,
} || std.fs.File.WriteError;

pub const Diagnostic = struct {
    token_: token.Token, // Associated token to get the operation and the line number.
    message: []const u8,
};

// TODO give the user feedback when he tries to use return statement outside of function.
pub const Interpreter = struct {
    const Self = @This();

    global_environment: *environment.Environment,
    active_environment: *environment.Environment,
    output: std.fs.File.Writer,
    allocator: std.mem.Allocator,
    diagnostic: ?Diagnostic = null,

    pub fn deinit(self: *Self) void {
        self.global_environment.deinit();
    }

    pub fn new(allocator: std.mem.Allocator) !Self {
        var interpreter = Interpreter{
            .output = undefined,
            .allocator = allocator,
            .global_environment = try environment.Environment.create(allocator),
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
        const tokens = try scan.scanTokens(&arena);

        var parse = parser.Parser{ .tokens = tokens.items };
        const parse_tree = try parse.parseInto(&arena);

        // Resolve pre pass for variable resolution.
        {
            // The memory needed for resolve can be freed after the resolve
            // pass is over. Therefore it gets its own little arena.
            var resolve_arena = std.heap.ArenaAllocator.init(self.allocator);
            defer resolve_arena.deinit();
            var res = resolver.Resolver.init(&resolve_arena);
            try res.resolve(parse_tree.statements.items);
            if (res.found_error) {
                return;
            }
        }

        self.active_environment = self.global_environment;
        self.output = stdout;
        try self.execute(parse_tree.statements.items);
    }

    fn execute(self: *Self, statements: []ast.Stmt) RuntimeError!void {
        for (statements) |stmt| {
            var arena = std.heap.ArenaAllocator.init(self.allocator);
            defer arena.deinit();

            _ = self.executeStatement(stmt, arena.allocator()) catch |err| {
                if (err == RuntimeError.Unimplemented) {
                    _ = try self.output.write("Hit unimplemented part of the interpreter.");
                } else {
                    const diagnostic = self.diagnostic.?;
                    main.reportError(diagnostic.token_.line, &[_][]const u8{ "Runtime Error: '", diagnostic.token_.lexeme, "' ", diagnostic.message });
                }
                return err;
            };
        }
    }

    fn executeBlock(self: *Self, statements: []ast.Stmt, env: *environment.Environment, arena: std.mem.Allocator) RuntimeError!Value {
        var block_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer block_arena.deinit();

        const previous_environment = self.active_environment;
        defer self.active_environment = previous_environment;

        self.active_environment = env;
        for (statements) |statement| {
            const val = try self.executeStatement(statement, block_arena.allocator());
            if (val != Value.nil) {
                // We delete our arena for the block before returning so we need to make sure we hoist the value out
                // to the arena outside of the current block to not get a dangling reference.
                return val.clone(arena);
            }
        }
        return Value.nil;
    }

    fn executeStatement(self: *Self, stmt: ast.Stmt, arena: std.mem.Allocator) RuntimeError!Value {
        switch (stmt) {
            .cond => |c| {
                const cond = try self.evaluateExpression(c.condition, arena);
                if (cond.isTruthy()) {
                    return try self.executeStatement(c.then.*, arena);
                } else if (c.els != null) {
                    return try self.executeStatement(c.els.?.*, arena);
                }
            },
            .expr => |e| {
                _ = try self.evaluateExpression(e, arena);
            },
            .print => |e| {
                const value = try self.evaluateExpression(e, arena);
                switch (value) {
                    .number => |n| try self.output.print("{d}", .{n}),
                    .string => |s| try self.output.print("{s}", .{s}),
                    .bool_ => |b| try self.output.print("{}", .{b}),
                    .nil => _ = try self.output.write("nil"),
                    .builtin => _ = try self.output.write("<native fn>"),
                    .class => |c| try self.output.print("class {s}", .{c.name}),
                    .instance => |i| try self.output.print("{s} instance", .{i.class.name}),
                    .function => |f| {
                        const func = f.function();
                        try self.output.print("<fn {s}>", .{func.name.lexeme});
                    },
                }
                _ = try self.output.write("\n");
            },
            .while_ => |w| {
                var cond = try self.evaluateExpression(w.condition, arena);

                while (cond.isTruthy()) {
                    for (w.body.items) |s| {
                        const value = try self.executeStatement(s, arena);
                        if (value != Value.nil) {
                            return value;
                        }
                    }
                    cond = try self.evaluateExpression(w.condition, arena);
                }
            },
            .var_decl => |decl| {
                var val: Value = Value.nil;
                if (decl.initializer) |initializer| {
                    val = try self.evaluateExpression(initializer, arena);
                }

                try self.active_environment.define(decl.name.lexeme, val);
            },
            .block => |statements| {
                // Open new environment for each nested block.
                const new_env = try environment.Environment.create_with_parent(self.allocator, self.active_environment);
                defer new_env.clean_unused();

                const value = try self.executeBlock(statements.items, new_env, arena);
                if (value != Value.nil) {
                    return value;
                }
            },
            .class => |c| {
                try self.active_environment.define(c.name.lexeme, Value.nil);
                self.active_environment.assign(c.name.lexeme, Value{ .class = Class{ .name = c.name.lexeme } }) catch {
                    // Do nothing since we defined the variable before so this case is never hit.
                };
                return Value.nil;
            },
            .function => |f| {
                const body = try ast.Ast.from(f, arena);
                const function = Function{ .declaration = body, .env = self.active_environment };
                self.active_environment.closure_refs += 1;
                // TODO do I also want to allow functions to be shadowed? This could potentially be confusing no?
                //      Probably needed though for proper compatibility with lox.
                try self.active_environment.define(f.name.lexeme, Value{ .function = function });
            },
            .ret => |r| {
                if (r.value) |val| {
                    return self.evaluateExpression(val, arena);
                }
                return Value.nil;
            },
        }
        return Value.nil;
    }

    fn evaluateExpression(self: *Self, expr: ast.Expr, arena: std.mem.Allocator) RuntimeError!Value {
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
                return self.evaluateExpression(g.*, arena);
            },
            .logical => |l| {
                const left = try self.evaluateExpression(l.left.*, arena);

                if (l.operator.type_ == token.Type.and_) {
                    if (!left.isTruthy()) return left;
                } else {
                    if (left.isTruthy()) return left;
                }

                return self.evaluateExpression(l.right.*, arena);
            },
            .unary => |u| {
                const right = try self.evaluateExpression(u.right.*, arena);

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
                return self.evaluateBinary(binary, arena);
            },
            .variable => |variable| {
                return self.getVariable(variable);
            },
            .assign => |assignment| {
                return self.evaluateAssignment(assignment, arena);
            },
            .get => |g| {
                const object = try self.evaluateExpression(g.object.*, arena);
                switch (object) {
                    .instance => |i| {
                        const val = i.fields.get(g.name.lexeme);
                        if (val) |v| {
                            return v.*;
                        } else {
                            self.diagnostic = Diagnostic{
                                .token_ = g.name,
                                .message = "Unknown property.",
                            };
                            return RuntimeError.UnknownProperty;
                        }
                    },
                    else => {},
                }
                self.diagnostic = Diagnostic{
                    .token_ = g.name,
                    .message = "Only instances have properties.",
                };
                return RuntimeError.GetNonInstance;
            },
            .call => |call| {
                var callee = try self.evaluateExpression(call.callee.*, arena);
                var arguments = std.ArrayList(Value).init(arena);

                for (call.arguments.items) |arg| {
                    try arguments.append(try self.evaluateExpression(arg, arena));
                }

                if (callee.call(arguments.items, self, arena)) |value| {
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

    fn getVariable(self: *Self, variable: ast.Variable) RuntimeError!Value {
        var val: ?Value = undefined;
        if (variable.resolve_steps) |steps| {
            val = self.active_environment.getInParent(steps, variable.name.lexeme);
        } else {
            val = self.global_environment.get(variable.name.lexeme);
        }

        if (val == null) {
            self.diagnostic = Diagnostic{
                .token_ = variable.name,
                .message = "is an undefined variable.",
            };
            return RuntimeError.UndefinedVariable;
        }

        return val.?;
    }

    fn evaluateBinary(self: *Self, binary: ast.Binary, arena: std.mem.Allocator) RuntimeError!Value {
        const left = try self.evaluateExpression(binary.left.*, arena);
        const right = try self.evaluateExpression(binary.right.*, arena);

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
                    // Should not be hit if all unary operators are handled in
                    // the previous switch arms.
                    std.debug.assert(false);
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
                // Should not be hit if we didn't forget to implement some
                // binary expression.
                std.debug.assert(false);
            },
        }

        return RuntimeError.Unimplemented;
    }

    fn evaluateAssignment(self: *Self, assignment: ast.Assignment, arena: std.mem.Allocator) RuntimeError!Value {
        const value = try self.evaluateExpression(assignment.value.*, arena);

        var res: environment.EnvironmentError!void = undefined;
        if (assignment.variable.resolve_steps) |steps| {
            res = self.active_environment.assignInParent(steps, assignment.variable.name.lexeme, value);
        } else {
            res = self.global_environment.assign(assignment.variable.name.lexeme, value);
        }

        if (res) |_| {
            return value;
        } else |err| {
            switch (err) {
                // Go up one level in the environment chain.
                environment.EnvironmentError.VariableNotFound => {
                    // No assignment possible so we report an error.
                    self.diagnostic = Diagnostic{
                        .token_ = assignment.variable.name,
                        .message = "is an undefined variable.",
                    };
                    return RuntimeError.UndefinedVariable;
                },
                environment.EnvironmentError.OutOfMemory => {
                    return RuntimeError.OutOfMemory;
                },
            }
        }
    }
};
