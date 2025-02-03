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
    function: *const fn (std.mem.Allocator, []*Value) std.mem.Allocator.Error!*Value,
};

const Function = struct {
    declaration: *ast.Function,
    env: *environment.Environment,
};

const Class = struct {
    name: []const u8, // This memory is not owned, it points into input_scratch.
    methods: std.StringHashMap(Function),
};

const Instance = struct {
    class: Class,
    fields: std.StringHashMap(*Value),
};

pub const Represent = union(enum) {
    const Self = @This();

    string: []const u8, // String values in here are owned by the value.
    number: f64,
    bool_: bool,
    nil: void,
    builtin: Builtin,
    function: Function,
    class: Class,
    instance: Instance,

    fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            .class => |*c| {
                var it = c.methods.valueIterator();
                while (it.next()) |m| {
                    if (m.env.refs > 0) {
                        m.env.refs -= 1;
                        m.env.clean_unused(true);
                    }
                }
                c.methods.deinit();
            },
            .instance => |*i| {
                var it = i.fields.valueIterator();
                while (it.next()) |v| {
                    v.*.deinit();
                }
                i.fields.deinit();
            },
            .function => |*f| {
                if (f.env.refs > 0) {
                    f.env.refs -= 1;
                    f.env.clean_unused(true);
                }
            },
            else => {},
        }
    }

    // TODO I'm not happy with the current state of string handling in my code.
    //      Think of a better scheme that scales better and isn't as brittle.
    fn clone(self: Self, allocator: std.mem.Allocator) std.mem.Allocator.Error!Self {
        var result = self;

        switch (self) {
            .string => |s| {
                result.string = try allocator.dupe(u8, s);
            },
            .instance => |i| {
                result.instance.fields = try i.fields.cloneWithAllocator(allocator);
            },
            .function => |_| {
                result.function.env.refs += 1;
            },
            else => {},
        }
        return result;
    }

    fn call(
        self: *Self,
        arguments: []*Value,
        interpreter: *Interpreter,
        arena: std.mem.Allocator,
    ) CallError!*Value {
        switch (self.*) {
            .string, .number, .bool_, .nil, .instance => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arena, arguments);
            },
            .class => |c| {
                if (arguments.len != 0) {
                    return ValueError.Arity;
                }
                return Value.init(arena, Represent{ .instance = Instance{
                    .class = c,
                    .fields = std.StringHashMap(*Value).init(arena),
                } });
            },
            .function => |*f| {
                const func = f.declaration;
                if (func.params.items.len != arguments.len) {
                    return ValueError.Arity;
                }

                var function_env = try environment.Environment.create_with_parent(interpreter.allocator, f.env);
                defer function_env.clean_unused(true);

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

    fn equal(self: Self, other: Self) bool {
        switch (self) {
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
            inline else => |s, s_tag| {
                switch (other) {
                    s_tag => |o| return std.meta.eql(s, o),
                    else => return false,
                }
            },
        }
    }
};

pub const Value = struct {
    ref: u32,
    repr: Represent,
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator, repr: Represent) !*Value {
        const place = try allocator.create(Value);
        place.*.ref = 1;
        place.*.allocator = allocator;
        place.*.repr = repr;
        return place;
    }

    fn equal(self: Value, other: *Value) bool {
        return self.repr.equal(other.repr);
    }

    fn isTruthy(self: Value) bool {
        return self.repr.isTruthy();
    }

    fn call(
        self: *Value,
        arguments: []*Value,
        interpreter: *Interpreter,
        arena: std.mem.Allocator,
    ) CallError!*Value {
        return self.repr.call(arguments, interpreter, arena);
    }

    pub fn deinit(self: *Value) void {
        std.debug.assert(self.ref > 0);
        self.ref -= 1;
        if (self.ref == 0) {
            self.repr.deinit(self.allocator);
            self.allocator.destroy(self);
        }
    }
};

fn builtinClock(arena: std.mem.Allocator, arguments: []*Value) !*Value {
    _ = arguments;
    const time: f64 = @floatFromInt(@divTrunc(std.time.milliTimestamp(), 1000));
    return try Value.init(arena, Represent{ .number = time });
}

pub const RuntimeError = error{
    MinusOperand,
    UndefinedVariable,
    IllegalBinaryOperand,
    InvalidCall,
    Unimplemented,
    OutOfMemory,
    NonInstance,
    UnknownProperty,
} || std.fs.File.WriteError;

pub const Diagnostic = struct {
    line_number: u32,
    input: []const u8, // Not owned by diagnostic, expected to be in input scratch.
    message: []const u8, // Not owned by diagnostic, expected to be static str.
};

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

        const clock = try Value.init(allocator, Represent{ .builtin = Builtin{ .arity = 0, .function = &builtinClock } });
        defer clock.deinit();
        try interpreter.global_environment.define("clock", clock);

        return interpreter;
    }

    pub fn run(
        self: *Self,
        stdout: std.fs.File.Writer,
        input: []const u8,
        input_scratch: std.mem.Allocator,
    ) !void {
        var scan = scanner.Scanner{ .source = input };
        const tokens = try scan.scanTokens(input_scratch);

        var parse = parser.Parser{ .tokens = tokens.items };
        const parse_tree = try parse.parseInto(input_scratch);

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
        for (statements) |*stmt| {
            var arena = std.heap.ArenaAllocator.init(self.allocator);
            defer arena.deinit();

            _ = self.executeStatement(stmt, arena.allocator()) catch |err| {
                if (err == RuntimeError.Unimplemented) {
                    _ = try self.output.write("Hit unimplemented part of the interpreter.");
                } else {
                    const diagnostic = self.diagnostic.?;
                    main.reportError(
                        diagnostic.line_number,
                        &[_][]const u8{
                            "Runtime Error: '",
                            diagnostic.input,
                            "' ",
                            diagnostic.message,
                        },
                    );
                }
                return err;
            };
        }
    }

    fn executeBlock(
        self: *Self,
        statements: []ast.Stmt,
        env: *environment.Environment,
        arena: std.mem.Allocator,
    ) RuntimeError!*Value {
        var block_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer block_arena.deinit();

        const previous_environment = self.active_environment;
        defer self.active_environment = previous_environment;

        self.active_environment = env;
        for (statements) |*statement| {
            var val = try self.executeStatement(statement, block_arena.allocator());
            errdefer val.deinit();
            if (val.repr != Represent.nil) {
                // If ref is 1 the value is allocated in the block arena and needs
                // to be hoisted to the outer arena.
                if (val.ref == 1) {
                    val = try Value.init(arena, try val.repr.clone(arena));
                }
                return val;
            } else {
                val.deinit();
            }
        }
        return try Value.init(arena, Represent.nil);
    }

    fn executeStatement(self: *Self, stmt: *ast.Stmt, arena: std.mem.Allocator) RuntimeError!*Value {
        const nil = try Value.init(arena, Represent.nil);
        switch (stmt.*) {
            .cond => |*c| {
                const cond = try self.evaluateExpression(&c.condition, arena);
                defer cond.deinit();
                if (cond.isTruthy()) {
                    return try self.executeStatement(c.then, arena);
                } else if (c.els != null) {
                    return try self.executeStatement(c.els.?, arena);
                }
            },
            .expr => |*e| {
                const val = try self.evaluateExpression(e, arena);
                val.deinit();
            },
            .print => |*e| {
                const value = try self.evaluateExpression(e, arena);
                switch (value.repr) {
                    .number => |n| try self.output.print("{d}", .{n}),
                    .string => |s| try self.output.print("{s}", .{s}),
                    .bool_ => |b| try self.output.print("{}", .{b}),
                    .nil => _ = try self.output.write("nil"),
                    .builtin => _ = try self.output.write("<native fn>"),
                    .class => |c| try self.output.print("class {s}", .{c.name}),
                    .instance => |i| try self.output.print("{s} instance", .{i.class.name}),
                    .function => |f| {
                        try self.output.print("<fn {s}>", .{f.declaration.name.lexeme});
                    },
                }
                _ = try self.output.write("\n");
                value.deinit();
            },
            .while_ => |*w| {
                var cond = try self.evaluateExpression(&w.condition, arena);
                defer cond.deinit();

                while (cond.isTruthy()) {
                    for (w.body.items) |*s| {
                        const value = try self.executeStatement(s, arena);
                        if (value.repr != Represent.nil) {
                            return value;
                        } else {
                            value.deinit();
                        }
                    }
                    cond.deinit();
                    cond = try self.evaluateExpression(&w.condition, arena);
                }
            },
            .var_decl => |*decl| {
                var val: *Value = nil;
                defer val.deinit();
                if (decl.initializer) |*initializer| {
                    val = try self.evaluateExpression(initializer, arena);
                }

                // Signal for the value is only in an arena for now.
                if (val.ref == 1) {
                    val = try Value.init(self.allocator, try val.repr.clone(self.allocator));
                }

                try self.active_environment.define(decl.name.lexeme, val);
            },
            .block => |statements| {
                // Open new environment for each nested block.
                const new_env = try environment.Environment.create_with_parent(self.allocator, self.active_environment);
                defer new_env.clean_unused(false);

                const value = try self.executeBlock(statements.items, new_env, arena);
                if (value.repr != Represent.nil) {
                    return value;
                } else {
                    value.deinit();
                }
            },
            .class => |*c| {
                try self.active_environment.define(c.name.lexeme, nil);

                var methods = std.StringHashMap(Function).init(self.allocator);
                try methods.ensureTotalCapacity(@intCast(c.methods.items.len));
                for (c.methods.items) |*m| {
                    const function = Function{ .declaration = m, .env = self.active_environment };
                    self.active_environment.refs += 1;
                    try methods.put(m.name.lexeme, function);
                }

                const val = try Value.init(self.allocator, Represent{
                    .class = Class{
                        .name = c.name.lexeme,
                        .methods = methods,
                    },
                });
                defer val.deinit();
                self.active_environment.assign(c.name.lexeme, val) catch {
                    // Do nothing since we defined the variable before so this case is never hit.
                };
                return nil;
            },
            .function => |*f| {
                const function = Function{ .declaration = f, .env = self.active_environment };
                self.active_environment.refs += 1;
                // TODO do I also want to allow functions to be shadowed? This could potentially be confusing no?
                //      Probably needed though for proper compatibility with lox.
                const val = try Value.init(self.allocator, Represent{ .function = function });
                defer val.deinit();
                try self.active_environment.define(f.name.lexeme, val);
            },
            .ret => |*r| {
                if (r.value) |*val| {
                    return self.evaluateExpression(val, arena);
                }
                return nil;
            },
        }
        return nil;
    }

    fn evaluateExpression(self: *Self, expr: *ast.Expr, arena: std.mem.Allocator) RuntimeError!*Value {
        switch (expr.*) {
            .literal => |l| {
                return switch (l) {
                    .bool_ => |b| Value.init(arena, Represent{ .bool_ = b }),
                    .number => |n| Value.init(arena, Represent{ .number = n }),
                    // The value is not duped here yet, it will happen when the
                    // block goes out of scope or the value is put into an environment
                    // via variable initialization.
                    .string => |s| Value.init(arena, Represent{ .string = s }),
                    .nil => Value.init(arena, Represent.nil),
                };
            },
            .grouping => |g| {
                return self.evaluateExpression(g, arena);
            },
            .logical => |l| {
                const left = try self.evaluateExpression(l.left, arena);

                if (l.operator.type_ == token.Type.and_) {
                    if (!left.isTruthy()) return left;
                } else {
                    if (left.isTruthy()) return left;
                }

                return self.evaluateExpression(l.right, arena);
            },
            .unary => |u| {
                const right = try self.evaluateExpression(u.right, arena);
                defer right.deinit();

                if (u.operator.type_ == token.Type.bang) {
                    return Value.init(arena, Represent{ .bool_ = !right.isTruthy() });
                }

                if (u.operator.type_ == token.Type.minus) {
                    if (@as(std.meta.Tag(Represent), right.repr) == .number) {
                        return Value.init(arena, Represent{ .number = -right.repr.number });
                    }

                    self.diagnostic = Diagnostic{
                        .line_number = u.operator.line,
                        .input = u.operator.lexeme,
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
                const object = try self.evaluateExpression(g.object, arena);
                defer object.deinit();

                switch (object.repr) {
                    .instance => |*i| {
                        const val = i.fields.get(g.name.lexeme);
                        if (val) |v| {
                            v.ref += 1;
                            return v;
                        }

                        const method = i.class.methods.get(g.name.lexeme);
                        if (method) |m| {
                            m.env.refs += 1;
                            return Value.init(arena, Represent{ .function = m });
                        }

                        self.diagnostic = Diagnostic{
                            .line_number = g.name.line,
                            .input = g.name.lexeme,
                            .message = "Unknown property.",
                        };
                        return RuntimeError.UnknownProperty;
                    },
                    else => {},
                }
                self.diagnostic = Diagnostic{
                    .line_number = g.name.line,
                    .input = g.name.lexeme,
                    .message = "Only instances have properties.",
                };
                return RuntimeError.NonInstance;
            },
            .set => |s| {
                const object = try self.evaluateExpression(s.object, arena);
                defer object.deinit();

                switch (object.repr) {
                    .instance => |*i| {
                        var value = try self.evaluateExpression(s.value, arena);
                        errdefer value.deinit();
                        // Signal that the value is only allocated in arena.
                        // Move to global storage.
                        if (value.ref == 1) {
                            value = try Value.init(self.allocator, try value.repr.clone(self.allocator));
                        }
                        const resp = try i.fields.getOrPut(s.name.lexeme);
                        value.ref += 1;
                        if (resp.found_existing) {
                            resp.value_ptr.*.deinit();
                        }
                        resp.value_ptr.* = value;
                        return value;
                    },
                    else => {},
                }
                self.diagnostic = Diagnostic{
                    .line_number = s.name.line,
                    .input = s.name.lexeme,
                    .message = "Only instances have fields.",
                };
                return RuntimeError.NonInstance;
            },
            .call => |call| {
                var callee = try self.evaluateExpression(call.callee, arena);
                defer callee.deinit();
                var arguments = std.ArrayList(*Value).init(arena);
                defer for (arguments.items) |arg| {
                    arg.deinit();
                };

                for (call.arguments.items) |*arg| {
                    try arguments.append(try self.evaluateExpression(arg, arena));
                }

                if (callee.call(arguments.items, self, arena)) |value| {
                    return value;
                } else |err| switch (err) {
                    ValueError.NonCallable => {
                        self.diagnostic = Diagnostic{
                            .line_number = call.line_number,
                            .input = ")",
                            .message = "can only call functions and classes.",
                        };
                        return RuntimeError.InvalidCall;
                    },
                    ValueError.Arity => {
                        self.diagnostic = Diagnostic{
                            .line_number = call.line_number,
                            .input = ")",
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

    fn getVariable(self: *Self, variable: ast.Variable) RuntimeError!*Value {
        var val: ?*Value = undefined;
        if (variable.resolve_steps) |steps| {
            val = self.active_environment.getInParent(steps, variable.name.lexeme);
        } else {
            val = self.global_environment.get(variable.name.lexeme);
        }

        if (val == null) {
            self.diagnostic = Diagnostic{
                .line_number = variable.name.line,
                .input = variable.name.lexeme,
                .message = "is an undefined variable.",
            };
            return RuntimeError.UndefinedVariable;
        }

        return val.?;
    }

    fn evaluateBinary(self: *Self, binary: ast.Binary, arena: std.mem.Allocator) RuntimeError!*Value {
        const left = try self.evaluateExpression(binary.left, arena);
        defer left.deinit();
        const right = try self.evaluateExpression(binary.right, arena);
        defer right.deinit();

        if (@as(std.meta.Tag(Represent), left.repr) == .number and
            @as(std.meta.Tag(Represent), right.repr) == .number)
        {
            switch (binary.operator.type_) {
                .minus => return Value.init(arena, Represent{
                    .number = left.repr.number - right.repr.number,
                }),
                // TODO possibly create runtime error here when dividing by 0 when nominator not 0 itself,
                //      currently we return inf since zig does the same. Need to look into how other lox
                //      interpreters handle this case
                .slash => return Value.init(arena, Represent{
                    .number = left.repr.number / right.repr.number,
                }),
                .star => return Value.init(arena, Represent{
                    .number = left.repr.number * right.repr.number,
                }),
                .plus => return Value.init(arena, Represent{
                    .number = left.repr.number + right.repr.number,
                }),
                .greater => return Value.init(arena, Represent{
                    .bool_ = left.repr.number > right.repr.number,
                }),
                .greater_equal => return Value.init(arena, Represent{
                    .bool_ = left.repr.number >= right.repr.number,
                }),
                .less => return Value.init(arena, Represent{
                    .bool_ = left.repr.number < right.repr.number,
                }),
                .less_equal => return Value.init(arena, Represent{
                    .bool_ = left.repr.number <= right.repr.number,
                }),
                else => {
                    // Do nothing and let the later code handle this case.
                },
            }
        }

        if (@as(std.meta.Tag(Represent), left.repr) == .string and
            @as(std.meta.Tag(Represent), right.repr) == .string)
        {
            if (binary.operator.type_ == .plus) {
                const combined = try std.mem.concat(arena, u8, &[_][]const u8{ left.repr.string, right.repr.string });
                return Value.init(arena, Represent{ .string = combined });
            }
        }

        switch (binary.operator.type_) {
            .equal_equal => return Value.init(arena, Represent{ .bool_ = left.equal(right) }),
            .bang_equal => return Value.init(arena, Represent{ .bool_ = !left.equal(right) }),
            .minus, .star, .slash, .less, .less_equal, .greater, .greater_equal => {
                self.diagnostic = Diagnostic{
                    .line_number = binary.operator.line,
                    .input = binary.operator.lexeme,
                    .message = "operands must be numbers.",
                };
                return RuntimeError.IllegalBinaryOperand;
            },
            .plus => {
                self.diagnostic = Diagnostic{
                    .line_number = binary.operator.line,
                    .input = binary.operator.lexeme,
                    .message = "operands must be two numbers or two strings.",
                };
                return RuntimeError.IllegalBinaryOperand;
            },
            else => {
                // Do nothing and let the later code handle this case.
            },
        }

        return RuntimeError.Unimplemented;
    }

    fn evaluateAssignment(self: *Self, assignment: ast.Assignment, arena: std.mem.Allocator) RuntimeError!*Value {
        var value = try self.evaluateExpression(assignment.value, arena);
        errdefer value.deinit();

        // Signal that the value only lives in arena allocation for now. move it into global allocator.
        if (value.ref == 1) {
            value = try Value.init(self.allocator, try value.repr.clone(self.allocator));
        }

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
                        .line_number = assignment.variable.name.line,
                        .input = assignment.variable.name.lexeme,
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
