const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const resolver = @import("resolver.zig");
const main = @import("main.zig");
const Allocator = std.mem.Allocator;

const constructor_constant: []const u8 = "init";

const ValueError = error{ NonCallable, Arity };
const CallError = ValueError || RuntimeError;

const Builtin = struct {
    arity: u8,
    function: *const fn (Allocator, []Value) Allocator.Error!Value,
};

const Function = struct {
    declaration: *const ast.Function,
    env: *environment.Environment,
    constructor: bool,
};

const Class = struct {
    name: []const u8, // This memory is not owned, it points into input_scratch.
    methods: std.StringHashMap(Function),
};

const Instance = struct {
    class: Class,
    fields: std.StringHashMap(*Rc),
};

// Add local, global differentiation.
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

    fn deinit(self: *Self, allocator: ?Allocator) void {
        switch (self.*) {
            .string => |s| if (allocator) |a| {
                a.free(s);
            },
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
                var fields = i.fields.valueIterator();
                while (fields.next()) |f| {
                    f.*.deinit();
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
    fn clone(self: Self, allocator: Allocator) Allocator.Error!Self {
        var result = self;

        switch (self) {
            .string => |s| {
                result.string = try allocator.dupe(u8, s);
            },
            .instance => |i| {
                result.instance.fields = try i.fields.cloneWithAllocator(allocator);
                var fields = result.instance.fields.valueIterator();
                while (fields.next()) |f| {
                    f.*.ref();
                }
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
        arguments: []Value,
        interpreter: *Interpreter,
        arena: Allocator,
    ) CallError!Value {
        switch (self.*) {
            .string, .number, .bool_, .nil, .instance => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arena, arguments);
            },
            .class => |c| {
                const constructor = c.methods.get(constructor_constant);
                var arity: usize = 0;
                if (constructor) |construct| {
                    arity = construct.declaration.params.len;
                }
                if (arguments.len != arity) {
                    return ValueError.Arity;
                }

                var instance = try Value.createRc(
                    interpreter.allocator,
                    Represent{
                        .instance = Instance{
                            .class = c,
                            .fields = std.StringHashMap(*Rc).init(arena),
                        },
                    },
                );

                if (constructor) |construct| {
                    defer instance.deinit();
                    var bound = try environment.Environment.create_with_parent(interpreter.allocator, construct.env);
                    try bound.define(scanner.this, instance.rc);
                    bound.refs += 1;
                    var initialization = Value{ .temp = Represent{
                        .function = Function{
                            .declaration = construct.declaration,
                            .env = bound,
                            .constructor = true,
                        },
                    } };
                    defer initialization.deinit();
                    return try initialization.call(arguments, interpreter, arena);
                } else {
                    return instance;
                }
            },
            .function => |*f| {
                const func = f.declaration;
                if (func.params.len != arguments.len) {
                    return ValueError.Arity;
                }

                var function_env = try environment.Environment.create_with_parent(interpreter.allocator, f.env);
                defer function_env.clean_unused(true);

                for (func.params, arguments) |param, argument| {
                    var promoted = try argument.promote(interpreter.allocator);
                    defer promoted.deinit();
                    try function_env.define(param.lexeme, promoted.rc);
                }

                var value = try interpreter.executeBlock(func.body, function_env);
                // Ensure constructors always return the this object when called.
                if (f.constructor) {
                    value.deinit();
                    // Since we know that we are a constructor, we know that the instance
                    // is bound to this. Therefore we can unwrap the option here.
                    return Value{ .rc = f.env.getInParent(0, scanner.this).? };
                }
                return value;
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

// Always in global memory and reference counted.
pub const Rc = struct {
    allocator: Allocator,
    repr: Represent,
    refs: u8,

    pub fn ref(self: *Rc) void {
        self.refs += 1;
    }

    pub fn deinit(self: *Rc) void {
        std.debug.assert(self.refs > 0);
        self.refs -= 1;
        if (self.refs == 0) {
            self.repr.deinit(self.allocator);
            const allocator = self.allocator;
            allocator.destroy(self);
        }
    }
};

pub const Value = union(enum) {
    temp: Represent,
    rc: *Rc,

    fn createRc(allocator: Allocator, represent: Represent) !Value {
        const rc = try allocator.create(Rc);
        rc.repr = try represent.clone(allocator);
        rc.allocator = allocator;
        rc.refs = 1;
        return Value{ .rc = rc };
    }

    fn equal(self: Value, other: Value) bool {
        const srepr = switch (self) {
            .temp => |r| r,
            .rc => |rc| rc.repr,
        };
        const orepr = switch (other) {
            .temp => |r| r,
            .rc => |rc| rc.repr,
        };
        return srepr.equal(orepr);
    }

    fn isTruthy(self: Value) bool {
        switch (self) {
            .temp => |r| return r.isTruthy(),
            .rc => |rc| return rc.repr.isTruthy(),
        }
    }

    fn call(
        self: *Value,
        arguments: []Value,
        interpreter: *Interpreter,
        arena: Allocator,
    ) CallError!Value {
        switch (self.*) {
            .temp => |*r| return r.call(arguments, interpreter, arena),
            .rc => |rc| return rc.repr.call(arguments, interpreter, arena),
        }
    }

    fn promote(self: Value, allocator: Allocator) !Value {
        switch (self) {
            .temp => |t| {
                const rc = try allocator.create(Rc);
                rc.repr = try t.clone(allocator);
                rc.allocator = allocator;
                rc.refs = 1;
                return Value{ .rc = rc };
            },
            .rc => return self,
        }
    }

    fn repr(self: Value) Represent {
        return switch (self) {
            .temp => |r| r,
            .rc => |rc| rc.repr,
        };
    }

    fn deinit(self: *Value) void {
        switch (self.*) {
            .temp => |*r| {
                r.deinit(null);
            },
            .rc => |rc| {
                rc.deinit();
            },
        }
    }
};

fn builtinClock(arena: Allocator, arguments: []Value) !Value {
    _ = arguments;
    _ = arena;
    const time: f64 = @floatFromInt(@divTrunc(std.time.milliTimestamp(), 1000));
    return Value{ .temp = Represent{ .number = time } };
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
} || std.io.AnyWriter.Error;

pub const Diagnostic = struct {
    line_number: u32,
    input: []const u8, // Not owned by diagnostic, expected to be in input scratch.
    message: []const u8, // Not owned by diagnostic, expected to be static str.
};

pub const Interpreter = struct {
    const Self = @This();

    global_environment: *environment.Environment,
    active_environment: *environment.Environment,
    output: std.io.AnyWriter,
    allocator: Allocator,
    diagnostic: ?Diagnostic = null,

    pub fn deinit(self: *Self) void {
        self.global_environment.deinit();
    }

    pub fn new(allocator: Allocator) !Self {
        var interpreter = Interpreter{
            .output = undefined,
            .allocator = allocator,
            .global_environment = try environment.Environment.create(allocator),
            .active_environment = undefined,
        };
        errdefer interpreter.deinit();

        var clock = try Value.createRc(allocator, Represent{
            .builtin = Builtin{ .arity = 0, .function = &builtinClock },
        });
        defer clock.deinit();
        try interpreter.global_environment.define("clock", clock.rc);

        return interpreter;
    }

    pub fn run(
        self: *Self,
        output: anytype,
        input: []const u8,
        input_scratch: Allocator,
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
            try res.resolve(parse_tree);
            if (res.found_error) {
                return;
            }
        }

        self.active_environment = self.global_environment;
        self.output = output.any();
        try self.execute(parse_tree);
    }

    fn execute(self: *Self, statements: []const ast.Stmt) RuntimeError!void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        for (statements) |*stmt| {
            _ = arena.reset(std.heap.ArenaAllocator.ResetMode.retain_capacity);
            var val = self.executeStatement(stmt, arena.allocator()) catch |err| {
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
            val.deinit();
        }
    }

    fn executeBlock(
        self: *Self,
        statements: []const ast.Stmt,
        env: *environment.Environment,
    ) RuntimeError!Value {
        var block_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer block_arena.deinit();

        const previous_environment = self.active_environment;
        defer self.active_environment = previous_environment;

        self.active_environment = env;
        for (statements) |*statement| {
            var val = try self.executeStatement(statement, block_arena.allocator());
            if (val.repr() != Represent.nil) {
                return val.promote(self.allocator);
            } else {
                val.deinit();
            }
        }
        return Value{ .temp = Represent.nil };
    }

    fn executeStatement(
        self: *Self,
        stmt: *const ast.Stmt,
        arena: Allocator,
    ) RuntimeError!Value {
        var nil = Value{ .temp = Represent.nil };
        switch (stmt.*) {
            .cond => |*c| {
                var cond = try self.evaluateExpression(&c.condition, arena);
                defer cond.deinit();
                if (cond.isTruthy()) {
                    return try self.executeStatement(c.then, arena);
                } else if (c.els != null) {
                    return try self.executeStatement(c.els.?, arena);
                }
            },
            .expr => |*e| {
                var val = try self.evaluateExpression(e, arena);
                val.deinit();
            },
            .print => |*e| {
                var value = try self.evaluateExpression(e, arena);
                defer value.deinit();
                switch (value.repr()) {
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
            },
            .while_ => |*w| {
                var cond = try self.evaluateExpression(&w.condition, arena);
                defer cond.deinit();

                while (cond.isTruthy()) {
                    for (w.body) |*s| {
                        var value = try self.executeStatement(s, arena);
                        if (value.repr() != Represent.nil) {
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
                var val = nil;
                defer val.deinit();
                if (decl.initializer) |*initializer| {
                    val = try self.evaluateExpression(initializer, arena);
                }

                // Make sure we have a reference counted value here.
                val = try val.promote(self.allocator);
                try self.active_environment.define(decl.name.lexeme, val.rc);
            },
            .block => |statements| {
                // Open new environment for each nested block.
                const new_env = try environment.Environment.create_with_parent(self.allocator, self.active_environment);
                defer new_env.clean_unused(false);

                return try self.executeBlock(statements, new_env);
            },
            .class => |*c| {
                nil = try nil.promote(self.allocator);
                errdefer nil.deinit();
                try self.active_environment.define(c.name.lexeme, nil.rc);

                var methods = std.StringHashMap(Function).init(self.allocator);
                try methods.ensureTotalCapacity(@intCast(c.methods.len));
                for (c.methods) |*m| {
                    const constructor = std.mem.eql(u8, m.name.lexeme, constructor_constant);
                    const function = Function{
                        .declaration = m,
                        .env = self.active_environment,
                        .constructor = constructor,
                    };
                    try methods.put(m.name.lexeme, function);
                }

                var class = try Value.createRc(self.allocator, Represent{
                    .class = Class{
                        .name = c.name.lexeme,
                        .methods = methods,
                    },
                });
                defer class.deinit();
                self.active_environment.assign(c.name.lexeme, class.rc) catch {
                    // Do nothing since we defined the variable before so this case is never hit.
                };
                return nil;
            },
            .function => |*f| {
                const function = Function{
                    .declaration = f,
                    .env = self.active_environment,
                    .constructor = false,
                };
                // TODO do I also want to allow functions to be shadowed? This could potentially be confusing no?
                //      Probably needed though for proper compatibility with lox.
                var fun = try Value.createRc(self.allocator, Represent{ .function = function });
                defer fun.deinit();
                try self.active_environment.define(f.name.lexeme, fun.rc);
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

    fn evaluateExpression(
        self: *Self,
        expr: *const ast.Expr,
        arena: Allocator,
    ) RuntimeError!Value {
        switch (expr.*) {
            .literal => |l| {
                return switch (l) {
                    .bool_ => |b| Value{ .temp = Represent{ .bool_ = b } },
                    .number => |n| Value{ .temp = Represent{ .number = n } },
                    // The value is not duped here yet, it will happen when the
                    // block goes out of scope or the value is put into an environment
                    // via variable initialization.
                    .string => |s| Value{ .temp = Represent{ .string = s } },
                    .nil => Value{ .temp = Represent.nil },
                };
            },
            .grouping => |g| {
                return self.evaluateExpression(g, arena);
            },
            .logical => |l| {
                var left = try self.evaluateExpression(l.left, arena);

                if (l.operator.type_ == token.Type.and_) {
                    if (!left.isTruthy()) return left;
                } else {
                    if (left.isTruthy()) return left;
                }
                left.deinit();

                return self.evaluateExpression(l.right, arena);
            },
            .unary => |u| {
                var right = try self.evaluateExpression(u.right, arena);
                defer right.deinit();

                if (u.operator.type_ == token.Type.bang) {
                    return Value{ .temp = Represent{ .bool_ = !right.isTruthy() } };
                }

                if (u.operator.type_ == token.Type.minus) {
                    if (@as(std.meta.Tag(Represent), right.repr()) == .number) {
                        return Value{ .temp = Represent{ .number = -right.repr().number } };
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
                var object = try self.evaluateExpression(g.object, arena);
                defer object.deinit();
                object = try object.promote(self.allocator);

                switch (object.rc.repr) {
                    .instance => |*i| {
                        const val = i.fields.get(g.name.lexeme);
                        if (val) |v| {
                            v.ref();
                            return Value{ .rc = v };
                        }

                        const method = i.class.methods.get(g.name.lexeme);
                        if (method) |m| {
                            var bound = try environment.Environment.create_with_parent(self.allocator, m.env);
                            // This call saves the fields we had when we we get the method
                            // I'm not sure this is semantically the same as what lox should be
                            // what happens when a field is later added to the instance?
                            try bound.define(scanner.this, object.rc);
                            bound.refs += 1;
                            return Value{ .temp = Represent{
                                .function = Function{
                                    .declaration = m.declaration,
                                    .env = bound,
                                    .constructor = m.constructor,
                                },
                            } };
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
                var object = try self.evaluateExpression(s.object, arena);
                defer object.deinit();
                object = try object.promote(self.allocator);

                switch (object.rc.repr) {
                    .instance => |*i| {
                        var value = try self.evaluateExpression(s.value, arena);
                        errdefer value.deinit();
                        value = try value.promote(self.allocator);
                        const resp = try i.fields.getOrPut(s.name.lexeme);
                        value.rc.ref();
                        if (resp.found_existing) {
                            resp.value_ptr.*.deinit();
                        }
                        resp.value_ptr.* = value.rc;
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
                var arguments = std.ArrayList(Value).init(arena);
                defer for (arguments.items) |*arg| {
                    arg.deinit();
                };

                for (call.arguments) |*arg| {
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

    fn getVariable(self: *Self, variable: ast.Variable) RuntimeError!Value {
        var val: ?*Rc = undefined;
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

        return Value{ .rc = val.? };
    }

    fn evaluateBinary(
        self: *Self,
        binary: ast.Binary,
        arena: Allocator,
    ) RuntimeError!Value {
        var left = try self.evaluateExpression(binary.left, arena);
        defer left.deinit();
        var right = try self.evaluateExpression(binary.right, arena);
        defer right.deinit();

        if (@as(std.meta.Tag(Represent), left.repr()) == .number and
            @as(std.meta.Tag(Represent), right.repr()) == .number)
        {
            switch (binary.operator.type_) {
                .minus => return Value{ .temp = Represent{
                    .number = left.repr().number - right.repr().number,
                } },
                // TODO possibly create runtime error here when dividing by 0 when nominator not 0 itself,
                //      currently we return inf since zig does the same. Need to look into how other lox
                //      interpreters handle this case
                .slash => return Value{ .temp = Represent{
                    .number = left.repr().number / right.repr().number,
                } },
                .star => return Value{ .temp = Represent{
                    .number = left.repr().number * right.repr().number,
                } },
                .plus => return Value{ .temp = Represent{
                    .number = left.repr().number + right.repr().number,
                } },
                .greater => return Value{ .temp = Represent{
                    .bool_ = left.repr().number > right.repr().number,
                } },
                .greater_equal => return Value{ .temp = Represent{
                    .bool_ = left.repr().number >= right.repr().number,
                } },
                .less => return Value{ .temp = Represent{
                    .bool_ = left.repr().number < right.repr().number,
                } },
                .less_equal => return Value{ .temp = Represent{
                    .bool_ = left.repr().number <= right.repr().number,
                } },
                else => {
                    // Do nothing and let the later code handle this case.
                },
            }
        }

        if (@as(std.meta.Tag(Represent), left.repr()) == .string and
            @as(std.meta.Tag(Represent), right.repr()) == .string)
        {
            if (binary.operator.type_ == .plus) {
                const combined = try std.mem.concat(arena, u8, &[_][]const u8{ left.repr().string, right.repr().string });
                return Value{ .temp = Represent{ .string = combined } };
            }
        }

        switch (binary.operator.type_) {
            .equal_equal => return Value{ .temp = Represent{ .bool_ = left.equal(right) } },
            .bang_equal => return Value{ .temp = Represent{ .bool_ = !left.equal(right) } },
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

    fn evaluateAssignment(
        self: *Self,
        assignment: ast.Assignment,
        arena: Allocator,
    ) RuntimeError!Value {
        var value = try self.evaluateExpression(assignment.value, arena);
        value = try value.promote(self.allocator);

        var res: environment.EnvironmentError!void = undefined;
        if (assignment.variable.resolve_steps) |steps| {
            res = self.active_environment.assignInParent(steps, assignment.variable.name.lexeme, value.rc);
        } else {
            res = self.global_environment.assign(assignment.variable.name.lexeme, value.rc);
        }

        if (res) |_| {
            return value;
        } else |err| {
            value.deinit();
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
