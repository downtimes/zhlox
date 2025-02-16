const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const environment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const resolver = @import("resolver.zig");
const main = @import("main.zig");
const constants = @import("constants.zig");
const Allocator = std.mem.Allocator;

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

    fn bind(self: Function, instance: *Rc, allocator: Allocator) !Value {
        var bound = try environment.Environment.create_with_parent(allocator, self.env);
        // This call saves the fields we had when we look up the method.
        // I'm not sure this is semantically the same as what lox does.
        // What happens when a field is later added to the instance in lox?
        try bound.define(constants.this, instance);
        bound.refs += 1;
        return Value{ .temp = Represent{
            .function = Function{
                .declaration = self.declaration,
                .env = bound,
                .constructor = self.constructor,
            },
        } };
    }
};

const Class = struct {
    name: []const u8, // This memory is not owned, it points into input_scratch.
    super: ?*Rc,
    methods: std.StringHashMap(Function),

    fn findMethod(self: Class, name: []const u8) ?Function {
        const method = self.methods.get(name);
        if (method) |m| {
            return m;
        }
        if (self.super) |s| {
            return s.repr.class.findMethod(name);
        }
        return null;
    }
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
    ret: void,
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
                if (c.super) |s| {
                    s.deinit();
                }
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
            .class => |c| {
                if (c.super) |s| {
                    s.ref();
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
            .string, .number, .bool_, .nil, .instance, .ret => return ValueError.NonCallable,
            .builtin => |f| {
                if (f.arity != arguments.len) {
                    return ValueError.Arity;
                }
                return f.function(arena, arguments);
            },
            .class => |c| {
                const constructor = c.methods.get(constants.constructor);
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
                    var initialization = try construct.bind(instance.rc, interpreter.allocator);
                    initialization.temp.function.constructor = true;
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
                    var promoted = switch (argument) {
                        .temp => try argument.promote(interpreter.allocator),
                        .rc => |rc| blk: {
                            rc.ref();
                            break :blk argument;
                        },
                    };
                    defer promoted.deinit();
                    try function_env.define(param.lexeme, promoted.rc);
                }

                var value = try interpreter.executeBlock(func.body, function_env);
                // Ensure constructors always return the this object when called.
                if (f.constructor) {
                    value.deinit();
                    // Since we know that we are a constructor, we know that the instance
                    // is bound to this. Therefore we can unwrap the option here.
                    return Value{ .rc = f.env.getInParent(0, constants.this).? };
                }
                return value;
            },
        }
    }

    // Strange rule according to Lox definition...
    fn isTruthy(self: Self) bool {
        switch (self) {
            .bool_ => |b| return b,
            .nil, .ret => return false,
            inline else => return true,
        }
    }

    fn equal(self: Self, other: Self) bool {
        switch (self) {
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
    IllegalSuperClass,
};

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
    ) Allocator.Error!void {
        var scan = scanner.Scanner{ .source = input };
        const tokens = try scan.scanTokens(input_scratch);
        if (scan.had_error) {
            return;
        }

        var parse = parser.Parser{ .tokens = tokens.items };
        const parse_tree = try parse.parseInto(input_scratch);
        if (parse.had_error) {
            return;
        }

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

    fn execute(self: *Self, statements: []const ast.Stmt) Allocator.Error!void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        for (statements) |*stmt| {
            _ = arena.reset(std.heap.ArenaAllocator.ResetMode.retain_capacity);
            var val = self.executeStatement(stmt, arena.allocator()) catch |err| {
                if (err == RuntimeError.Unimplemented) {
                    _ = self.output.write("Hit unimplemented part of the interpreter.") catch unreachable;
                } else if (err == RuntimeError.OutOfMemory) {
                    return RuntimeError.OutOfMemory;
                } else {
                    const diagnostic = self.diagnostic.?;
                    main.reportError(
                        diagnostic.line_number,
                        &[_][]const u8{
                            "Runtime error: '",
                            diagnostic.input,
                            "' ",
                            diagnostic.message,
                        },
                    );
                }
                return;
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
            if (val.repr() != Represent.nil or
                val.repr() == Represent.ret)
            {
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
                    .number => |n| self.output.print("{d}", .{n}) catch unreachable,
                    .string => |s| self.output.print("{s}", .{s}) catch unreachable,
                    .bool_ => |b| self.output.print("{}", .{b}) catch unreachable,
                    .nil => _ = self.output.write("nil") catch unreachable,
                    .ret => _ = self.output.write("return") catch unreachable,
                    .builtin => _ = self.output.write("<native fn>") catch unreachable,
                    .class => |c| self.output.print("class {s}", .{c.name}) catch unreachable,
                    .instance => |i| self.output.print("{s} instance", .{i.class.name}) catch unreachable,
                    .function => |f| {
                        self.output.print("<fn {s}>", .{f.declaration.name.lexeme}) catch unreachable;
                    },
                }
                _ = self.output.write("\n") catch unreachable;
            },
            .while_ => |*w| {
                var cond = try self.evaluateExpression(&w.condition, arena);
                defer cond.deinit();

                while (cond.isTruthy()) {
                    for (w.body) |*s| {
                        var value = try self.executeStatement(s, arena);
                        if (value.repr() != Represent.nil or
                            value.repr() == Represent.ret)
                        {
                            return value.promote(self.allocator);
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
                var super: ?*Rc = null;
                if (c.super) |s| {
                    var class = try self.getVariable(s);
                    errdefer class.deinit();
                    if (@as(std.meta.Tag(Represent), class.repr()) != .class) {
                        self.diagnostic = Diagnostic{
                            .input = s.name.lexeme,
                            .line_number = s.name.line,
                            .message = "superclass must be a class.",
                        };
                        return RuntimeError.IllegalSuperClass;
                    }

                    // Don't call class.deinit() since the super variable takes
                    // over the reference count of the Rc.
                    class = try class.promote(self.allocator);
                    super = class.rc;
                }
                defer if (super) |s| s.deinit();

                nil = try nil.promote(self.allocator);
                errdefer nil.deinit();
                try self.active_environment.define(c.name.lexeme, nil.rc);

                if (super) |s| {
                    self.active_environment = try environment.Environment.create_with_parent(self.allocator, self.active_environment);
                    try self.active_environment.define(constants.super, s);
                }

                var methods = std.StringHashMap(Function).init(self.allocator);
                try methods.ensureTotalCapacity(@intCast(c.methods.len));
                for (c.methods) |*m| {
                    const constructor = std.mem.eql(u8, m.name.lexeme, constants.constructor);
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
                        .super = super,
                        .methods = methods,
                    },
                });
                defer class.deinit();

                if (super != null) {
                    self.active_environment = self.active_environment.parent.?;
                }

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
                return Value{ .temp = Represent.ret };
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

                        const method = i.class.findMethod(g.name.lexeme);
                        if (method) |m| {
                            return m.bind(object.rc, self.allocator);
                        }

                        self.diagnostic = Diagnostic{
                            .line_number = g.name.line,
                            .input = g.name.lexeme,
                            .message = "unknown property.",
                        };
                        return RuntimeError.UnknownProperty;
                    },
                    else => {},
                }
                self.diagnostic = Diagnostic{
                    .line_number = g.name.line,
                    .input = g.name.lexeme,
                    .message = "only instances have properties.",
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
                    .message = "only instances have fields.",
                };
                return RuntimeError.NonInstance;
            },
            .super => |super| {
                // We know super should not be in global environment and should not be the current active environment.
                std.debug.assert(super.keyword.resolve_steps != null and super.keyword.resolve_steps.? > 0);
                // Assumes the environment in which this is bound is the environment under which super is bound.
                // Also assumes that if we find an ast with super the super class and this instance are 100% bound.
                var class = self.active_environment.getInParent(super.keyword.resolve_steps.?, constants.super).?;
                var object = self.active_environment.getInParent(super.keyword.resolve_steps.? - 1, constants.this).?;
                defer class.deinit();
                defer object.deinit();
                std.debug.assert(@as(std.meta.Tag(Represent), class.repr) == .class);
                std.debug.assert(@as(std.meta.Tag(Represent), object.repr) == .instance);

                const method = class.repr.class.findMethod(super.method.lexeme);
                if (method) |m| {
                    return m.bind(object, self.allocator);
                }

                self.diagnostic = Diagnostic{
                    .line_number = super.method.line,
                    .input = super.method.lexeme,
                    .message = "undefined property.",
                };
                return RuntimeError.UndefinedVariable;
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
        const rrepr = right.repr();
        const lrepr = left.repr();

        if (@as(std.meta.Tag(Represent), lrepr) == .number and
            @as(std.meta.Tag(Represent), rrepr) == .number)
        {
            switch (binary.operator.type_) {
                .minus => return Value{ .temp = Represent{
                    .number = lrepr.number - rrepr.number,
                } },
                // TODO possibly create runtime error here when dividing by 0 when nominator not 0 itself,
                //      currently we return inf since zig does the same. Need to look into how other lox
                //      interpreters handle this case
                .slash => return Value{ .temp = Represent{
                    .number = lrepr.number / rrepr.number,
                } },
                .star => return Value{ .temp = Represent{
                    .number = lrepr.number * rrepr.number,
                } },
                .plus => return Value{ .temp = Represent{
                    .number = lrepr.number + rrepr.number,
                } },
                .greater => return Value{ .temp = Represent{
                    .bool_ = lrepr.number > rrepr.number,
                } },
                .greater_equal => return Value{ .temp = Represent{
                    .bool_ = lrepr.number >= rrepr.number,
                } },
                .less => return Value{ .temp = Represent{
                    .bool_ = lrepr.number < rrepr.number,
                } },
                .less_equal => return Value{ .temp = Represent{
                    .bool_ = lrepr.number <= rrepr.number,
                } },
                else => {
                    // Do nothing and let the later code handle this case.
                },
            }
        }

        if (@as(std.meta.Tag(Represent), lrepr) == .string and
            @as(std.meta.Tag(Represent), rrepr) == .string)
        {
            if (binary.operator.type_ == .plus) {
                const combined = try std.mem.concat(arena, u8, &[_][]const u8{ lrepr.string, rrepr.string });
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
