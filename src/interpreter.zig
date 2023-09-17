const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const enviroment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const main = @import("main.zig");

pub const Value = union(enum) {
    const Self = @This();

    string: []const u8, // We expect the memory of this slice to be owned by our interpreter or by the environment.
    number: f64,
    bool_: bool,
    nil: void,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            else => {},
        }
    }

    // TODO I'm not happy with the current state of the string handling in my code. Think of a better sheme that
    //      scales better and isn't as brittle.
    pub fn deepCopy(self: Self, allocator: std.mem.Allocator) !Self {
        var result = self;

        switch (self) {
            .string => |s| {
                const new_str = try allocator.dupe(u8, s);
                result.string = new_str;
            },
            else => {},
        }
        return result;
    }

    // Strange rule according to Lex definition...
    fn isTruthy(self: Self) bool {
        switch (self) {
            .bool_ => |b| return b,
            .number => return true,
            .nil => return false,
            .string => return true,
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
        }
    }
};

pub const RuntimError = error{ MinusOperand, UndefinedVariable, IllegalBinaryOperand, Unimplemented, OutOfMemory } || std.fs.File.WriteError;

pub const Diagonstic = struct {
    token_: token.Token, // Associated token to get the operation and the line number.
    message: []const u8,
};

pub const Interpreter = struct {
    const Self = @This();

    environments: std.ArrayListUnmanaged(enviroment.Environment),
    allocator: std.mem.Allocator,
    current_environment: u16 = 0,
    diagnostic: ?Diagonstic = null,

    pub fn deinit(self: *Self) void {
        for (self.environments.items) |*env| {
            env.deinit();
        }
        self.environments.deinit(self.allocator);
    }

    pub fn new(allocator: std.mem.Allocator) !Self {
        const env = try enviroment.Environment.init(allocator);
        var environments = try std.ArrayListUnmanaged(enviroment.Environment).initCapacity(allocator, 5);
        environments.appendAssumeCapacity(env);
        return Interpreter{ .allocator = allocator, .environments = environments };
    }

    pub fn run(self: *Self, stdout: std.fs.File.Writer, input: []const u8) !void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();

        var scan = scanner.Scanner{ .source = input };
        const tokens = try scan.scanTokens(arena.allocator());

        var parse = parser.Parser{ .tokens = tokens.items };
        const parse_tree = try parse.parseInto(arena.allocator());

        self.execute(stdout, parse_tree.statements.items) catch |err| {
            if (err == RuntimError.Unimplemented) {
                _ = try stdout.write("Hit unimplemented part of the interpreter.");
            } else {
                const diagnostic = self.diagnostic.?;
                main.reportError(diagnostic.token_.line, &[_][]const u8{ "Runtime Error:", diagnostic.token_.lexeme, " ", diagnostic.message });
            }
            return err;
        };
    }

    fn execute(self: *Self, output: std.fs.File.Writer, statements: []ast.Stmt) RuntimError!void {
        for (statements) |stmt| {
            try self.executeStatement(output, stmt);
        }
    }

    fn executeStatement(self: *Self, output: std.fs.File.Writer, stmt: ast.Stmt) RuntimError!void {
        switch (stmt) {
            .expr => |e| {
                var value = try self.evaluateExpression(e.*);
                defer value.deinit(self.allocator);
            },
            .print => |e| {
                // TODO do I want to propagate print errors upwards?
                var value = try self.evaluateExpression(e.*);
                defer value.deinit(self.allocator);

                switch (value) {
                    .number => |n| try output.print("{d}", .{n}),
                    .string => |s| try output.print("{s}", .{s}),
                    .bool_ => |b| try output.print("{}", .{b}),
                    .nil => _ = try output.write("nil"),
                }
                _ = try output.write("\n");
            },
            .var_decl => |decl| {
                var val: Value = Value.nil;
                defer val.deinit(self.allocator);

                if (decl.initializer != null) {
                    val = try self.evaluateExpression(decl.initializer.?.*);
                }

                var current_env = &self.environments.items[self.current_environment];
                try current_env.define(decl.name.lexeme, Value.nil);
            },
        }
    }

    fn evaluateExpression(self: *Self, expr: ast.Expr) RuntimError!Value {
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
                return try self.evaluateExpression(g.*);
            },
            .unary => |u| {
                var right = try self.evaluateExpression(u.right.*);
                defer right.deinit(self.allocator);

                if (u.operator.type_ == token.Type.bang) {
                    return Value{ .bool_ = !right.isTruthy() };
                }

                if (u.operator.type_ == token.Type.minus) {
                    if (@as(std.meta.Tag(Value), right) == .number) {
                        return Value{ .number = right.number };
                    }

                    self.diagnostic = Diagonstic{
                        .token_ = u.operator,
                        .message = "operand must be a number.",
                    };
                    return RuntimError.MinusOperand;
                }
                return RuntimError.Unimplemented;
            },
            .binary => |b| {
                var left = try self.evaluateExpression(b.left.*);
                var right = try self.evaluateExpression(b.right.*);
                defer left.deinit(self.allocator);
                defer right.deinit(self.allocator);

                if (@as(std.meta.Tag(Value), left) == .number and @as(std.meta.Tag(Value), right) == .number) {
                    switch (b.operator.type_) {
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
                    if (b.operator.type_ == .plus) {
                        const combined = try std.mem.concat(self.allocator, u8, &[_][]const u8{ left.string, right.string });
                        return Value{ .string = combined };
                    }
                }

                switch (b.operator.type_) {
                    .equal_equal => return Value{ .bool_ = left.equal(right) },
                    .bang_equal => return Value{ .bool_ = !left.equal(right) },
                    .minus, .star, .slash, .less, .less_equal, .greater, .gerater_equal => {
                        self.diagnostic = Diagonstic{
                            .token_ = b.operator,
                            .message = "operands must be numbers.",
                        };
                        return RuntimError.IllegalBinaryOperand;
                    },
                    .plus => {
                        self.diagnostic = Diagonstic{
                            .token_ = b.operator,
                            .message = "operands must be two numbers or two strings.",
                        };
                        return RuntimError.IllegalBinaryOperand;
                    },
                    else => {
                        // TODO
                    },
                }

                return RuntimError.Unimplemented;
            },
            .variable => |variable| {
                var environment_index = self.environments.items.len;
                const val = found: while (environment_index > 0) {
                    environment_index -= 1;
                    const val = self.environments.items[environment_index].get(variable, self.allocator);
                    if (val != null) {
                        break :found val;
                    }
                } else null;

                if (val == null) {
                    self.diagnostic = Diagonstic{
                        .token_ = variable,
                        .message = "is an undefined variable.",
                    };
                    return RuntimError.UndefinedVariable;
                }

                return val.?;
            },
            .assign => |assignment| {
                var value = try self.evaluateExpression(assignment.value.*);

                var environment_index = self.environments.items.len;
                while (environment_index > 0) {
                    environment_index -= 1;
                    var current_environment = &self.environments.items[environment_index];
                    current_environment.assign(assignment.name, value) catch |err| {
                        switch (err) {
                            // Go up one level in the environment chain.
                            enviroment.EnvironmentError.VariableNotFound => {
                                continue;
                            },
                            enviroment.EnvironmentError.OutOfMemory => {
                                return RuntimError.OutOfMemory;
                            },
                        }
                    };

                    // Assignment was successfull so we can return the value
                    return value;
                }

                // No assignment possible so we report an error.
                self.diagnostic = Diagonstic{
                    .token_ = assignment.name,
                    .message = "is an undefined variable.",
                };
                return RuntimError.UndefinedVariable;
            },
        }

        return RuntimError.Unimplemented;
    }
};
