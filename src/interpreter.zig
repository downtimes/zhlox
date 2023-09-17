const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");
const enviroment = @import("environment.zig");
const scanner = @import("scanner.zig");
const parser = @import("parser.zig");
const main = @import("main.zig");

pub const Value = union(enum) {
    const Self = @This();

    string: []const u8,
    number: f64,
    bool_: bool,
    nil: void,

    pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .string => |s| allocator.free(s),
            else => {},
        }
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

    global_env: enviroment.Environment,
    allocator: std.mem.Allocator,
    diagnostic: ?Diagonstic = null,

    pub fn deinit(self: *Self) void {
        // TODO the global env does not own the values atm. We need to also free the values in the environment.
        self.global_env.deinit();
    }

    pub fn new(allocator: std.mem.Allocator) !Self {
        const env = enviroment.Environment{
            .arena = try allocator.create(std.heap.ArenaAllocator),
            .values = std.StringHashMapUnmanaged(Value){},
        };
        env.arena.* = std.heap.ArenaAllocator.init(allocator);

        return Interpreter{ .allocator = allocator, .global_env = env };
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
                value.deinit(self.allocator);
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
                // TODO workaround for a compiler bug https://github.com/ziglang/zig/issues/10253
                //      We want to initialize a variable to Value.nil, but when we do we can't change the type of value
                //      afterwards. Therefore we need do to seperate define calls here.
                if (decl.initializer != null) {
                    const value = try self.evaluateExpression(decl.initializer.?.*);
                    try self.global_env.define(decl.name.lexeme, value);
                    return;
                }

                try self.global_env.define(decl.name.lexeme, Value.nil);
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
                        var owned_string = try self.allocator.alloc(u8, s.len);
                        std.mem.copyForwards(u8, owned_string, s);
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
                errdefer right.deinit(self.allocator);

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
                errdefer left.deinit(self.allocator);
                errdefer right.deinit(self.allocator);

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
                        defer left.deinit(self.allocator);
                        defer right.deinit(self.allocator);

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
                std.debug.print("Getting variable {s}", .{variable.lexeme});
                const val = self.global_env.get(variable);
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
                const value = try self.evaluateExpression(assignment.value.*);
                self.global_env.assign(assignment.name, value) catch |err| {
                    switch (err) {
                        enviroment.EnvironmentError.VariableNotFound => {
                            self.diagnostic = Diagonstic{
                                .token_ = assignment.name,
                                .message = "is an undefined variable.",
                            };
                            return RuntimError.UndefinedVariable;
                        },
                        enviroment.EnvironmentError.OutOfMemory => {
                            return RuntimError.OutOfMemory;
                        },
                    }
                };
                return value;
            },
        }

        return RuntimError.Unimplemented;
    }
};
