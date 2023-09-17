const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");

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

pub const RuntimError = error{ MinusOperand, IllegalBinaryOperand, Unimplemented, OutOfMemory };

pub const Diagonstic = struct {
    token_: token.Token, // Associated token to get the operation and the line number.
    message: []const u8,
};

pub const Interpreter = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    diagnostic: ?Diagonstic = null,

    pub fn execute(self: *Self, expr: ast.Expr) RuntimError!Value {
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
                return try self.execute(g.*);
            },
            .unary => |u| {
                const right = try self.execute(u.right.*);
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
                var left = try self.execute(b.left.*);
                var right = try self.execute(b.right.*);
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
        }

        return RuntimError.Unimplemented;
    }
};
