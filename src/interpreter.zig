const std = @import("std");
const ast = @import("ast.zig");
const token = @import("token.zig");

pub const Value = union(enum) {
    const Self = @This();

    string: []const u8,
    number: f64,
    bool_: bool,
    nil: void,

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

pub const RuntimError = error{ MinusOperand, IllegalBinaryOperand, Unimplemented };

pub const Diagonstic = struct {
    token_: token.Token, // Associated token to get the operation and the line number.
    message: []const u8,
};

pub const Interpreter = struct {
    const Self = @This();

    diagnostic: ?Diagonstic = null,

    pub fn execute(self: *Self, expr: ast.Expr) RuntimError!Value {
        switch (expr) {
            .literal => |l| {
                return switch (l) {
                    .bool_ => |b| Value{ .bool_ = b },
                    .number => |n| Value{ .number = n },
                    .string => |s| Value{ .string = s },
                    .nil => Value.nil,
                };
            },
            .grouping => |g| {
                return try self.execute(g.*);
            },
            .unary => |u| {
                const right = try self.execute(u.right.*);

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
                const left = try self.execute(b.left.*);
                const right = try self.execute(b.right.*);

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
                        // TODO handle string concatenation. We either need to copy all slices to be owned by us or we
                        //      need to do a copy on concat and handle concatenated strings and strings from input differently.
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
