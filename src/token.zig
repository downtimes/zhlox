const std = @import("std");

pub const Type = enum {
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    comma,
    dot,
    minus,
    plus,
    semicolon,
    slash,
    star,
    bang,
    bang_equal,
    equal,
    equal_equal,
    greater,
    gerater_equal,
    less,
    less_equal,
    identifier,
    string,
    number,
    class,
    else_,
    false,
    fun,
    for_,
    if_,
    nil,
    or_,
    and_,
    print,
    return_,
    super,
    this,
    true,
    var_,
    while_,
    eof,
};

pub const Literal = union(enum) {
    number: f64,
    // It is a little strange that the literal value is basically just the lexeme without the quotes.
    string: []const u8,
};

pub const Token = struct {
    const Self = @This();
    type_: Type,
    lexeme: []const u8,
    literal: ?Literal,
    line: u32,

    pub fn deepCopy(self: Self, allocator: std.mem.Allocator) !Self {
        var new_literal = self.literal;
        if (new_literal != null) {
            new_literal = switch (new_literal.?) {
                .string => |s| Literal{ .string = try allocator.dupe(u8, s) },
                else => new_literal,
            };
        }
        return Self{
            .type_ = self.type_,
            .lexeme = try allocator.dupe(u8, self.lexeme),
            .literal = new_literal,
            .line = self.line,
        };
    }

    pub fn output(self: Self, writer: anytype) !void {
        const name = @tagName(self.type_);
        if (self.literal == null) {
            try writer.print("{s} {s}\n", .{ name, self.lexeme });
        } else {
            const text = switch (self.literal.?) {
                .number => |value| {
                    try writer.print("{s} {s} {d}\n", .{ name, self.lexeme, value });
                    return;
                },
                .string => |string| string,
                .bool_ => |b| if (b) "true" else "false",
            };
            try writer.print("{s} {s} {s}\n", .{ name, self.lexeme, text });
        }
    }
};
