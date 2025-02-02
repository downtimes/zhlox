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
    greater_equal,
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
    string: []const u8, // String is not owned, it points into the input_scratch.
};

pub const Token = struct {
    const Self = @This();
    type_: Type,
    lexeme: []const u8, // String is not owned, it points into the input_scratch.
    literal: ?Literal,
    line: u32,

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
