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
    string: []const u8,
};

pub const Token = struct {
    const Self = @This();
    type_: Type,
    lexeme: ?[]const u8,
    literal: ?Literal,
    line: u32,

    pub fn outPut(self: Self, writer: anytype) !void {
        if (self.literal == null) {
            try writer.print("{s} {s}\n", .{ @tagName(self.type_), self.lexeme orelse "" });
        } else {
            switch (self.literal.?) {
                .number => |value| try writer.print("{s} {s} {d}\n", .{ @tagName(self.type_), self.lexeme orelse "", value }),
                .string => |string| try writer.print("{s} {s} {s}\n", .{ @tagName(self.type_), self.lexeme orelse "", string }),
            }
        }
    }
};
