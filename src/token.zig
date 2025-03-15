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
    @"else",
    false,
    fun,
    @"for",
    @"if",
    nil,
    @"or",
    @"and",
    print,
    @"return",
    super,
    this,
    true,
    @"var",
    @"while",
    eof,
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8, // String is not owned, it points into the input_scratch.
};

pub const Token = struct {
    type: Type,
    lexeme: []const u8, // String is not owned, it points into the input_scratch.
    literal: ?Literal,
    line: u32,
};
