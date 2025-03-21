const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");
const constants = @import("constants.zig");
const Allocator = std.mem.Allocator;

const reserved_words = std.StaticStringMap(token.Type).initComptime(.{
    .{ "and", token.Type.@"and" },
    .{ "class", token.Type.class },
    .{ "else", token.Type.@"else" },
    .{ "false", token.Type.false },
    .{ "for", token.Type.@"for" },
    .{ "fun", token.Type.fun },
    .{ "if", token.Type.@"if" },
    .{ "nil", token.Type.nil },
    .{ "or", token.Type.@"or" },
    .{ "print", token.Type.print },
    .{ "return", token.Type.@"return" },
    .{ constants.super, token.Type.super },
    .{ constants.this, token.Type.this },
    .{ "true", token.Type.true },
    .{ "var", token.Type.@"var" },
    .{ "while", token.Type.@"while" },
});

// TODO: make language utf8 compatible?
pub const Scanner = struct {
    const Self = @This();
    const Return = std.ArrayListUnmanaged(token.Token);

    start_of_lexeme: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    had_error: bool = false,
    source: []const u8,

    fn reportError(self: *Scanner, message: []const u8) void {
        main.reportError(self.line, &[_][]const u8{
            "Lex error: '",
            self.lexeme(),
            "' ",
            message,
        });
        self.had_error = true;
    }

    pub fn scanTokens(self: *Self, input_scratch: std.mem.Allocator) Allocator.Error!Return {
        var tokens: Return = .empty;
        errdefer tokens.deinit(input_scratch);

        while (!self.isAtEnd()) {
            self.start_of_lexeme = self.current;
            try self.scanToken(input_scratch, &tokens);
        }

        try self.addToken(input_scratch, &tokens, token.Type.eof, null);
        return tokens;
    }

    fn lexeme(self: Self) []const u8 {
        return self.source[self.start_of_lexeme..self.current];
    }

    fn addToken(self: Self, allocator: std.mem.Allocator, tokens: *Return, @"type": token.Type, literal: ?token.Literal) !void {
        try tokens.append(allocator, token.Token{
            .type = @"type",
            .lexeme = self.lexeme(),
            .literal = literal,
            .line = self.line,
        });
    }

    fn scanToken(self: *Self, allocator: std.mem.Allocator, tokens: *Return) !void {
        const Type = token.Type;
        const c = self.consume();
        switch (c) {
            '(' => try self.addToken(allocator, tokens, Type.left_paren, null),
            ')' => try self.addToken(allocator, tokens, Type.right_paren, null),
            '{' => try self.addToken(allocator, tokens, Type.left_brace, null),
            '}' => try self.addToken(allocator, tokens, Type.right_brace, null),
            ',' => try self.addToken(allocator, tokens, Type.comma, null),
            '.' => try self.addToken(allocator, tokens, Type.dot, null),
            '-' => try self.addToken(allocator, tokens, Type.minus, null),
            '+' => try self.addToken(allocator, tokens, Type.plus, null),
            ';' => try self.addToken(allocator, tokens, Type.semicolon, null),
            '*' => try self.addToken(allocator, tokens, Type.star, null),
            '!' => {
                const t = if (self.consumeOnMatch('=')) Type.bang_equal else Type.bang;
                try self.addToken(allocator, tokens, t, null);
            },
            '=' => {
                const t = if (self.consumeOnMatch('=')) Type.equal_equal else Type.equal;
                try self.addToken(allocator, tokens, t, null);
            },
            '<' => {
                const t = if (self.consumeOnMatch('=')) Type.less_equal else Type.less;
                try self.addToken(allocator, tokens, t, null);
            },
            '>' => {
                const t = if (self.consumeOnMatch('=')) Type.greater_equal else Type.greater;
                try self.addToken(allocator, tokens, t, null);
            },
            '/' => {
                if (self.consumeOnMatch('/')) {
                    while (!self.isAtEnd() and self.peek() != '\n') _ = self.consume();
                } else {
                    try self.addToken(allocator, tokens, Type.slash, null);
                }
            },
            '\n' => {
                self.line += 1;
            },
            '"' => {
                while (!self.isAtEnd() and self.peek() != '"') {
                    if (self.peek() == '\n') self.line += 1;
                    _ = self.consume();
                }

                if (self.isAtEnd()) {
                    self.reportError("untermintated string.");
                    return;
                }

                _ = self.consume(); // Drop closing brace as well.
                // Drop quotes for the literal value as well by subslicing the
                // lexeme
                const quoted_string = self.lexeme();
                try self.addToken(allocator, tokens, Type.string, token.Literal{ .string = quoted_string[1 .. quoted_string.len - 1] });
            },
            '0'...'9' => {
                while (std.ascii.isDigit(self.peek())) _ = self.consume();

                // Allow one dot in the number.
                if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
                    _ = self.consume();
                    while (std.ascii.isDigit(self.peek())) _ = self.consume();
                }

                const number = std.fmt.parseFloat(f64, self.lexeme()) catch blk: {
                    self.reportError("failed to parse number.");
                    break :blk 0;
                };
                try self.addToken(allocator, tokens, Type.number, token.Literal{ .number = number });
            },
            'a'...'z', 'A'...'Z', '_' => {
                var next = self.peek();
                while (std.ascii.isAlphanumeric(next) or next == '_') : (next = self.peek()) {
                    _ = self.consume();
                }

                const text = self.lexeme();
                const t = reserved_words.get(text);

                try self.addToken(allocator, tokens, t orelse Type.identifier, null);
            },
            // Ignore whitespace.
            ' ', '\r', '\t' => {},
            else => self.reportError("unexpected character in input."),
        }
    }

    fn peekNext(self: Self) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn peek(self: Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn consumeOnMatch(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != expected) return false;

        self.current += 1;
        return true;
    }

    fn consume(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        return c;
    }

    fn isAtEnd(self: Self) bool {
        return self.current >= self.source.len;
    }
};
