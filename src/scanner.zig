const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");

const reserved_words = std.ComptimeStringMap(token.Type, .{
    .{ "and", token.Type.and_ },
    .{ "class", token.Type.class },
    .{ "else", token.Type.else_ },
    .{ "false", token.Type.false },
    .{ "for", token.Type.for_ },
    .{ "fun", token.Type.fun },
    .{ "if", token.Type.if_ },
    .{ "nil", token.Type.nil },
    .{ "or", token.Type.or_ },
    .{ "print", token.Type.print },
    .{ "return", token.Type.return_ },
    .{ "super", token.Type.super },
    .{ "this", token.Type.this },
    .{ "true", token.Type.true },
    .{ "var", token.Type.var_ },
    .{ "while", token.Type.while_ },
});

// TODO we can only handle ascii at the moment. Other utf-8 will not work.
pub const Scanner = struct {
    const Self = @This();
    const Return = std.ArrayList(token.Token);

    start_of_lexeme: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    source: []const u8,
    allocator: std.mem.Allocator,

    pub fn scanTokens(self: *Self) !Return {
        var tokens = Return.init(self.allocator);

        while (!self.isAtEnd()) {
            self.start_of_lexeme = self.current;
            try self.scanToken(&tokens);
        }

        try self.addToken(&tokens, token.Type.eof, null);
        return tokens;
    }

    fn addToken(self: Self, tokens: *Return, type_: token.Type, literal: ?token.Literal) !void {
        try tokens.append(token.Token{
            .type_ = type_,
            .lexeme = self.source[self.start_of_lexeme..self.current],
            .literal = literal,
            .line = self.line,
        });
    }

    fn scanToken(self: *Self, tokens: *Return) !void {
        const Type = token.Type;
        const c = self.consume();
        switch (c) {
            '(' => try self.addToken(tokens, Type.left_paren, null),
            ')' => try self.addToken(tokens, Type.right_paren, null),
            '{' => try self.addToken(tokens, Type.left_brace, null),
            '}' => try self.addToken(tokens, Type.right_brace, null),
            ',' => try self.addToken(tokens, Type.comma, null),
            '.' => try self.addToken(tokens, Type.dot, null),
            '-' => try self.addToken(tokens, Type.minus, null),
            '+' => try self.addToken(tokens, Type.plus, null),
            ';' => try self.addToken(tokens, Type.semicolon, null),
            '*' => try self.addToken(tokens, Type.star, null),
            '!' => {
                const t = if (self.consumeOnMatch('=')) Type.bang_equal else Type.bang;
                try self.addToken(tokens, t, null);
            },
            '=' => {
                const t = if (self.consumeOnMatch('=')) Type.equal_equal else Type.equal;
                try self.addToken(tokens, t, null);
            },
            '<' => {
                const t = if (self.consumeOnMatch('=')) Type.less_equal else Type.less;
                try self.addToken(tokens, t, null);
            },
            '>' => {
                const t = if (self.consumeOnMatch('=')) Type.gerater_equal else Type.greater;
                try self.addToken(tokens, t, null);
            },
            '/' => {
                if (self.consumeOnMatch('/')) {
                    while (!self.isAtEnd() and self.peek() != '\n') _ = self.consume();
                } else {
                    try self.addToken(tokens, Type.slash, null);
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
                    main.reportError(self.line, [_][]const u8{"Unterminated string."});
                    return;
                }

                _ = self.consume(); // Drop closing brace as well.
                try self.addToken(tokens, Type.string, token.Literal{ .string = self.source[self.start_of_lexeme + 1 .. self.current - 1] });
            },
            '0'...'9' => {
                while (std.ascii.isDigit(self.peek())) _ = self.consume();

                // Allow one dot in the number.
                if (self.peek() == '.' and std.ascii.isDigit(self.peekNext())) {
                    _ = self.consume();
                    while (std.ascii.isDigit(self.peek())) _ = self.consume();
                }

                const number = try std.fmt.parseFloat(f64, self.source[self.start_of_lexeme..self.current]);
                try self.addToken(tokens, Type.number, token.Literal{ .number = number });
            },
            'a'...'z', 'A'...'Z', '_' => {
                var next = self.peek();
                while (std.ascii.isAlphanumeric(next) or next == '_') : (next = self.peek()) {
                    _ = self.consume();
                }

                const text = self.source[self.start_of_lexeme..self.current];
                const t = reserved_words.get(text);

                try self.addToken(tokens, t orelse Type.identifier, null);
            },
            // Ignore whitespace.
            ' ', '\r', '\t' => {},
            // TODO scan all the invalid characters until the next valid one and report the range instead of each
            //      character.
            else => main.reportError(self.line, [_][]const u8{"Unexpected character in input."}),
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
