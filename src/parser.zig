const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");
const ast = @import("ast.zig");

const ParseError = error{ UnexpectedToken, OutOfMemory };

pub const Diagnostic = struct {
    found: token.Token,
    message: []const u8,
};

// TODO think about how to handle allocation in the best way. Arena probably. Where to save the allocator?
pub const Parser = struct {
    const Self = @This();

    tokens: []token.Token,
    allocator: std.mem.Allocator,
    current: u32 = 0,
    diagnostic: ?Diagnostic = null,

    pub fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type_ == token.Type.semicolon) return; // Synchronization point is the statement border.

            switch (self.peek().type_) {
                //Any of the keywords also opens a statement and we are good to go
                .class | .fun | .var_ | .for_ | .if_ | .while_ | .print | .return_ => return,
                else => {},
            }

            self.advance();
        }
    }

    pub fn expression(self: *Self) ParseError!*ast.Expr {
        return try self.equality();
    }

    fn equality(self: *Self) ParseError!*ast.Expr {
        var result = try self.comparison();
        errdefer result.destroySelf(self.allocator);

        while (self.match(&[_]token.Type{ token.Type.bang_equal, token.Type.equal_equal })) {
            const operator = self.previous();
            const right = try self.comparison();
            errdefer right.destroySelf(self.allocator);

            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn comparison(self: *Self) ParseError!*ast.Expr {
        var result = try self.term();
        errdefer result.destroySelf(self.allocator);

        while (self.match(&[_]token.Type{ token.Type.greater, token.Type.gerater_equal, token.Type.less, token.Type.less_equal })) {
            const operator = self.previous();
            const right = try self.term();
            errdefer right.destroySelf(self.allocator);

            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn term(self: *Self) ParseError!*ast.Expr {
        var result = try self.factor();
        errdefer result.destroySelf(self.allocator);

        while (self.match(&[_]token.Type{ token.Type.minus, token.Type.plus })) {
            const operator = self.previous();
            const right = try self.factor();
            errdefer right.destroySelf(self.allocator);

            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn factor(self: *Self) ParseError!*ast.Expr {
        var result = try self.unary();
        errdefer result.destroySelf(self.allocator);

        while (self.match(&[_]token.Type{ token.Type.slash, token.Type.star })) {
            const operator = self.previous();
            const right = try self.unary();
            errdefer right.destroySelf(self.allocator);

            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn unary(self: *Self) ParseError!*ast.Expr {
        if (self.match(&[_]token.Type{ token.Type.bang, token.Type.minus })) {
            const operator = self.previous();
            const right = try self.unary();
            errdefer right.destroySelf(self.allocator);

            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .unary = ast.Unary{ .operator = operator, .right = right } };
            return new_expr;
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParseError!*ast.Expr {
        if (self.match(&[_]token.Type{token.Type.false})) {
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal{ .bool_ = false } };
            return new_expr;
        }
        if (self.match(&[_]token.Type{token.Type.true})) {
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal{ .bool_ = true } };
            return new_expr;
        }
        if (self.match(&[_]token.Type{token.Type.nil})) {
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal.nil };
            return new_expr;
        }

        if (self.match(&[_]token.Type{ token.Type.number, token.Type.string })) {
            const new_expr = try self.allocator.create(ast.Expr);
            // Literal should be there since we matched on the type.
            const literal = switch (self.previous().literal.?) {
                .number => |n| ast.Literal{ .number = n },
                .string => |s| ast.Literal{ .string = s },
            };
            new_expr.* = ast.Expr{ .literal = literal };
            return new_expr;
        }

        if (self.match(&[_]token.Type{token.Type.left_paren})) {
            const result = try self.expression();
            errdefer result.destroySelf(self.allocator);

            try self.consume(token.Type.right_paren, "Expected closing ')' after expression.");
            const new_expr = try self.allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .grouping = result };
            return new_expr;
        }

        // None of the cases we expected match here. Therefore we need to report an error and unwind
        self.diagnostic = Diagnostic{
            .found = self.peek(),
            .message = "Expected primary expression.",
        };
        return ParseError.UnexpectedToken;
    }

    fn match(self: *Self, types: []const token.Type) bool {
        for (types) |t| {
            if (self.check(t)) {
                self.advance();
                return true;
            }
        }
        return false;
    }

    fn check(self: Self, token_type: token.Type) bool {
        if (self.isAtEnd()) return false;
        return self.peek().type_ == token_type;
    }

    fn peek(self: Self) token.Token {
        return self.tokens[self.current];
    }

    fn previous(self: Self) token.Token {
        return self.tokens[self.current - 1];
    }

    fn advance(self: *Self) void {
        if (!self.isAtEnd()) self.current += 1;
    }

    fn consume(self: *Self, type_: token.Type, message: []const u8) ParseError!void {
        if (self.check(type_)) {
            self.advance();
            return;
        }

        self.diagnostic = Diagnostic{
            .found = self.previous(),
            .message = message,
        };
        return ParseError.UnexpectedToken;
    }

    fn isAtEnd(self: Self) bool {
        // Safe since it is guaranteed by our parser that we always have an eof token at the end of our tokenstream.
        return self.peek().type_ == token.Type.eof;
    }
};
