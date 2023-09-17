const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");
const ast = @import("ast.zig");

const ParseError = error{ UnexpectedToken, InvalidAssignment, OutOfMemory };

pub const Diagnostic = struct {
    found: token.Token,
    message: []const u8,
};

pub const Parser = struct {
    const Self = @This();

    tokens: []token.Token,
    current: u32 = 0,
    diagnostic: ?Diagnostic = null,

    pub fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().type_ == token.Type.semicolon) return; // Synchronization point is the statement border.

            switch (self.peek().type_) {
                //Any of the keywords also opens a statement and we are good to go
                .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
                else => {},
            }

            self.advance();
        }
    }

    pub fn parseInto(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Ast {
        var result = try ast.Ast.init(allocator);
        errdefer result.deinit();

        var last_err: ?ParseError = null;
        while (!self.isAtEnd()) {
            // Keep going on statement errors. We synchronize to the next statement beginning so that a mistake in
            // one statement should not interfere with parsing another statement. Therefore we give the user
            // as much information about their errors as possible. If any one error occured we remember it and return
            // the last error to the caller. Therefore the caller can't execute an invalid syntax tree.
            const decl = self.declaration(result.arena.allocator()) catch |err| {
                if (err == ParseError.OutOfMemory) return err;

                last_err = err;
                if (self.diagnostic != null) {
                    const diagnostic = self.diagnostic.?;
                    const found = self.diagnostic.?.found;
                    main.reportError(diagnostic.found.line, &[_][]const u8{ "Parse Error: ", found.lexeme, " ", diagnostic.message });
                }

                // In case of invalid assignment the parsing found an error but we are not confused about the state
                // so we aren't in panic mode and there is no need to synchronize. Synchronization might gloss over
                // other errors that are usefull for our users.
                if (err != ParseError.InvalidAssignment) {
                    self.synchronize();
                }
                continue;
            };
            try result.append(decl);
        }

        if (last_err != null) {
            return last_err.?;
        }

        return result;
    }

    fn declaration(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Stmt {
        if (self.match(&[_]token.Type{token.Type.var_})) {
            return self.variableDeclaration(allocator);
        }

        return self.statement(allocator);
    }

    fn variableDeclaration(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Stmt {
        try self.consume(token.Type.identifier, "Expected variable name.");
        const name = self.previous();

        var initializer: ?*ast.Expr = null;
        if (self.match(&[_]token.Type{token.Type.equal})) {
            initializer = try self.expression(allocator);
        }

        try self.consume(token.Type.semicolon, "Expected ';' after variable declaration.");
        return ast.Stmt{ .var_decl = ast.VariableDeclaration{ .name = name, .initializer = initializer } };
    }

    fn statement(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Stmt {
        if (self.match(&[_]token.Type{token.Type.print})) return self.printStatement(allocator);
        if (self.match(&[_]token.Type{token.Type.left_brace})) {
            return ast.Stmt{ .block = try self.block(allocator) };
        }

        return self.expressionStatement(allocator);
    }

    fn block(self: *Self, allocator: std.mem.Allocator) ParseError!std.ArrayListUnmanaged(ast.Stmt) {
        var statements = std.ArrayListUnmanaged(ast.Stmt){};

        while (!self.check(token.Type.right_brace) and !self.isAtEnd()) {
            const decl = try self.declaration(allocator);
            try statements.append(allocator, decl);
        }

        try self.consume(token.Type.right_brace, "Expected '}' at the end of a block.");
        return statements;
    }

    fn printStatement(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Stmt {
        const value = try self.expression(allocator);
        try self.consume(token.Type.semicolon, "Expected ';' after value.");
        return ast.Stmt{ .print = value };
    }

    fn expressionStatement(self: *Self, allocator: std.mem.Allocator) ParseError!ast.Stmt {
        const expr = try self.expression(allocator);
        try self.consume(token.Type.semicolon, "Expected ';' after expression.");
        return ast.Stmt{ .expr = expr };
    }

    fn expression(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        return self.assignment(allocator);
    }

    fn assignment(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        const expr = try self.equality(allocator);

        if (self.match(&[_]token.Type{token.Type.equal})) {
            const equals = self.previous();
            const value = try self.assignment(allocator);

            if (@as(std.meta.Tag(ast.Expr), expr.*) == .variable) {
                const new_expr = try allocator.create(ast.Expr);
                new_expr.* = ast.Expr{ .assign = ast.Assignment{ .name = expr.variable, .value = value } };
                return new_expr;
            }

            self.diagnostic = Diagnostic{
                .found = equals,
                .message = "invalid assignment target.",
            };
            return ParseError.InvalidAssignment;
        }

        return expr;
    }

    fn equality(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        var result = try self.comparison(allocator);

        while (self.match(&[_]token.Type{ token.Type.bang_equal, token.Type.equal_equal })) {
            const operator = self.previous();
            const right = try self.comparison(allocator);

            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn comparison(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        var result = try self.term(allocator);

        while (self.match(&[_]token.Type{ token.Type.greater, token.Type.gerater_equal, token.Type.less, token.Type.less_equal })) {
            const operator = self.previous();
            const right = try self.term(allocator);

            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn term(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        var result = try self.factor(allocator);

        while (self.match(&[_]token.Type{ token.Type.minus, token.Type.plus })) {
            const operator = self.previous();
            const right = try self.factor(allocator);

            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn factor(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        var result = try self.unary(allocator);

        while (self.match(&[_]token.Type{ token.Type.slash, token.Type.star })) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .binary = ast.Binary{ .left = result, .operator = operator, .right = right } };
            result = new_expr;
        }
        return result;
    }

    fn unary(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        if (self.match(&[_]token.Type{ token.Type.bang, token.Type.minus })) {
            const operator = self.previous();
            const right = try self.unary(allocator);

            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .unary = ast.Unary{ .operator = operator, .right = right } };
            return new_expr;
        }

        return self.primary(allocator);
    }

    fn primary(self: *Self, allocator: std.mem.Allocator) ParseError!*ast.Expr {
        if (self.match(&[_]token.Type{token.Type.false})) {
            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal{ .bool_ = false } };
            return new_expr;
        }
        if (self.match(&[_]token.Type{token.Type.true})) {
            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal{ .bool_ = true } };
            return new_expr;
        }
        if (self.match(&[_]token.Type{token.Type.nil})) {
            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .literal = ast.Literal.nil };
            return new_expr;
        }

        if (self.match(&[_]token.Type{ token.Type.number, token.Type.string })) {
            const new_expr = try allocator.create(ast.Expr);
            // Literal should be there since we matched on the type.
            const literal = switch (self.previous().literal.?) {
                .number => |n| ast.Literal{ .number = n },
                .string => |s| ast.Literal{ .string = s },
            };
            new_expr.* = ast.Expr{ .literal = literal };
            return new_expr;
        }

        if (self.match(&[_]token.Type{token.Type.identifier})) {
            const new_expr = try allocator.create(ast.Expr);
            new_expr.* = ast.Expr{ .variable = self.previous() };
            return new_expr;
        }

        if (self.match(&[_]token.Type{token.Type.left_paren})) {
            const result = try self.expression(allocator);

            try self.consume(token.Type.right_paren, "Expected closing ')' after expression.");
            const new_expr = try allocator.create(ast.Expr);
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
