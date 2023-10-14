const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");
const ast = @import("ast.zig");

const ParseError = error{ UnexpectedToken, InvalidAssignment, OutOfMemory, TooManyArguments };

pub const Diagnostic = struct {
    found: token.Token,
    message: []const u8,
};

pub const Parser = struct {
    const Self = @This();
    const Stmt = ParseError!ast.Stmt;
    const Expr = ParseError!ast.Expr;

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

    // This Ast doesn't own the tokens contained in it.
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

    fn declaration(self: *Self, allocator: std.mem.Allocator) Stmt {
        if (self.match(&[_]token.Type{token.Type.fun})) {
            return self.function(allocator);
        }
        if (self.match(&[_]token.Type{token.Type.var_})) {
            return self.variableDeclaration(allocator);
        }

        return self.statement(allocator);
    }

    fn function(self: *Self, allocator: std.mem.Allocator) Stmt {
        try self.consume(token.Type.identifier, "Expected function name.");
        const name = self.previous();

        try self.consume(token.Type.left_paren, "Expected '(' after function name.");
        var params = std.ArrayListUnmanaged(token.Token){};

        if (!self.check(token.Type.right_paren)) {
            try self.consume(token.Type.identifier, "Expected parameter name.");
            try params.append(allocator, self.previous());
            while (self.match(&[_]token.Type{token.Type.comma})) {
                try self.consume(token.Type.identifier, "Expected parameter name.");
                try params.append(allocator, self.previous());
                if (params.items.len >= 255) {
                    self.diagnostic = Diagnostic{
                        .found = self.peek(),
                        .message = "Can't have more than 255 arguments.",
                    };
                    return ParseError.TooManyArguments;
                }
            }
        }
        try self.consume(token.Type.right_paren, "Expcted ')' after parameters.");

        try self.consume(token.Type.left_brace, "Expected '{' before function body.");
        const body = try self.block(allocator);
        return ast.Stmt{ .function = ast.Function{ .name = name, .params = params, .body = body } };
    }

    fn variableDeclaration(self: *Self, allocator: std.mem.Allocator) Stmt {
        try self.consume(token.Type.identifier, "Expected variable name.");
        const name = self.previous();

        var initializer: ?ast.Expr = null;
        if (self.match(&[_]token.Type{token.Type.equal})) {
            initializer = try self.expression(allocator);
        }

        try self.consume(token.Type.semicolon, "Expected ';' after variable declaration.");
        return ast.Stmt{ .var_decl = ast.VariableDeclaration{ .name = name, .initializer = initializer } };
    }

    fn statement(self: *Self, allocator: std.mem.Allocator) Stmt {
        if (self.match(&[_]token.Type{token.Type.for_})) return self.forStatement(allocator);
        if (self.match(&[_]token.Type{token.Type.if_})) return self.ifStatement(allocator);
        if (self.match(&[_]token.Type{token.Type.print})) return self.printStatement(allocator);
        if (self.match(&[_]token.Type{token.Type.while_})) return self.whileStatement(allocator);
        if (self.match(&[_]token.Type{token.Type.left_brace})) {
            return ast.Stmt{ .block = try self.block(allocator) };
        }

        return self.expressionStatement(allocator);
    }

    fn ifStatement(self: *Self, allocator: std.mem.Allocator) Stmt {
        try self.consume(token.Type.left_paren, "Expected '(' after 'if.'");
        const condition = try self.expression(allocator);
        try self.consume(token.Type.right_paren, "Expected ')' after the if condition.");

        var then = try allocator.create(ast.Stmt);
        then.* = try self.statement(allocator);
        var els: ?*ast.Stmt = null;
        if (self.match(&[_]token.Type{token.Type.else_})) {
            els = try allocator.create(ast.Stmt);
            els.?.* = try self.statement(allocator);
        }

        return ast.Stmt{ .cond = ast.Conditional{ .condition = condition, .then = then, .els = els } };
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

    fn printStatement(self: *Self, allocator: std.mem.Allocator) Stmt {
        const value = try self.expression(allocator);
        try self.consume(token.Type.semicolon, "Expected ';' after value.");
        return ast.Stmt{ .print = value };
    }

    fn forStatement(self: *Self, allocator: std.mem.Allocator) Stmt {
        try self.consume(token.Type.left_paren, "Exppected '(' after 'for'.");

        var init: ?ast.Stmt = null;
        if (self.match(&[_]token.Type{token.Type.semicolon})) {
            // Already null
        } else if (self.match(&[_]token.Type{token.Type.var_})) {
            init = try self.variableDeclaration(allocator);
        } else {
            init = try self.expressionStatement(allocator);
        }

        var cond: ?ast.Expr = null;
        if (!self.check(token.Type.semicolon)) {
            cond = try self.expression(allocator);
        }
        try self.consume(token.Type.semicolon, "Expected ';' after loop condition.");

        var after: ?ast.Expr = null;
        if (!self.check(token.Type.right_paren)) {
            after = try self.expression(allocator);
        }
        try self.consume(token.Type.right_paren, "Expected ')' after for clause.");

        var body = try self.statement(allocator);

        if (after != null) {
            var sugar = std.ArrayListUnmanaged(ast.Stmt){};
            try sugar.append(allocator, body);
            try sugar.append(allocator, ast.Stmt{ .expr = after.? });
            body = ast.Stmt{ .block = sugar };
        }

        if (cond == null) {
            cond.? = ast.Expr{ .literal = ast.Literal{ .bool_ = true } };
        }
        const old_body = try allocator.create(ast.Stmt);
        old_body.* = body;
        body = ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond.?, .body = old_body } };

        if (init != null) {
            var sugar = std.ArrayListUnmanaged(ast.Stmt){};
            try sugar.append(allocator, init.?);
            try sugar.append(allocator, body);
            body = ast.Stmt{ .block = sugar };
        }

        return body;
    }

    fn whileStatement(self: *Self, allocator: std.mem.Allocator) Stmt {
        try self.consume(token.Type.left_paren, "Expected '(' after 'while'.");
        const cond = try self.expression(allocator);
        try self.consume(token.Type.right_paren, "Expected ')' after while condition.");
        const stmt = try allocator.create(ast.Stmt);
        stmt.* = try self.statement(allocator);

        return ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond, .body = stmt } };
    }

    fn expressionStatement(self: *Self, allocator: std.mem.Allocator) Stmt {
        const expr = try self.expression(allocator);
        try self.consume(token.Type.semicolon, "Expected ';' after expression.");
        return ast.Stmt{ .expr = expr };
    }

    fn expression(self: *Self, allocator: std.mem.Allocator) Expr {
        return self.assignment(allocator);
    }

    fn assignment(self: *Self, allocator: std.mem.Allocator) Expr {
        const expr = try self.logicalOr(allocator);

        if (self.match(&[_]token.Type{token.Type.equal})) {
            const equals = self.previous();
            const value = try allocator.create(ast.Expr);
            value.* = try self.assignment(allocator);

            if (@as(std.meta.Tag(ast.Expr), expr) == .variable) {
                return ast.Expr{ .assign = ast.Assignment{ .name = expr.variable, .value = value } };
            }

            self.diagnostic = Diagnostic{
                .found = equals,
                .message = "invalid assignment target.",
            };
            return ParseError.InvalidAssignment;
        }

        return expr;
    }

    fn logicalOr(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.logicalAnd(allocator);

        while (self.match(&[_]token.Type{token.Type.or_})) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.logicalAnd(allocator);
            result = ast.Expr{ .logical = ast.Logical{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn logicalAnd(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.equality(allocator);

        while (self.match(&[_]token.Type{token.Type.and_})) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.equality(allocator);
            result = ast.Expr{ .logical = ast.Logical{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn equality(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.comparison(allocator);

        while (self.match(&[_]token.Type{ token.Type.bang_equal, token.Type.equal_equal })) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.comparison(allocator);

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn comparison(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.term(allocator);

        while (self.match(&[_]token.Type{ token.Type.greater, token.Type.gerater_equal, token.Type.less, token.Type.less_equal })) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.term(allocator);

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn term(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.factor(allocator);

        while (self.match(&[_]token.Type{ token.Type.minus, token.Type.plus })) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.factor(allocator);

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn factor(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.unary(allocator);

        while (self.match(&[_]token.Type{ token.Type.slash, token.Type.star })) {
            const operator = self.previous();
            const left = try allocator.create(ast.Expr);
            left.* = result;
            const right = try allocator.create(ast.Expr);
            right.* = try self.unary(allocator);
            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn unary(self: *Self, allocator: std.mem.Allocator) Expr {
        if (self.match(&[_]token.Type{ token.Type.bang, token.Type.minus })) {
            const operator = self.previous();
            const right = try allocator.create(ast.Expr);
            right.* = try self.unary(allocator);

            return ast.Expr{ .unary = ast.Unary{ .operator = operator, .right = right } };
        }

        return self.call(allocator);
    }

    fn call(self: *Self, allocator: std.mem.Allocator) Expr {
        var result = try self.primary(allocator);

        // The code is a little more verbose than it needs to be in preparation for more code later down the line.
        while (true) {
            if (self.match(&[_]token.Type{token.Type.left_paren})) {
                result = try self.finishCall(result, allocator);
            } else {
                break;
            }
        }

        return result;
    }

    fn finishCall(self: *Self, callee: ast.Expr, allocator: std.mem.Allocator) Expr {
        var arguments = std.ArrayListUnmanaged(ast.Expr){};
        if (!self.check(token.Type.right_paren)) {
            try arguments.append(allocator, try self.expression(allocator));
            while (self.match(&[_]token.Type{token.Type.comma})) {
                try arguments.append(allocator, try self.expression(allocator));
                if (arguments.items.len >= 255) {
                    self.diagnostic = Diagnostic{
                        .found = self.peek(),
                        .message = "Can't have more than 255 arguments.",
                    };
                    return ParseError.TooManyArguments;
                }
            }
        }

        try self.consume(token.Type.right_paren, "Expected ')' after arguments.");
        const closing_paren = self.previous();
        const callee_place = try allocator.create(ast.Expr);
        callee_place.* = callee;

        return ast.Expr{ .call = ast.Call{
            .callee = callee_place,
            .closing_paren = closing_paren,
            .arguments = arguments,
        } };
    }

    fn primary(self: *Self, allocator: std.mem.Allocator) Expr {
        if (self.match(&[_]token.Type{token.Type.false})) {
            return ast.Expr{ .literal = ast.Literal{ .bool_ = false } };
        }
        if (self.match(&[_]token.Type{token.Type.true})) {
            return ast.Expr{ .literal = ast.Literal{ .bool_ = true } };
        }
        if (self.match(&[_]token.Type{token.Type.nil})) {
            return ast.Expr{ .literal = ast.Literal.nil };
        }

        if (self.match(&[_]token.Type{ token.Type.number, token.Type.string })) {
            // Literal should be there since we matched on the type.
            const literal = switch (self.previous().literal.?) {
                .number => |n| ast.Literal{ .number = n },
                .string => |s| ast.Literal{ .string = try allocator.dupe(u8, s) },
            };
            return ast.Expr{ .literal = literal };
        }

        if (self.match(&[_]token.Type{token.Type.identifier})) {
            return ast.Expr{ .variable = self.previous() };
        }

        if (self.match(&[_]token.Type{token.Type.left_paren})) {
            const grouping = try allocator.create(ast.Expr);
            grouping.* = try self.expression(allocator);

            try self.consume(token.Type.right_paren, "Expected closing ')' after expression.");
            return ast.Expr{ .grouping = grouping };
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
