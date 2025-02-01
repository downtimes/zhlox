const std = @import("std");
const token = @import("token.zig");
const main = @import("main.zig");
const ast = @import("ast.zig");
const config = @import("config.zig");

const ParseError = error{ UnexpectedToken, InvalidAssignment, OutOfMemory, TooManyArguments };

const FunctionKind = enum { method, function };

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
    allocator: std.mem.Allocator = undefined,

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
    pub fn parseInto(self: *Self, arena: *std.heap.ArenaAllocator) ParseError!ast.Ast {
        self.allocator = arena.allocator();
        var result = try ast.Ast.init(self.allocator);

        var last_err: ?ParseError = null;
        while (!self.isAtEnd()) {
            // Keep going on statement errors. We synchronize to the next statement beginning so that a mistake in
            // one statement should not interfere with parsing another statement. Therefore we give the user
            // as much information about their errors as possible. If any one error occurred we remember it and return
            // the last error to the caller. Therefore the caller can't execute an invalid syntax tree.
            const decl = self.declaration() catch |err| {
                if (err == ParseError.OutOfMemory) return err;

                last_err = err;
                if (self.diagnostic != null) {
                    const diagnostic = self.diagnostic.?;
                    const found = self.diagnostic.?.found;
                    main.reportError(diagnostic.found.line, &[_][]const u8{ "Parse Error: '", found.lexeme, "' ", diagnostic.message });
                }

                // In case of invalid assignment the parsing found an error but we are not confused about the state
                // so we aren't in panic mode and there is no need to synchronize. Synchronization might gloss over
                // other errors that are useful for our users.
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

    fn declaration(self: *Self) Stmt {
        if (self.match(&[_]token.Type{token.Type.fun})) {
            return self.function(FunctionKind.function);
        }
        if (self.match(&[_]token.Type{token.Type.var_})) {
            return self.variableDeclaration();
        }
        if (self.match(&[_]token.Type{token.Type.class})) {
            return self.class_declaration();
        }

        return self.statement();
    }

    fn class_declaration(self: *Self) Stmt {
        try self.consume(token.Type.identifier, "Expected class name");
        const identifier = self.previous();
        try self.consume(token.Type.left_brace, "Expected '{' before class body.");

        var methods = std.ArrayList(ast.Stmt).init(self.allocator);
        while (!self.check(token.Type.right_brace) and !self.isAtEnd()) {
            try methods.append(try self.function(FunctionKind.method));
        }

        try self.consume(token.Type.right_brace, "Expected '}' after class body.");

        return ast.Stmt{ .class = ast.Class{
            .name = identifier,
            .methods = methods.moveToUnmanaged(),
        } };
    }

    fn function(self: *Self, kind: FunctionKind) Stmt {
        _ = kind; // implement flag for methods to know in interpreter if this needs to be bound.
        try self.consume(token.Type.identifier, "Expected function name.");
        const name = self.previous();

        try self.consume(token.Type.left_paren, "Expected '(' after function name.");
        var params = std.ArrayList(token.Token).init(self.allocator);

        if (!self.check(token.Type.right_paren)) {
            try self.consume(token.Type.identifier, "Expected parameter name.");
            try params.append(self.previous());
            while (self.match(&[_]token.Type{token.Type.comma})) {
                try self.consume(token.Type.identifier, "Expected parameter name.");
                try params.append(self.previous());
                if (params.items.len >= config.max_params) {
                    self.diagnostic = Diagnostic{
                        .found = self.peek(),
                        .message = std.fmt.comptimePrint("Can't have more than {d} arguments.", .{config.max_params}),
                    };
                    return ParseError.TooManyArguments;
                }
            }
        }
        try self.consume(token.Type.right_paren, "Expected ')' after parameters.");

        try self.consume(token.Type.left_brace, "Expected '{' before function body.");
        const body = try self.block();
        return ast.Stmt{ .function = ast.Function{ .name = name, .params = params.moveToUnmanaged(), .body = body } };
    }

    fn variableDeclaration(self: *Self) Stmt {
        try self.consume(token.Type.identifier, "Expected variable name.");
        const name = self.previous();

        var initializer: ?ast.Expr = null;
        if (self.match(&[_]token.Type{token.Type.equal})) {
            initializer = try self.expression();
        }

        try self.consume(token.Type.semicolon, "Expected ';' after variable declaration.");
        return ast.Stmt{ .var_decl = ast.VariableDeclaration{ .name = name, .initializer = initializer } };
    }

    fn statement(self: *Self) Stmt {
        if (self.match(&[_]token.Type{token.Type.for_})) return self.forStatement();
        if (self.match(&[_]token.Type{token.Type.if_})) return self.ifStatement();
        if (self.match(&[_]token.Type{token.Type.print})) return self.printStatement();
        if (self.match(&[_]token.Type{token.Type.return_})) return self.returnStatement();
        if (self.match(&[_]token.Type{token.Type.while_})) return self.whileStatement();
        if (self.match(&[_]token.Type{token.Type.left_brace})) {
            return ast.Stmt{ .block = try self.block() };
        }

        return self.expressionStatement();
    }

    fn ifStatement(self: *Self) Stmt {
        try self.consume(token.Type.left_paren, "Expected '(' after 'if.'");
        const condition = try self.expression();
        try self.consume(token.Type.right_paren, "Expected ')' after the if condition.");

        const then = try self.allocator.create(ast.Stmt);
        then.* = try self.statement();
        var els: ?*ast.Stmt = null;
        if (self.match(&[_]token.Type{token.Type.else_})) {
            els = try self.allocator.create(ast.Stmt);
            els.?.* = try self.statement();
        }

        return ast.Stmt{ .cond = ast.Conditional{ .condition = condition, .then = then, .els = els } };
    }

    fn block(self: *Self) ParseError!std.ArrayListUnmanaged(ast.Stmt) {
        var statements = std.ArrayList(ast.Stmt).init(self.allocator);

        while (!self.check(token.Type.right_brace) and !self.isAtEnd()) {
            const decl = try self.declaration();
            try statements.append(decl);
        }

        try self.consume(token.Type.right_brace, "Expected '}' at the end of a block.");
        return statements.moveToUnmanaged();
    }

    fn printStatement(self: *Self) Stmt {
        const value = try self.expression();
        try self.consume(token.Type.semicolon, "Expected ';' after value.");
        return ast.Stmt{ .print = value };
    }

    fn forStatement(self: *Self) Stmt {
        try self.consume(token.Type.left_paren, "Expected '(' after 'for'.");

        var init: ?ast.Stmt = null;
        if (self.match(&[_]token.Type{token.Type.semicolon})) {
            // init already null, nothing to do.
        } else if (self.match(&[_]token.Type{token.Type.var_})) {
            init = try self.variableDeclaration();
        } else {
            init = try self.expressionStatement();
        }

        var cond = ast.Expr{ .literal = ast.Literal{ .bool_ = true } };
        if (!self.check(token.Type.semicolon)) {
            cond = try self.expression();
        }
        try self.consume(token.Type.semicolon, "Expected ';' after loop condition.");

        var after: ?ast.Expr = null;
        if (!self.check(token.Type.right_paren)) {
            after = try self.expression();
        }
        try self.consume(token.Type.right_paren, "Expected ')' after for clause.");

        var body = try self.statement();

        // The base case is that the loop is a block, we can then take the
        // list of statements and just wrap them in a while statement.
        // Otherwise we create a new list with only one element.
        // Depending on if the increment part of the if exists or not, we stuff
        // it into the statement list as well.
        switch (body) {
            .block => |*b| {
                // Stuff the increment after the while body.
                if (after) |a| {
                    try b.append(self.allocator, ast.Stmt{ .expr = a });
                }
                body = ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond, .body = b.* } };
            },
            else => {
                var while_body = std.ArrayList(ast.Stmt).init(self.allocator);
                try while_body.append(body);
                // Stuff the increment after the while body.
                if (after) |a| {
                    try while_body.append(ast.Stmt{ .expr = a });
                }
                body = ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond, .body = while_body.moveToUnmanaged() } };
            },
        }

        if (init) |i| {
            var sugar = std.ArrayList(ast.Stmt).init(self.allocator);
            try sugar.append(i);
            try sugar.append(body);
            body = ast.Stmt{ .block = sugar.moveToUnmanaged() };
        }

        return body;
    }

    fn returnStatement(self: *Self) Stmt {
        const keyword = self.previous();
        var value: ?ast.Expr = null;
        if (!self.check(token.Type.semicolon)) {
            value = try self.expression();
        }
        try self.consume(token.Type.semicolon, "Expected ';' after return.");
        return ast.Stmt{ .ret = ast.Return{ .keyword = keyword, .value = value } };
    }

    fn whileStatement(self: *Self) Stmt {
        try self.consume(token.Type.left_paren, "Expected '(' after 'while'.");
        const cond = try self.expression();
        try self.consume(token.Type.right_paren, "Expected ')' after while condition.");
        // Base case is that the statement will be a block. Only if it is not
        // a block do we manually wrap it into a block.
        const stmt = try self.statement();
        switch (stmt) {
            .block => |b| {
                return ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond, .body = b } };
            },
            else => {
                var statements = std.ArrayList(ast.Stmt).init(self.allocator);
                try statements.append(stmt);
                return ast.Stmt{ .while_ = ast.WhileStmt{ .condition = cond, .body = statements.moveToUnmanaged() } };
            },
        }
    }

    fn expressionStatement(self: *Self) Stmt {
        const expr = try self.expression();
        try self.consume(token.Type.semicolon, "Expected ';' after expression.");
        return ast.Stmt{ .expr = expr };
    }

    fn expression(self: *Self) Expr {
        return self.assignment();
    }

    fn assignment(self: *Self) Expr {
        const expr = try self.logicalOr();

        if (self.match(&[_]token.Type{token.Type.equal})) {
            const equals = self.previous();
            const value = try self.allocator.create(ast.Expr);
            value.* = try self.assignment();

            if (@as(std.meta.Tag(ast.Expr), expr) == .variable) {
                return ast.Expr{ .assign = ast.Assignment{ .variable = expr.variable, .value = value } };
            }

            self.diagnostic = Diagnostic{
                .found = equals,
                .message = "invalid assignment target.",
            };
            return ParseError.InvalidAssignment;
        }

        return expr;
    }

    fn logicalOr(self: *Self) Expr {
        var result = try self.logicalAnd();

        while (self.match(&[_]token.Type{token.Type.or_})) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.logicalAnd();
            result = ast.Expr{ .logical = ast.Logical{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn logicalAnd(self: *Self) Expr {
        var result = try self.equality();

        while (self.match(&[_]token.Type{token.Type.and_})) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.equality();
            result = ast.Expr{ .logical = ast.Logical{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn equality(self: *Self) Expr {
        var result = try self.comparison();

        while (self.match(&[_]token.Type{ token.Type.bang_equal, token.Type.equal_equal })) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.comparison();

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn comparison(self: *Self) Expr {
        var result = try self.term();

        while (self.match(&[_]token.Type{ token.Type.greater, token.Type.greater_equal, token.Type.less, token.Type.less_equal })) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.term();

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn term(self: *Self) Expr {
        var result = try self.factor();

        while (self.match(&[_]token.Type{ token.Type.minus, token.Type.plus })) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.factor();

            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn factor(self: *Self) Expr {
        var result = try self.unary();

        while (self.match(&[_]token.Type{ token.Type.slash, token.Type.star })) {
            const operator = self.previous();
            const left = try self.allocator.create(ast.Expr);
            left.* = result;
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.unary();
            result = ast.Expr{ .binary = ast.Binary{ .left = left, .operator = operator, .right = right } };
        }
        return result;
    }

    fn unary(self: *Self) Expr {
        if (self.match(&[_]token.Type{ token.Type.bang, token.Type.minus })) {
            const operator = self.previous();
            const right = try self.allocator.create(ast.Expr);
            right.* = try self.unary();

            return ast.Expr{ .unary = ast.Unary{ .operator = operator, .right = right } };
        }

        return self.call();
    }

    fn call(self: *Self) Expr {
        var expr = try self.primary();

        // The code is a little more verbose than it needs to be in preparation for more code later down the line.
        while (true) {
            if (self.match(&[_]token.Type{token.Type.left_paren})) {
                expr = try self.finishCall(expr);
            } else {
                break;
            }
        }

        return expr;
    }

    fn finishCall(self: *Self, callee: ast.Expr) Expr {
        var arguments = std.ArrayList(ast.Expr).init(self.allocator);
        if (!self.check(token.Type.right_paren)) {
            try arguments.append(try self.expression());
            while (self.match(&[_]token.Type{token.Type.comma})) {
                try arguments.append(try self.expression());
                if (arguments.items.len >= config.max_params) {
                    self.diagnostic = Diagnostic{
                        .found = self.peek(),
                        .message = std.fmt.comptimePrint("Can't have more than {d} arguments.", .{config.max_params}),
                    };
                    return ParseError.TooManyArguments;
                }
            }
        }

        try self.consume(token.Type.right_paren, "Expected ')' after arguments.");
        const closing_paren = self.previous();
        const callee_place = try self.allocator.create(ast.Expr);
        callee_place.* = callee;

        return ast.Expr{ .call = ast.Call{
            .callee = callee_place,
            .closing_paren = closing_paren,
            .arguments = arguments.moveToUnmanaged(),
        } };
    }

    fn primary(self: *Self) Expr {
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
                .string => |s| ast.Literal{ .string = s },
            };
            return ast.Expr{ .literal = literal };
        }

        if (self.match(&[_]token.Type{token.Type.identifier})) {
            return ast.Expr{ .variable = ast.Variable{
                .name = self.previous(),
                .resolve_steps = null,
            } };
        }

        if (self.match(&[_]token.Type{token.Type.left_paren})) {
            const grouping = try self.allocator.create(ast.Expr);
            grouping.* = try self.expression();

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
        // Safe since it is guaranteed by our parser that we always have an eof
        // token at the end of our token stream.
        return self.peek().type_ == token.Type.eof;
    }
};
