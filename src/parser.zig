const std = @import("std");

const tok = @import("tokenizer.zig");
const ast = @import("ast.zig");

pub const Error = union(enum) {
    token_error: TokenError,
    prefix_error: PrefixError,
    infix_error: InfixError,

    pub fn toString(self: *Error, allocator: std.mem.Allocator) ![]u8 {
        return switch (self.*) {
            inline else => |*s| s.toString(allocator),
        };
    }
};

const TokenError = struct {
    actual: tok.Token,
    expected: []const u8,

    fn toString(self: TokenError, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            "{}:{}: error: expected {s}, got {s}",
            .{ self.actual.line_no, self.actual.line_pos, self.expected, self.actual.literal },
        );
    }
};

const PrefixError = struct {
    token_type: tok.TokenType,

    fn toString(self: PrefixError, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            "prefix not defined for {s}",
            .{@tagName(self.token_type)},
        );
    }
};

const InfixError = struct {
    token_type: tok.TokenType,

    fn toString(self: InfixError, allocator: std.mem.Allocator) ![]u8 {
        return std.fmt.allocPrint(
            allocator,
            "infix not defined for {s}",
            .{@tagName(self.token_type)},
        );
    }
};

pub const Parser = struct {
    tokenizer: tok.Tokenizer,
    current_token: tok.Token,
    next_token: tok.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

    const Precedence = enum(u8) {
        LOWEST,
        ASSIGN, // =
        OR, // or
        AND, // and
        EQUALITY, // == !=
        COMPARISON, // < > <= >=
        SUM, // + -
        PRODUCT, // * /
        PREFIX, // ! -
        CALL, // . ()
    };

    const ParseRule = struct {
        prefix: ?*const fn (*Parser) ?*ast.Expression,
        infix: ?*const fn (*Parser, *ast.Expression) ?*ast.Expression,
        precedence: Precedence,
    };

    pub fn getParseRule(token_type: tok.TokenType) ParseRule {
        const t = tok.TokenType;
        comptime var cb =
            [_]ParseRule{.{ .prefix = null, .infix = null, .precedence = .LOWEST }} ** 256;
        cb[@intFromEnum(t.IDENTIFIER)] = .{ .prefix = parseIdentifierExpression, .infix = null, .precedence = .LOWEST };
        cb[@intFromEnum(t.NUMBER)] = .{ .prefix = parseNumberLiteralExpression, .infix = null, .precedence = .LOWEST };
        cb[@intFromEnum(t.BANG)] = .{ .prefix = parsePrefixExpression, .infix = null, .precedence = .LOWEST };
        cb[@intFromEnum(t.MINUS)] = .{ .prefix = parsePrefixExpression, .infix = parseInfixExpression, .precedence = .SUM };
        cb[@intFromEnum(t.PLUS)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .SUM };
        cb[@intFromEnum(t.SLASH)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .PRODUCT };
        cb[@intFromEnum(t.STAR)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .PRODUCT };
        cb[@intFromEnum(t.EQUAL_EQUAL)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .EQUALITY };
        cb[@intFromEnum(t.BANG_EQUAL)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .EQUALITY };
        cb[@intFromEnum(t.LESS)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .COMPARISON };
        cb[@intFromEnum(t.LESS_EQUAL)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .COMPARISON };
        cb[@intFromEnum(t.GREATER)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .COMPARISON };
        cb[@intFromEnum(t.GREATER_EQUAL)] = .{ .prefix = null, .infix = parseInfixExpression, .precedence = .COMPARISON };
        cb[@intFromEnum(t.TRUE)] = .{ .prefix = parseBooleanExpression, .infix = null, .precedence = .LOWEST };
        cb[@intFromEnum(t.FALSE)] = .{ .prefix = parseBooleanExpression, .infix = null, .precedence = .LOWEST };
        cb[@intFromEnum(t.LPAREN)] = .{ .prefix = parseGroupedExpression, .infix = null, .precedence = .LOWEST };
        return cb[@intFromEnum(token_type)];
    }

    pub fn init(allocator: std.mem.Allocator, tokenizer: tok.Tokenizer) Parser {
        var parser = Parser{
            .tokenizer = tokenizer,
            .current_token = undefined,
            .next_token = undefined,
            .allocator = allocator,
            .errors = std.ArrayList(Error).init(allocator),
        };
        parser.advanceToken();
        parser.advanceToken();
        return parser;
    }

    pub fn deinit(self: *Parser) void {
        self.errors.deinit();
    }

    fn advanceToken(self: *Parser) void {
        self.current_token = self.next_token;
        self.next_token = self.tokenizer.next();
    }

    fn isNextToken(self: *Parser, token_type: tok.TokenType) bool {
        return self.next_token.token_type == token_type;
    }

    fn isCurrentToken(self: *Parser, token_type: tok.TokenType) bool {
        return self.current_token.token_type == token_type;
    }

    fn consumeToken(self: *Parser, token_type: tok.TokenType) bool {
        if (!self.isCurrentToken(token_type)) {
            return false;
        }
        self.advanceToken();
        return true;
    }

    fn parseIdentifier(self: *Parser) ?ast.IdentifierExpression {
        const current_token = self.current_token;
        if (!self.consumeToken(.IDENTIFIER)) {
            self.addError(.{ .token_error = .{
                .actual = self.current_token,
                .expected = "an identifier",
            } });
            return null;
        }
        return .{ .token = current_token };
    }

    fn parseIdentifierExpression(self: *Parser) ?*ast.Expression {
        const identifier = self.parseIdentifier();
        if (identifier == null) {
            return null;
        }
        return ast.Expression.create(self.allocator, .{ .identifier_expression = identifier.? });
    }

    fn parseNumberLiteralExpression(self: *Parser) ?*ast.Expression {
        if (!self.isCurrentToken(.NUMBER)) {
            return null;
        }
        const number: f64 = std.fmt.parseFloat(f64, self.current_token.literal) catch {
            return null;
        };
        self.advanceToken();
        return ast.Expression.create(self.allocator, .{ .number_literal_expression = .{ .value = number } });
    }

    fn parseBooleanExpression(self: *Parser) ?*ast.Expression {
        const token_type = self.current_token.token_type;
        self.advanceToken();
        return ast.Expression.create(self.allocator, .{ .boolean_expression = .{ .value = token_type == tok.TokenType.TRUE } });
    }

    fn parsePrefixExpression(self: *Parser) ?*ast.Expression {
        const operator = self.current_token;
        self.advanceToken();
        const expression = self.parseExpression(.PREFIX);
        if (expression == null) {
            return null;
        }
        return ast.Expression.create(
            self.allocator,
            .{ .prefix_expression = .{ .operator = operator, .expression = expression.? } },
        );
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) ?*ast.Expression {
        const operator = self.current_token;
        const rule: ParseRule = getParseRule(operator.token_type);
        self.advanceToken();
        const right = self.parseExpression(rule.precedence);
        if (right == null) {
            left.destroy(self.allocator);
            return null;
        }
        return ast.Expression.create(
            self.allocator,
            .{ .infix_expression = .{ .operator = operator, .left = left, .right = right.? } },
        );
    }

    fn parseGroupedExpression(self: *Parser) ?*ast.Expression {
        self.advanceToken();
        const expression = self.parseExpression(.LOWEST);
        if (expression == null) {
            return null;
        }
        if (!self.consumeToken(.RPAREN)) {
            expression.?.destroy(self.allocator);
            return null;
        }
        return expression;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ?*ast.Expression {
        var rule: ParseRule = getParseRule(self.current_token.token_type);
        if (rule.prefix == null) {
            self.addError(.{ .prefix_error = .{ .token_type = self.current_token.token_type } });
            return null;
        }
        var left = rule.prefix.?(self);
        while (!self.isNextToken(.SEMICOLON) and
            @intFromEnum(precedence) < @intFromEnum(getParseRule(self.current_token.token_type).precedence))
        {
            if (left == null) {
                return null;
            }
            rule = getParseRule(self.current_token.token_type);
            if (rule.infix == null) {
                self.addError(.{ .infix_error = .{ .token_type = self.current_token.token_type } });
                return left;
            }
            left = rule.infix.?(self, left.?);
        }
        return left;
    }

    fn parseVarStatement(self: *Parser) ?ast.VarStatement {
        const identifier = self.parseIdentifier();
        if (identifier == null) {
            return null;
        }
        if (!self.consumeToken(.EQUAL)) {
            self.addError(.{ .token_error = .{
                .actual = self.current_token,
                .expected = "=",
            } });
            return null;
        }
        const value = self.parseExpression(.LOWEST);
        if (value == null) {
            return null;
        }
        return .{ .identifier = identifier.?, .value = value.? };
    }

    fn parseReturnStatement(self: *Parser) ?ast.ReturnStatement {
        const value = self.parseExpression(.LOWEST);
        if (value == null) {
            return null;
        }
        return .{ .value = value.? };
    }

    fn parseBlockStatement(self: *Parser) ?ast.BlockStatement {
        var block = ast.BlockStatement.init(self.allocator);
        while (!self.isCurrentToken(.RBRACE) and !self.isCurrentToken(.EOF)) {
            const statement: ?ast.Statement = self.parseStatement();
            if (statement) |s| {
                block.statements.append(s) catch {};
            }
            self.advanceToken();
        }
        return block;
    }

    fn parseIfStatement(self: *Parser) ?ast.IfStatement {
        if (!self.consumeToken(.LPAREN)) {
            return null;
        }
        const condition = self.parseExpression(.LOWEST);
        if (condition == null) {
            return null;
        }
        if (!self.consumeToken(.RPAREN) or !self.consumeToken(.LBRACE)) {
            return null;
        }
        const truthy = self.parseBlockStatement();
        if (truthy == null) {
            return null;
        }
        if (!self.consumeToken(.RBRACE)) {
            return null;
        }
        var falsey: ?ast.BlockStatement = null;
        if (self.consumeToken(.ELSE)) {
            if (!self.consumeToken(.LBRACE)) {
                return null;
            }
            falsey = self.parseBlockStatement();
            if (falsey == null) {
                return null;
            }
            if (!self.consumeToken(.RBRACE)) {
                return null;
            }
        }
        return .{ .condition = condition.?, .truthy = truthy.?, .falsey = falsey };
    }

    fn parseExpressionStatement(self: *Parser) ?ast.ExpressionStatement {
        const value = self.parseExpression(.LOWEST);
        if (value == null) {
            return null;
        }
        if (self.isNextToken(.SEMICOLON)) {
            self.advanceToken();
        }
        return .{ .value = value.? };
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        switch (self.current_token.token_type) {
            .VAR => {
                self.advanceToken();
                if (self.parseVarStatement()) |s| {
                    return .{ .var_statement = s };
                }
            },
            .RETURN => {
                self.advanceToken();
                if (self.parseReturnStatement()) |s| {
                    return .{ .return_statement = s };
                }
            },
            .IF => {
                self.advanceToken();
                if (self.parseIfStatement()) |s| {
                    return .{ .if_statement = s };
                }
            },
            .FOR => {},
            .WHILE => {},
            else => {
                if (self.parseExpressionStatement()) |s| {
                    return .{ .expression_statement = s };
                }
            },
        }
        self.addError(.{ .token_error = .{
            .actual = self.current_token,
            .expected = "an identifier, var, return, if, for or while",
        } });
        return null;
    }

    pub fn parseModule(self: *Parser) ast.Module {
        var module = ast.Module.init(self.allocator);
        while (self.current_token.token_type != .EOF) {
            const statement: ?ast.Statement = self.parseStatement();
            if (statement) |s| {
                module.statements.append(s) catch {};
            }
            self.advanceToken();
        }
        return module;
    }

    fn addError(self: *Parser, err: Error) void {
        self.errors.append(err) catch {};
    }
};
