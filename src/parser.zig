const std = @import("std");

const tok = @import("tokenizer.zig");
const ast = @import("ast.zig");

pub const Error = union(enum) {
    token_error: TokenError,

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

pub const Parser = struct {
    tokenizer: tok.Tokenizer,
    current_token: tok.Token,
    next_token: tok.Token,
    allocator: std.mem.Allocator,
    errors: std.ArrayList(Error),

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

    fn expectCurrentToken(self: *Parser, token_type: tok.TokenType) bool {
        if (!self.isCurrentToken(token_type)) {
            return false;
        }
        self.advanceToken();
        return true;
    }

    const Precedence = enum(u8) {
        LOWEST,
        ASSIGN, // =
        OR, // or
        AND, // and
        EQUALITT, // == !=
        COMPARISON, // < > <= >=
        SUM, // + -
        PRODUCT, // * /
        PREFIX, // ! -
        CALL, // . ()
    };

    const OperatorCallbacks = struct {
        prefix: fn () ast.Expression,
        infix: fn (ast.Expression) ast.Expression,
    };

    fn parseExpression(self: *Parser, _: Precedence) ?ast.Expression {
        // TODO: parse expressions
        while (!self.isCurrentToken(.SEMICOLON)) {
            self.advanceToken();
        }
        return .{ .dummy_expression = ast.DummyExpression{} };
    }

    fn parseIdentifier(self: *Parser) ?ast.Identifier {
        const current_token = self.current_token;
        if (!self.expectCurrentToken(.IDENTIFIER)) {
            self.addError(.{ .token_error = .{
                .actual = self.current_token,
                .expected = "an identifier",
            } });
            return null;
        }
        return .{ .token = current_token };
    }

    fn parseVarStatement(self: *Parser) ?ast.VarStatement {
        const identifier = self.parseIdentifier();
        if (identifier == null) {
            return null;
        }
        if (!self.expectCurrentToken(.EQUAL)) {
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
            .IF => {},
            .FOR => {},
            .WHILE => {},
            else => {},
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
