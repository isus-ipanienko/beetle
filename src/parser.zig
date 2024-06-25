const std = @import("std");

const tok = @import("tokenizer.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    tokenizer: tok.Tokenizer,
    current_token: tok.Token,
    next_token: tok.Token,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tokenizer: tok.Tokenizer) Parser {
        var parser = Parser{
            .tokenizer = tokenizer,
            .current_token = undefined,
            .next_token = undefined,
            .allocator = allocator,
        };
        parser.advanceToken();
        parser.advanceToken();
        return parser;
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

    fn parseIdentifier(self: *Parser) ?ast.Identifier {
        const current_token = self.current_token;
        if (!self.expectCurrentToken(.IDENTIFIER)) {
            return null;
        }
        return ast.Identifier{ .token = current_token };
    }

    fn parseExpression(self: *Parser) ?ast.Expression {
        // TODO: parse expressions
        while (!self.isCurrentToken(.SEMICOLON)) {
            self.advanceToken();
        }
        return ast.Expression{ .dummy_expression = ast.DummyExpression{} };
    }

    fn parseVarStatement(self: *Parser) ?ast.VarStatement {
        const identifier = self.parseIdentifier();
        if (identifier == null or !self.expectCurrentToken(.EQUAL)) {
            return null;
        }
        const expression = self.parseExpression();
        if (expression == null) {
            return null;
        }
        return ast.VarStatement.init(identifier.?, expression.?);
    }

    fn parseStatement(self: *Parser) ?ast.Statement {
        switch (self.current_token.token_type) {
            .VAR => {
                self.advanceToken();
                const statement = self.parseVarStatement();
                if (statement) |s| {
                    return ast.Statement{ .var_statement = s };
                }
            },
            else => {},
        }
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
};
