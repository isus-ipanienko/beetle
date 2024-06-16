const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,

    IDENTIFIER,
    NUMBER,

    EQUAL,
    DOUBLE_EQUAL,
    BANG,
    BANG_EQUAL,
    PLUS,
    MINUS,

    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    VAR,
};

pub const Token = struct {
    token_type: TokenType,
    literal: []const u8,
    line_no: usize,
    line_pos: usize,

    pub fn init(token_type: TokenType, literal: []const u8, line_no: usize, line_pos: usize) Token {
        return Token{ .token_type = token_type, .literal = literal, .line_no = line_no, .line_pos = line_pos };
    }
};

pub const Tokenizer = struct {
    source: []const u8,
    cursor: usize,
    line_no: usize,
    line_pos: usize,

    pub fn init(source: []const u8) Tokenizer {
        return Tokenizer{
            .source = source,
            .cursor = 0,
            .line_no = 0,
            .line_pos = 0,
        };
    }

    fn makeToken(self: *Tokenizer, token_type: TokenType, len: usize) Token {
        return Token.init(token_type, self.source[self.cursor .. self.cursor + len], self.line_no, self.line_pos);
    }

    fn isWhitespace(char: u8) bool {
        return char == ' ' or char == '\t' or char == '\n' or char == '\r';
    }

    fn nextEqual(self: *Tokenizer) Token {
        if (self.source[self.cursor + 1] == '=') {
            return self.makeToken(.DOUBLE_EQUAL, 2);
        } else {
            return self.makeToken(.EQUAL, 1);
        }
    }

    fn nextBang(self: *Tokenizer) Token {
        if (self.source[self.cursor + 1] == '=') {
            return self.makeToken(.BANG_EQUAL, 2);
        } else {
            return self.makeToken(.BANG, 1);
        }
    }

    pub fn next(self: *Tokenizer) Token {
        skip_whitespace: while (self.cursor < self.source.len) : (self.cursor += 1) {
            switch (self.source[self.cursor]) {
                ' ' => self.line_pos += 1,
                '\t' => self.line_pos += 1,
                '\n' => {
                    self.line_no += 1;
                    self.line_pos = 0;
                },
                '\r' => {},
                else => break :skip_whitespace,
            }
        }
        var token: Token = undefined;
        switch (self.source[self.cursor]) {
            '=' => token = self.nextEqual(),
            '!' => token = self.nextBang(),
            '+' => token = self.makeToken(.PLUS, 1),
            '-' => token = self.makeToken(.MINUS, 1),
            ',' => token = self.makeToken(.COMMA, 1),
            ';' => token = self.makeToken(.SEMICOLON, 1),
            '(' => token = self.makeToken(.LPAREN, 1),
            ')' => token = self.makeToken(.RPAREN, 1),
            '{' => token = self.makeToken(.LBRACE, 1),
            '}' => token = self.makeToken(.RBRACE, 1),
            else => token = self.makeToken(.ILLEGAL, 1),
        }
        self.cursor += token.literal.len;
        self.line_pos += token.literal.len;
        return token;
    }
};

test "tokenizer" {
    const input = "= == != ! + , ; ( ) { }";
    const expected: [11]Token = .{
        .{ .token_type = .EQUAL, .literal = "=", .line_no = 0, .line_pos = 0 },
        .{ .token_type = .DOUBLE_EQUAL, .literal = "==", .line_no = 0, .line_pos = 2 },
        .{ .token_type = .BANG_EQUAL, .literal = "!=", .line_no = 0, .line_pos = 5 },
        .{ .token_type = .BANG, .literal = "!", .line_no = 0, .line_pos = 8 },
        .{ .token_type = .PLUS, .literal = "+", .line_no = 0, .line_pos = 10 },
        .{ .token_type = .COMMA, .literal = ",", .line_no = 0, .line_pos = 12 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 0, .line_pos = 14 },
        .{ .token_type = .LPAREN, .literal = "(", .line_no = 0, .line_pos = 16 },
        .{ .token_type = .RPAREN, .literal = ")", .line_no = 0, .line_pos = 18 },
        .{ .token_type = .LBRACE, .literal = "{", .line_no = 0, .line_pos = 20 },
        .{ .token_type = .RBRACE, .literal = "}", .line_no = 0, .line_pos = 22 },
    };
    var actual: [expected.len]Token = undefined;
    var tokenizer = Tokenizer.init(input);
    for (0..actual.len) |i| {
        actual[i] = tokenizer.next();
    }
    try std.testing.expectEqualDeep(expected, actual);
}
