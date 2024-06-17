const std = @import("std");

pub const TokenType = enum {
    IDENTIFIER,
    NUMBER,
    STRING,
    TRUE,
    FALSE,
    NIL,

    VAR,
    FUNCTION,
    RETURN,

    IF,
    ELSE,
    FOR,
    WHILE,

    PLUS,
    MINUS,
    STAR,
    SLASH,

    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    AND,
    OR,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,

    ILLEGAL,
    EOF,
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

    fn makeToken(self: *const Tokenizer, token_type: TokenType, len: usize) Token {
        return Token.init(token_type, self.source[self.cursor .. self.cursor + len], self.line_no, self.line_pos);
    }

    fn isWhitespace(char: u8) bool {
        return char == ' ' or char == '\t' or char == '\n' or char == '\r';
    }

    fn isIdentifierAt(self: *const Tokenizer, at: usize) bool {
        const char: u8 = self.source[at];
        return (char >= 'a' and char <= 'z') or (char >= 'A' and char <= 'Z') or char == '_';
    }

    fn isDigitAt(self: *const Tokenizer, at: usize) bool {
        const char: u8 = self.source[at];
        return char >= '0' and char <= '9';
    }

    fn checkAt(self: *const Tokenizer, at: usize, char: u8) bool {
        return at < self.source.len and self.source[at] == char;
    }

    fn nextEqual(self: *const Tokenizer) Token {
        if (self.checkAt(self.cursor + 1, '=')) {
            return self.makeToken(.EQUAL_EQUAL, 2);
        } else {
            return self.makeToken(.EQUAL, 1);
        }
    }

    fn nextBang(self: *const Tokenizer) Token {
        if (self.checkAt(self.cursor + 1, '=')) {
            return self.makeToken(.BANG_EQUAL, 2);
        } else {
            return self.makeToken(.BANG, 1);
        }
    }

    fn nextGreater(self: *const Tokenizer) Token {
        if (self.checkAt(self.cursor + 1, '=')) {
            return self.makeToken(.GREATER_EQUAL, 2);
        } else {
            return self.makeToken(.GREATER, 1);
        }
    }

    fn nextLess(self: *const Tokenizer) Token {
        if (self.checkAt(self.cursor + 1, '=')) {
            return self.makeToken(.LESS_EQUAL, 2);
        } else {
            return self.makeToken(.LESS, 1);
        }
    }

    fn nextNumber(self: *const Tokenizer) Token {
        var len: usize = 1;
        while (self.cursor + len < self.source.len and self.isDigitAt(self.cursor + len)) : (len += 1) {}
        if (self.checkAt(self.cursor + len, '.')) {
            len += 1;
            while (self.cursor + len < self.source.len and self.isDigitAt(self.cursor + len)) : (len += 1) {}
        }
        return self.makeToken(.NUMBER, len);
    }

    fn nextString(self: *const Tokenizer) Token {
        var len: usize = 1;
        while (self.cursor + len < self.source.len and !self.checkAt(self.cursor + len, '"')) : (len += 1) {}
        if (self.checkAt(self.cursor + len, '"')) {
            return self.makeToken(.STRING, len);
        } else {
            return self.makeToken(.ILLEGAL, len);
        }
    }

    fn checkKeyword(self: *const Tokenizer, keyword: []const u8) bool {
        return self.cursor + keyword.len < self.source.len and std.mem.eql(
            u8,
            self.source[self.cursor .. self.cursor + keyword.len],
            keyword,
        );
    }

    fn nextIdentifier(self: *const Tokenizer) Token {
        var len: usize = 1;
        while (self.cursor + len < self.source.len and self.isIdentifierAt(self.cursor + len)) : (len += 1) {}
        var token_type: TokenType = .IDENTIFIER;
        if (self.checkKeyword("var")) {
            token_type = .VAR;
        } else if (self.checkKeyword("fn")) {
            token_type = .FUNCTION;
        } else if (self.checkKeyword("for")) {
            token_type = .FOR;
        } else if (self.checkKeyword("false")) {
            token_type = .FALSE;
        } else if (self.checkKeyword("return")) {
            token_type = .RETURN;
        } else if (self.checkKeyword("true")) {
            token_type = .TRUE;
        } else if (self.checkKeyword("nil")) {
            token_type = .NIL;
        } else if (self.checkKeyword("if")) {
            token_type = .IF;
        } else if (self.checkKeyword("else")) {
            token_type = .ELSE;
        } else if (self.checkKeyword("while")) {
            token_type = .WHILE;
        } else if (self.checkKeyword("and")) {
            token_type = .AND;
        } else if (self.checkKeyword("or")) {
            token_type = .OR;
        }
        return self.makeToken(token_type, len);
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
                '/' => {
                    if (self.checkAt(self.cursor + 1, '/')) {
                        self.cursor += 1;
                        skip_comment: while (self.cursor < self.source.len) : (self.cursor += 1) {
                            if (self.source[self.cursor] == '\n') {
                                self.line_no += 1;
                                self.line_pos = 0;
                                break :skip_comment;
                            }
                        }
                    } else {
                        break :skip_whitespace;
                    }
                },
                else => break :skip_whitespace,
            }
        }
        var token: Token = undefined;
        switch (self.source[self.cursor]) {
            '=' => token = self.nextEqual(),
            '!' => token = self.nextBang(),
            '>' => token = self.nextGreater(),
            '<' => token = self.nextLess(),
            '+' => token = self.makeToken(.PLUS, 1),
            '-' => token = self.makeToken(.MINUS, 1),
            '*' => token = self.makeToken(.STAR, 1),
            '/' => token = self.makeToken(.SLASH, 1),
            ',' => token = self.makeToken(.COMMA, 1),
            ';' => token = self.makeToken(.SEMICOLON, 1),
            '(' => token = self.makeToken(.LPAREN, 1),
            ')' => token = self.makeToken(.RPAREN, 1),
            '{' => token = self.makeToken(.LBRACE, 1),
            '}' => token = self.makeToken(.RBRACE, 1),
            '"' => token = self.nextString(),
            else => {
                if (self.isDigitAt(self.cursor)) {
                    token = self.nextNumber();
                } else if (self.isIdentifierAt(self.cursor)) {
                    token = self.nextIdentifier();
                } else {
                    token = self.makeToken(.ILLEGAL, 1);
                }
            },
        }
        self.cursor += token.literal.len;
        self.line_pos += token.literal.len;
        return token;
    }
};

test "tokenizer" {
    const input =
        \\var five = 5;
        \\var ten = 10.5;
        \\
        \\  // this function adds two values
        \\var add = fn(x, y) {
        \\  return x + y;
        \\};
        \\
        \\var result = add(five, ten);
    ;
    const expected: [37]Token = .{
        .{ .token_type = .VAR, .literal = "var", .line_no = 0, .line_pos = 0 },
        .{ .token_type = .IDENTIFIER, .literal = "five", .line_no = 0, .line_pos = 4 },
        .{ .token_type = .EQUAL, .literal = "=", .line_no = 0, .line_pos = 9 },
        .{ .token_type = .NUMBER, .literal = "5", .line_no = 0, .line_pos = 11 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 0, .line_pos = 12 },
        .{ .token_type = .VAR, .literal = "var", .line_no = 1, .line_pos = 0 },
        .{ .token_type = .IDENTIFIER, .literal = "ten", .line_no = 1, .line_pos = 4 },
        .{ .token_type = .EQUAL, .literal = "=", .line_no = 1, .line_pos = 8 },
        .{ .token_type = .NUMBER, .literal = "10.5", .line_no = 1, .line_pos = 10 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 1, .line_pos = 14 },
        .{ .token_type = .VAR, .literal = "var", .line_no = 4, .line_pos = 0 },
        .{ .token_type = .IDENTIFIER, .literal = "add", .line_no = 4, .line_pos = 4 },
        .{ .token_type = .EQUAL, .literal = "=", .line_no = 4, .line_pos = 8 },
        .{ .token_type = .FUNCTION, .literal = "fn", .line_no = 4, .line_pos = 10 },
        .{ .token_type = .LPAREN, .literal = "(", .line_no = 4, .line_pos = 12 },
        .{ .token_type = .IDENTIFIER, .literal = "x", .line_no = 4, .line_pos = 13 },
        .{ .token_type = .COMMA, .literal = ",", .line_no = 4, .line_pos = 14 },
        .{ .token_type = .IDENTIFIER, .literal = "y", .line_no = 4, .line_pos = 16 },
        .{ .token_type = .RPAREN, .literal = ")", .line_no = 4, .line_pos = 17 },
        .{ .token_type = .LBRACE, .literal = "{", .line_no = 4, .line_pos = 19 },
        .{ .token_type = .RETURN, .literal = "return", .line_no = 5, .line_pos = 2 },
        .{ .token_type = .IDENTIFIER, .literal = "x", .line_no = 5, .line_pos = 9 },
        .{ .token_type = .PLUS, .literal = "+", .line_no = 5, .line_pos = 11 },
        .{ .token_type = .IDENTIFIER, .literal = "y", .line_no = 5, .line_pos = 13 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 5, .line_pos = 14 },
        .{ .token_type = .RBRACE, .literal = "}", .line_no = 6, .line_pos = 0 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 6, .line_pos = 1 },
        .{ .token_type = .VAR, .literal = "var", .line_no = 8, .line_pos = 0 },
        .{ .token_type = .IDENTIFIER, .literal = "result", .line_no = 8, .line_pos = 4 },
        .{ .token_type = .EQUAL, .literal = "=", .line_no = 8, .line_pos = 11 },
        .{ .token_type = .IDENTIFIER, .literal = "add", .line_no = 8, .line_pos = 13 },
        .{ .token_type = .LPAREN, .literal = "(", .line_no = 8, .line_pos = 16 },
        .{ .token_type = .IDENTIFIER, .literal = "five", .line_no = 8, .line_pos = 17 },
        .{ .token_type = .COMMA, .literal = ",", .line_no = 8, .line_pos = 21 },
        .{ .token_type = .IDENTIFIER, .literal = "ten", .line_no = 8, .line_pos = 23 },
        .{ .token_type = .RPAREN, .literal = ")", .line_no = 8, .line_pos = 26 },
        .{ .token_type = .SEMICOLON, .literal = ";", .line_no = 8, .line_pos = 27 },
    };
    var actual: [expected.len]Token = undefined;
    var tokenizer = Tokenizer.init(input);
    for (0..actual.len) |i| {
        actual[i] = tokenizer.next();
    }
    try std.testing.expectEqualDeep(expected, actual);
}
