const tok = @import("tokenizer.zig");

pub const Parser = struct {
    tokenizer: tok.Tokenizer,
    current: tok.Token,
    next: tok.Token,

    pub fn init() Parser {
        var tokenizer = tok.Tokenizer.init();
        return Parser{
            .tokenizer = tokenizer,
            .current = tokenizer.next(),
            .next = tokenizer.next(),
        };
    }

    fn nextToken(self: *Parser) void {
        self.current = self.next;
        self.next = self.tokenizer.next();
    }
};
