const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const tok = @import("tokenizer.zig");

pub fn main() !void {
    var input = [_]u8{0} ** 1024;
    try stdout.writeAll(">> ");
    while (try stdin.readUntilDelimiterOrEof(&input, '\n')) |line| {
        var tokenizer = tok.Tokenizer.init(line);
        var token: tok.Token = tokenizer.next();
        while (token.token_type != tok.TokenType.EOF) : (token = tokenizer.next()) {
            try stdout.print("{}\n", .{token});
        }
        try stdout.writeAll(">> ");
    }
}
