const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const tok = @import("tokenizer.zig");
const ast = @import("ast.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
    const gpa = general_purpose_allocator.allocator();
    var module = ast.Module.init(gpa);
    defer module.deinit();
    var input = [_]u8{0} ** 1024;
    try stdout.writeAll("Beetle 0.0.0\n");
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEof(&input, '\n')) |line| {
        var tokenizer = tok.Tokenizer.init(line);
        var token: tok.Token = tokenizer.next();
        while (token.token_type != tok.TokenType.EOF) : (token = tokenizer.next()) {
            try stdout.print("{}\n", .{token});
        }
        try stdout.writeAll("> ");
    }
}
