const std = @import("std");
const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const tok = @import("tokenizer.zig");
const ast = @import("ast.zig");
const parse = @import("parser.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
    const gpa = general_purpose_allocator.allocator();
    var input = [_]u8{0} ** 1024;
    try stdout.writeAll("Beetle 0.0.0\n");
    try stdout.writeAll("> ");
    while (try stdin.readUntilDelimiterOrEof(&input, '\n')) |line| {
        const tokenizer = tok.Tokenizer.init(line);
        var parser = parse.Parser.init(gpa, tokenizer);
        var module = parser.parseModule();
        for (module.statements.items) |statement| {
            try stdout.print("{}\n", .{statement});
        }
        module.run();
        module.deinit(gpa);
        try stdout.writeAll("> ");
    }
}
