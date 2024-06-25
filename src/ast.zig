const std = @import("std");

const tok = @import("tokenizer.zig");

pub const Identifier = struct {
    token: tok.Token,
};

pub const Expression = union(enum) {
    dummy_expression: DummyExpression,

    fn eval(self: Expression) void {
        switch (self) {
            inline else => |s| s.eval(),
        }
    }
};

const stdout = std.io.getStdOut().writer();
pub const DummyExpression = struct {
    fn eval(_: DummyExpression) void {
        stdout.writeAll("Evaluated!\n") catch {};
    }
};

pub const Statement = union(enum) {
    var_statement: VarStatement,
    return_statement: ReturnStatement,

    fn execute(self: *Statement) void {
        switch (self.*) {
            inline else => |*s| s.execute(),
        }
    }
};

pub const ReturnStatement = struct {
    value: Expression,

    fn execute(self: *ReturnStatement) void {
        self.value.eval();
    }
};

pub const VarStatement = struct {
    identifier: Identifier,
    value: Expression,

    fn execute(self: *VarStatement) void {
        self.value.eval();
    }
};

pub const Module = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Module {
        return Module{
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Module) void {
        self.statements.deinit();
    }

    pub fn run(self: *Module) void {
        for (self.statements.items) |*statement| {
            statement.execute();
        }
    }
};
