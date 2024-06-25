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

    fn execute(self: *Statement) void {
        switch (self.*) {
            inline else => |*s| s.execute(),
        }
    }
};

pub const VarStatement = struct {
    identifier: Identifier,
    expression: Expression,

    pub fn init(identifier: Identifier, expression: Expression) VarStatement {
        return VarStatement{
            .identifier = identifier,
            .expression = expression,
        };
    }

    fn execute(self: *VarStatement) void {
        self.expression.eval();
    }
};

pub const Module = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) Module {
        const statements = std.ArrayList(Statement).init(allocator);
        return Module{
            .statements = statements,
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
