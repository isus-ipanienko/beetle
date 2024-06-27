const std = @import("std");

const tok = @import("tokenizer.zig");

pub const Expression = union(enum) {
    identifier_expression: IdentifierExpression,
    number_literal_expression: NumberLiteralExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,

    pub fn create(allocator: std.mem.Allocator, options: Expression) *Expression {
        const new: *Expression = allocator.create(Expression) catch {
            unreachable;
        };
        new.* = options;
        return new;
    }

    fn destroy(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self) {
            inline else => |s| s.destroy(allocator),
        }
        allocator.destroy(self);
    }

    fn eval(self: *Expression) void {
        switch (self.*) {
            inline else => |s| s.eval(),
        }
    }
};

pub const PrefixExpression = struct {
    operator: tok.TokenType,
    expression: *Expression,

    fn destroy(self: *PrefixExpression, allocator: std.mem.Allocator) void {
        self.expression.destroy(allocator);
    }

    fn eval(_: PrefixExpression) void {}
};

pub const InfixExpression = struct {
    operator: tok.TokenType,
    left: *Expression,
    right: *Expression,

    fn destroy(self: *InfixExpression, allocator: std.mem.Allocator) void {
        self.left.destroy(allocator);
        self.right.destroy(allocator);
    }

    fn eval(_: InfixExpression) void {}
};

pub const IdentifierExpression = struct {
    token: tok.Token,

    fn destroy(_: *IdentifierExpression, _: std.mem.Allocator) void {}

    fn eval(_: IdentifierExpression) void {}
};

pub const NumberLiteralExpression = struct {
    value: f64,

    fn destroy(_: *NumberLiteralExpression, _: std.mem.Allocator) void {}

    fn eval(_: NumberLiteralExpression) void {}
};

pub const Statement = union(enum) {
    var_statement: VarStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,

    fn execute(self: *Statement) void {
        switch (self.*) {
            inline else => |*s| s.execute(),
        }
    }
};

pub const ReturnStatement = struct {
    value: *Expression,

    fn execute(self: *ReturnStatement) void {
        self.value.eval();
    }
};

pub const VarStatement = struct {
    identifier: IdentifierExpression,
    value: *Expression,

    fn execute(self: *VarStatement) void {
        self.value.eval();
    }
};

pub const ExpressionStatement = struct {
    value: *Expression,

    fn execute(self: *ExpressionStatement) void {
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
