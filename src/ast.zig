const std = @import("std");

const tok = @import("tokenizer.zig");

pub const Expression = union(enum) {
    identifier_expression: IdentifierExpression,
    number_literal_expression: NumberLiteralExpression,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    boolean_expression: BooleanExpression,

    pub fn create(allocator: std.mem.Allocator, options: Expression) *Expression {
        const new: *Expression = allocator.create(Expression) catch {
            unreachable;
        };
        new.* = options;
        return new;
    }

    pub fn destroy(self: *Expression, allocator: std.mem.Allocator) void {
        switch (self.*) {
            inline else => |*s| s.destroy(allocator),
        }
        allocator.destroy(self);
    }

    pub fn toString(self: *const Expression, allocator: std.mem.Allocator) []const u8 {
        return switch (self.*) {
            inline else => |*s| s.toString(allocator),
        };
    }

    fn eval(self: *Expression) void {
        switch (self.*) {
            inline else => |s| s.eval(),
        }
    }
};

pub const PrefixExpression = struct {
    operator: tok.Token,
    expression: *Expression,

    fn destroy(self: *PrefixExpression, allocator: std.mem.Allocator) void {
        self.expression.destroy(allocator);
    }

    fn toString(self: *const PrefixExpression, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "{s}{s}",
            .{ self.operator.literal, self.expression.toString(allocator) },
        ) catch {
            return "ERROR";
        };
    }

    fn eval(_: PrefixExpression) void {}
};

pub const InfixExpression = struct {
    operator: tok.Token,
    left: *Expression,
    right: *Expression,

    fn destroy(self: *InfixExpression, allocator: std.mem.Allocator) void {
        self.left.destroy(allocator);
        self.right.destroy(allocator);
    }

    fn toString(self: *const InfixExpression, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "({s} {s} {s})",
            .{ self.left.toString(allocator), self.operator.literal, self.right.toString(allocator) },
        ) catch {
            return "ERROR";
        };
    }

    fn eval(_: InfixExpression) void {}
};

pub const IdentifierExpression = struct {
    token: tok.Token,

    fn destroy(_: *IdentifierExpression, _: std.mem.Allocator) void {}

    fn toString(self: *const IdentifierExpression, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.token.literal}) catch {
            return "ERROR";
        };
    }

    fn eval(_: IdentifierExpression) void {}
};

pub const NumberLiteralExpression = struct {
    value: f64,

    fn destroy(_: *NumberLiteralExpression, _: std.mem.Allocator) void {}

    fn toString(self: *const NumberLiteralExpression, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{d}", .{self.value}) catch {
            return "ERROR";
        };
    }

    fn eval(_: NumberLiteralExpression) void {}
};

pub const BooleanExpression = struct {
    value: bool,

    fn destroy(_: *BooleanExpression, _: std.mem.Allocator) void {}

    fn toString(self: *const BooleanExpression, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{}", .{self.value}) catch {
            return "ERROR";
        };
    }

    fn eval(_: BooleanExpression) void {}
};

pub const Statement = union(enum) {
    var_statement: VarStatement,
    return_statement: ReturnStatement,
    expression_statement: ExpressionStatement,
    if_statement: IfStatement,

    pub fn toString(self: *const Statement, allocator: std.mem.Allocator) []const u8 {
        return switch (self.*) {
            inline else => |*s| s.toString(allocator),
        };
    }

    fn execute(self: *Statement) void {
        switch (self.*) {
            inline else => |*s| s.execute(),
        }
    }
};

pub const ReturnStatement = struct {
    value: *Expression,

    fn toString(self: *const ReturnStatement, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "return {s};",
            .{self.value.toString(allocator)},
        ) catch {
            return "ERROR";
        };
    }

    fn execute(self: *ReturnStatement) void {
        self.value.eval();
    }
};

pub const VarStatement = struct {
    identifier: IdentifierExpression,
    value: *Expression,

    fn toString(self: *const VarStatement, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(
            allocator,
            "var {s} = {s};",
            .{ self.identifier.toString(allocator), self.value.toString(allocator) },
        ) catch {
            return "ERROR";
        };
    }

    fn execute(self: *VarStatement) void {
        self.value.eval();
    }
};

pub const ExpressionStatement = struct {
    value: *Expression,

    fn toString(self: *const ExpressionStatement, allocator: std.mem.Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.value.toString(allocator)}) catch {
            return "ERROR";
        };
    }

    fn execute(self: *ExpressionStatement) void {
        self.value.eval();
    }
};

pub const BlockStatement = struct {
    statements: std.ArrayList(Statement),

    pub fn init(allocator: std.mem.Allocator) BlockStatement {
        return BlockStatement{
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    fn toString(self: *const BlockStatement, allocator: std.mem.Allocator) []const u8 {
        var accumulator: []const u8 = "";
        for (self.statements.items) |*statement| {
            accumulator = std.fmt.allocPrint(
                allocator,
                "{s}\n    {s}",
                .{ accumulator, statement.toString(allocator) },
            ) catch {
                return "ERROR";
            };
        }
        return accumulator;
    }
};

pub const IfStatement = struct {
    condition: *Expression,
    truthy: BlockStatement,
    falsey: ?BlockStatement,

    fn toString(self: *const IfStatement, allocator: std.mem.Allocator) []const u8 {
        var accumulator: []const u8 = std.fmt.allocPrint(
            allocator,
            "if ({s}) {{{s}\n}}",
            .{ self.condition.toString(allocator), self.truthy.toString(allocator) },
        ) catch {
            return "ERROR";
        };
        if (self.falsey) |falsey| {
            accumulator = std.fmt.allocPrint(
                allocator,
                "{s} else {{{s}\n}}",
                .{ accumulator, falsey.toString(allocator) },
            ) catch {
                return "ERROR";
            };
        }
        return accumulator;
    }

    fn execute(self: *IfStatement) void {
        self.condition.eval();
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
