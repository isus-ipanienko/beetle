const std = @import("std");

const tok = @import("tokenizer.zig");

const IExpression = struct {
    evalFn: *const fn (*IExpression) void,

    fn eval(iface: *IExpression) void {
        iface.evalFn(iface);
    }
};

const IStatement = struct {
    executeFn: *const fn (*IStatement) void,

    fn execute(iface: *IStatement) void {
        iface.executeFn(iface);
    }
};

const VarStatement = struct {
    iStatement: IStatement,
    identifier: tok.Token,
    expression: IExpression,

    fn init(token: tok.Token) VarStatement {
        return VarStatement{
            .iStatement = IStatement{ .executeFn = execute },
            .identifier = token,
        };
    }

    fn execute(iface: *IStatement) void {
        var self: *VarStatement = @fieldParentPtr("iStatement", iface);
        self.expression.eval();
    }
};

pub const Module = struct {
    statements: std.ArrayList(IStatement),

    pub fn init(allocator: std.mem.Allocator) Module {
        const statements = std.ArrayList(IStatement).init(allocator);
        return Module{
            .statements = statements,
        };
    }

    pub fn deinit(self: *Module) void {
        self.statements.deinit();
    }

    fn run(self: *Module) void {
        for (self.statements.items) |statement| {
            statement.execute();
        }
    }
};
