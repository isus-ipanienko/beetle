const std = @import("std");

const tok = @import("tokenizer.zig");

pub const Identifier = struct {
    token: tok.Token,
};

const stdout = std.io.getStdOut().writer();
pub const IExpression = struct {
    evalFn: *const fn (*IExpression) void,

    fn eval(_: *IExpression) void {
        stdout.writeAll("Evaluated!\n") catch {};
        // iface.evalFn(iface);
    }
};

const IStatementTable = struct {
    executeFn: *const fn (*IStatement) void,
    deinitFn: *const fn (*IStatement, std.mem.Allocator) void,
};

pub const IStatement = struct {
    self: *anyopaque,
    vtable: *const IStatementTable,

    fn execute(iface: *IStatement) void {
        iface.vtable.executeFn(@alignCast(@ptrCast(iface.self)));
    }

    fn deinit(iface: *IStatement, allocator: std.mem.Allocator) void {
        iface.vtable.deinitFn(@alignCast(@ptrCast(iface.self)), allocator);
    }
};

const varStatementVTable = IStatementTable{
    .executeFn = VarStatement.execute,
    .deinitFn = VarStatement.deinit,
};

pub const VarStatement = struct {
    iStatement: IStatement,
    identifier: Identifier,
    expression: IExpression,

    pub fn init(allocator: std.mem.Allocator, identifier: Identifier, expression: IExpression) ?*VarStatement {
        const vs = allocator.create(VarStatement) catch {
            return null;
        };
        vs.* = VarStatement{
            .iStatement = IStatement{ .self = vs, .vtable = &varStatementVTable },
            .identifier = identifier,
            .expression = expression,
        };
        return vs;
    }

    fn deinit(iface: *IStatement, allocator: std.mem.Allocator) void {
        const self: *VarStatement = @fieldParentPtr("iStatement", iface);
        allocator.destroy(self);
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

    pub fn deinit(self: *Module, allocator: std.mem.Allocator) void {
        for (self.statements.items) |*statement| {
            statement.deinit(allocator);
        }
        self.statements.deinit();
    }

    pub fn run(self: *Module) void {
        for (self.statements.items) |*statement| {
            statement.execute();
        }
    }
};
