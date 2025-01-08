const std = @import("std");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");

pub const LetStatement = struct {
    token: lexer.Token = undefined,
    name: []const u8 = undefined,
    value: ?ExpressionStatement = undefined,

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: LetStatement, writer: anytype) !void {
        _ = try writer.write(self.tokenLiteral());
        _ = try writer.write(" ");
        _ = try writer.write(self.name);
        _ = try writer.write(" = ");
        if (self.value) |_| {
            std.debug.print("ok we have an expr", .{});
        }

        _ = try writer.write(";");
    }
};

pub const ReturnStatement = struct {
    token: lexer.Token,
    returnValue: ?ExpressionStatement = undefined,
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ReturnStatement, writer: anytype) !void {
        _ = try writer.write(self.token.literal);
        _ = try writer.write(" ");
        if (self.returnValue) |rValue| {
            try rValue.toString(writer);
        }
        _ = try writer.write(";");
    }
};

pub const Expression = union(enum) {
    // NOTE: Does this need to be here?
    token: lexer.Token,
    // TODO: enumerate various expressions
    pub fn toString(self: Expression, writer: anytype) !void {
        _ = self;
        _ = writer;
        // TODO: implement cases
        // switch (self) {
        //     inline else => |case| try case.toString(writer),
        // }
    }
};

pub const ExpressionStatement = struct {
    token: lexer.Token,
    expression: Expression,

    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ExpressionStatement, writer: anytype) !void {
        // if (self.expression) |es| {
        try self.expression.toString(writer);
        // }
    }
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expressionStatement: ExpressionStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            inline else => |case| case.tokenLiteral(),
        };
    }

    pub fn toString(self: Statement, writer: anytype) !void {
        switch (self) {
            inline else => |case| try case.toString(writer),
        }
    }
};

pub const Program = struct {
    const Self = @This();
    statements: std.ArrayList(Statement),
    allocator: mem.Allocator,

    pub fn init(allocator: mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.statements.deinit();
    }

    fn getTokenLiteral(self: *Self) []const u8 {
        if (self.statements.items.len > 0) {
            return self.statements.items[0].tokenLiteral();
        } else {
            return "";
        }
    }

    // we'll pass an arbitrary writer() in to this function
    // and all children will use it as well
    pub fn toString(self: *Self, writer: anytype) !void {
        for (self.statements.items) |stmt| {
            try stmt.toString(writer);
        }
    }
};
