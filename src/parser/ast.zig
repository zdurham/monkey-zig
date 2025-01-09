const std = @import("std");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");

pub const LetStatement = struct {
    token: lexer.Token = undefined,
    name: []const u8 = undefined,
    value: ?Expression = undefined,

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: LetStatement, writer: anytype) !void {
        _ = try writer.write(self.tokenLiteral());
        _ = try writer.write(" ");
        _ = try writer.write(self.name);
        _ = try writer.write(" = ");
        if (self.value) |expr| {
            _ = try expr.toString(writer);
        }

        _ = try writer.write(";");
    }
};

pub const ReturnStatement = struct {
    token: lexer.Token,
    returnValue: ?Expression = undefined,
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

pub const Identifier = struct {
    token: lexer.Token,
    value: []const u8,

    pub fn tokenLiteral(self: Identifier) !void {
        return self.token.literal();
    }

    pub fn toString(self: Identifier, writer: anytype) !void {
        _ = try writer.write(self.value);
    }
};

pub const IntegerLiteral = struct {
    token: lexer.Token,
    value: u64,

    pub fn tokenLiteral(self: *const IntegerLiteral) !void {
        return self.token.literal;
    }

    pub fn toString(self: *const IntegerLiteral, writer: anytype) !void {
        _ = try writer.write(self.token.literal);
    }
};

pub const PrefixExpression = struct {
    const Self = @This();
    allocator: mem.Allocator,
    token: lexer.Token,
    operator: []const u8,
    right: ?*Expression,

    pub fn init(allocator: mem.Allocator, token: lexer.Token, operator: []const u8) Self {
        return Self{
            .allocator = allocator,
            .token = token,
            .operator = operator,
            .right = null,
        };
    }
    // TODO: we will eventually need to deinit the right expression
    // and furthermore provide a deinit on all expressions
    // pub fn deinit(_: *Self) void {
    // }

    pub fn createRight(self: *Self, expression: Expression) !void {
        // create a pointer
        // because we can't just point back to Expression
        self.right = try self.allocator.create(Expression);
        // then we assign the actual expression to the pointer
        self.right.?.* = expression;
    }

    pub fn tokenLiteral(self: *Self) !void {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) !void {
        _ = try writer.write("(");
        _ = try writer.write(self.operator);
        if (self.right) |right| {
            std.debug.print("{any}\n", .{right});
            // try right.toString(writer);
        }
        _ = try writer.write(")");
    }
};

pub const Expression = union(enum) {
    const Self = @This();
    identifier: Identifier,
    integerLiteral: IntegerLiteral,
    prefixExpression: PrefixExpression,

    pub fn toString(self: Self, writer: anytype) !void {
        switch (self) {
            inline else => |expr| try expr.toString(writer),
        }
    }
};

pub const ExpressionStatement = struct {
    token: lexer.Token, // first token of the expression...
    expression: ?Expression = undefined,

    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ExpressionStatement, writer: anytype) !void {
        if (self.expression) |es| {
            try es.toString(writer);
        }
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
