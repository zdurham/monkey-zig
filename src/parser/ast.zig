const std = @import("std");
const Allocator = std.mem.Allocator;
const lexer = @import("../lexer/lexer.zig");
const WriteError = std.posix.WriteError;

pub const LetStatement = struct {
    token: lexer.Token = undefined,
    name: []const u8 = undefined,
    value: ?Expression = null,

    pub fn deinit(self: *LetStatement) void {
        if (self.value) |*expr| {
            expr.deinit();
            self.value = null;
        }
    }

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: LetStatement, writer: anytype) anyerror!void {
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
    value: ?Expression = undefined,

    pub fn deinit(self: *ReturnStatement) void {
        if (self.value) |*expr| {
            expr.deinit();
            self.value = null;
        }
    }

    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ReturnStatement, writer: anytype) anyerror!void {
        _ = try writer.write(self.token.literal);
        _ = try writer.write(" ");
        if (self.value) |rValue| {
            try rValue.toString(writer);
        }
        _ = try writer.write(";");
    }
};

pub const Identifier = struct {
    token: lexer.Token,
    value: []const u8,

    pub fn deinit(_: Identifier) void {}

    pub fn tokenLiteral(self: Identifier) anyerror!void {
        return self.token.literal();
    }

    pub fn toString(self: Identifier, writer: anytype) anyerror!void {
        _ = try writer.write(self.value);
    }
};

pub const IntegerLiteral = struct {
    token: lexer.Token,
    value: u64,

    pub fn deinit(_: IntegerLiteral) void {}

    pub fn tokenLiteral(self: *const IntegerLiteral) anyerror!void {
        return self.token.literal;
    }

    pub fn toString(self: *const IntegerLiteral, writer: anytype) anyerror!void {
        _ = try writer.write(self.token.literal);
    }
};

pub const Boolean = struct {
    token: lexer.Token,
    value: bool,

    pub fn deinit(_: Boolean) void {}

    pub fn tokenLiteral(self: *const Boolean) anyerror!void {
        return self.token.literal;
    }

    pub fn toString(self: *const Boolean, writer: anytype) anyerror!void {
        _ = try writer.write(self.token.literal);
    }
};

pub const PrefixExpression = struct {
    const Self = @This();
    allocator: Allocator,
    token: lexer.Token,
    operator: []const u8,
    right: ?*Expression,

    pub fn init(allocator: Allocator, token: lexer.Token, operator: []const u8) Self {
        return Self{
            .allocator = allocator,
            .token = token,
            .operator = operator,
            .right = null,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.right) |right| {
            // call deinit on the expression recursively
            right.deinit();
            // still have to remove this pointer
            self.allocator.destroy(right);
            self.right = null;
        }
    }

    pub fn createRight(self: *Self, expression: Expression) anyerror!void {
        // create a pointer
        // because we can't just point back to Expression
        self.right = try self.allocator.create(Expression);
        // then we assign the actual expression to the pointer
        self.right.?.* = expression;
    }

    pub fn tokenLiteral(self: *Self) anyerror!void {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        _ = try writer.write(self.operator);
        if (self.right) |right| {
            try right.toString(writer);
        }
        _ = try writer.write(")");
    }
};

pub const InfixExpression = struct {
    const Self = @This();
    allocator: Allocator,
    token: lexer.Token,
    left: ?*Expression = undefined,
    operator: []const u8,
    right: ?*Expression = undefined,

    pub fn init(allocator: Allocator, token: lexer.Token, operator: []const u8, left: ?*Expression) Self {
        return Self{
            .allocator = allocator,
            .token = token,
            .operator = operator,
            .left = left,
            .right = null,
        };
    }

    pub fn deinit(self: *Self) void {
        if (self.right) |right| {
            right.deinit();
            self.allocator.destroy(right);
            self.right = null;
        }

        if (self.left) |left| {
            left.deinit();
            self.allocator.destroy(left);
            self.left = null;
        }
    }

    pub fn createRight(self: *Self, expression: Expression) anyerror!void {
        // create a pointer
        // because we can't just point back to Expression
        self.right = try self.allocator.create(Expression);
        // then we assign the actual expression to the pointer
        self.right.?.* = expression;
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        _ = try writer.write("(");
        if (self.left) |left| {
            try left.toString(writer);
        }
        _ = try writer.write(" ");
        _ = try writer.write(self.operator);
        _ = try writer.write(" ");

        if (self.right) |right| {
            try right.toString(writer);
        }
        _ = try writer.write(")");
    }
};

pub const IfExpression = struct {
    const Self = @This();
    allocator: Allocator,
    token: lexer.Token, // if token
    condition: ?*Expression = null,
    consequence: ?*Block = null,
    alternative: ?*Block = null,

    pub fn deinit(self: *Self) void {
        if (self.condition) |condition| {
            condition.deinit();
            self.allocator.destroy(condition);
            self.condition = null;
        }
        if (self.consequence) |consequence| {
            consequence.deinit();
            self.allocator.destroy(consequence);
            self.consequence = null;
        }
        if (self.alternative) |alternative| {
            alternative.deinit();
            self.allocator.destroy(alternative);
            self.alternative = null;
        }
    }

    pub fn init(allocator: Allocator, token: lexer.Token) Self {
        return Self{
            .allocator = allocator,
            .token = token,
            .condition = null,
        };
    }

    pub fn createCondition(self: *Self, expression: Expression) !void {
        self.condition = try self.allocator.create(Expression);
        self.condition.?.* = expression;
    }

    pub fn createConsequence(self: *Self, block: Block) !void {
        self.consequence = try self.allocator.create(Block);
        self.consequence.?.* = block;
    }

    pub fn createAlternative(self: *Self, block: Block) !void {
        self.alternative = try self.allocator.create(Block);
        self.alternative.?.* = block;
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        _ = try writer.write("if");
        if (self.condition) |condition| {
            try condition.toString(writer);
        }
        _ = try writer.write(" ");
        if (self.consequence) |cons| {
            try cons.toString(writer);
        }
        if (self.alternative) |alt| {
            _ = try writer.write("else ");
            try alt.toString(writer);
        }
    }
};

pub const Block = struct {
    const Self = @This();
    allocator: Allocator,
    token: lexer.Token,
    statements: std.ArrayList(Statement),

    pub fn init(allocator: Allocator, token: lexer.Token) Self {
        return Self{
            .token = token,
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        for (self.statements.items) |*stmt| {
            stmt.*.deinit();
        }
        self.statements.deinit();
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        for (self.statements.items) |stmt| {
            try stmt.toString(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    const Self = @This();
    allocator: Allocator,
    token: lexer.Token,
    parameters: std.ArrayList(Identifier),
    body: ?*Block = null,

    pub fn init(allocator: Allocator, token: lexer.Token) Self {
        return Self{ .token = token, .allocator = allocator, .parameters = std.ArrayList(Identifier).init(allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.parameters.deinit();
        if (self.body) |body| {
            body.deinit();
            self.allocator.destroy(body);
            self.body = null;
        }
    }

    pub fn tokenLiteral(self: Self) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        // wow this is all so tedious here
        var strings = std.ArrayList([]const u8).init(self.allocator);
        defer for (strings) |string| {
            self.allocator.free(string);
        };
        defer strings.deinit();

        for (self.parameters.items) |param| {
            try strings.append(param.value);
        }

        const params = try std.mem.join(self.allocator, ",", strings.items);
        defer self.allocator.free(params);
        _ = try writer.write(self.tokenLiteral());
        _ = try writer.write("(");
        _ = try writer.write(params);
        _ = try writer.write(")");
        if (self.body) |body| {
            try body.toString();
        }
    }
};

pub const Expression = union(enum) {
    const Self = @This();
    identifier: Identifier,
    boolean: Boolean,
    infixExpression: InfixExpression,
    integerLiteral: IntegerLiteral,
    prefixExpression: PrefixExpression,
    ifExpression: IfExpression,

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            inline else => |*expr| expr.*.deinit(),
        }
    }

    pub fn toString(self: Self, writer: anytype) anyerror!void {
        switch (self) {
            inline else => |expr| try expr.toString(writer),
        }
    }
};

pub const ExpressionStatement = struct {
    const Self = @This();
    token: lexer.Token, // first token of the expression...
    expression: ?Expression = null,

    pub fn deinit(self: *Self) void {
        if (self.expression) |*expr| {
            expr.deinit();
            self.expression = null;
        }
    }

    pub fn tokenLiteral(self: ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn toString(self: ExpressionStatement, writer: anytype) anyerror!void {
        if (self.expression) |es| {
            try es.toString(writer);
        }
    }
};

pub const Statement = union(enum) {
    const Self = @This();
    letStatement: LetStatement,
    returnStatement: ReturnStatement,
    expressionStatement: ExpressionStatement,

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            inline else => |*stmt| stmt.*.deinit(),
        }
    }

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            inline else => |case| case.tokenLiteral(),
        };
    }

    pub fn toString(self: Statement, writer: anytype) anyerror!void {
        switch (self) {
            inline else => |case| try case.toString(writer),
        }
    }
};

pub const Program = struct {
    const Self = @This();
    statements: std.ArrayList(Statement),
    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .allocator = allocator,
            .statements = std.ArrayList(Statement).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        // statements are immutable, so we're using pointer capture
        // to get a pointer and modify the original values directly
        for (self.statements.items) |*stmt| {
            stmt.*.deinit();
        }
        defer self.statements.deinit();
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
    pub fn toString(self: *Self, writer: anytype) anyerror!void {
        for (self.statements.items) |stmt| {
            try stmt.toString(writer);
        }
    }
};
