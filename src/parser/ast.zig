const std = @import("std");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");

pub const LetStatement = struct {
    token: lexer.Token = undefined,
    name: []const u8 = undefined,
    // value: ?Expression TODO: implement expressions

    pub fn tokenLiteral(self: LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ReturnStatement = struct {
    token: lexer.Token,
    // returnValue: Expression, // TODO: implement expression
    pub fn tokenLiteral(self: ReturnStatement) []const u8 {
        return self.token.literal;
    }
};

pub const Statement = union(enum) {
    letStatement: LetStatement,
    returnStatement: ReturnStatement,

    pub fn tokenLiteral(self: Statement) []const u8 {
        return switch (self) {
            inline else => |case| case.tokenLiteral(),
        };
    }
};

const Expression = struct {
    pub fn tokenLiteral() []const u8 {}
    pub fn expressionNode() void {}
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

    fn get_token_literal(self: *Self) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};
