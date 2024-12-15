const std = @import("std");
const lexer = @import("../lexer/lexer.zig");

const Identifier = struct { token: lexer.Token, value: []const u8 };

const LetStatement = struct {
    const Self = @This();
    token: lexer.Token,
    name: *Identifier,
    value: Expression,

    pub fn tokenLiteral() []const u8 {
        return Self.token.IDENT;
    }
};

// const Node = union(enum) {
//     statement: Statement,
//     expression: Expression,
//
//     pub fn tokenLiteral() void {}
// };

pub const Statement = union(enum) {
    const Self = @This();
    pub fn tokenLiteral(self: *Self) []const u8 {
        _ = self;
    }
};

// pub const Statement = struct {
//     pub fn tokenLiteral() []const u8 {}
//     pub fn statementNode() void {}
// };

const Expression = struct {
    pub fn tokenLiteral() []const u8 {}
    pub fn expressionNode() void {}
};

pub const Program = struct {
    const Self = @This();
    statements: std.ArrayList(Statement),

    fn get_token_literal(self: *Self) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};
