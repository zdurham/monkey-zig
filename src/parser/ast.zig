const std = @import("std");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");

pub const LetStatement = struct {
    const Self = @This();
    token: lexer.Token = undefined,
    name: []const u8 = undefined,
    // value: ?Expression TODO: implement expressions

    pub fn tokenLiteral(self: *Self) []const u8 {
        return self.token.value;
    }
};

pub const Statement = union(enum) {
    const Self = @This();

    letStatement: LetStatement,

    pub fn tokenLiteral(self: *Self) []const u8 {
        _ = self;
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
