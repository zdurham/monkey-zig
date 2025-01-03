const std = @import("std");
const ast = @import("./ast.zig");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");
const TokenType = lexer.TokenType;
const print = std.debug.print;

const Parser = struct {
    const Self = @This();
    lexer: *lexer.Lexer,
    allocator: mem.Allocator,
    currentToken: lexer.Token = undefined,
    peekToken: lexer.Token = undefined,
    errors: std.ArrayList([]const u8),

    pub fn init(allocator: mem.Allocator, l: *lexer.Lexer) Self {
        const errors = std.ArrayList([]const u8).init(allocator);
        var parser = Parser{
            .lexer = l,
            .allocator = allocator,
            .errors = errors,
        };
        parser.nextToken();
        parser.nextToken();
        return parser;
    }

    pub fn deinit(self: *Self) void {
        defer for (self.errors.items) |err| {
            self.allocator.free(err);
        };
        defer self.errors.deinit();
    }

    pub fn parseProgram(self: *Self) !ast.Program {
        var program = ast.Program.init(self.allocator);
        while (!self.currentTokenIs(TokenType.EOF)) {
            const statement = self.parseStatement();
            if (statement) |s| {
                try program.statements.append(s);
            }
            self.nextToken();
        }
        return program;
    }

    fn parseStatement(self: *Self) ?ast.Statement {
        return switch (self.currentToken.kind) {
            TokenType.LET => self.parseLetStatement(),
            TokenType.RETURN => self.parseReturnStatement(),
            else => null,
        };
    }

    fn parseLetStatement(self: *Self) ?ast.Statement {
        var stmt = ast.LetStatement{
            .token = self.currentToken,
        };
        if (!self.expectPeek(TokenType.IDENT)) {
            return null;
        }

        stmt.name = self.currentToken.literal;

        if (!self.expectPeek(TokenType.ASSIGN)) {
            return null;
        }
        while (!self.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }
        return ast.Statement{ .letStatement = stmt };
    }

    fn parseReturnStatement(self: *Self) ?ast.Statement {
        const stmt = ast.ReturnStatement{ .token = self.currentToken };
        self.nextToken();

        // for now skipping the expression itself
        while (!self.currentTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return ast.Statement{ .returnStatement = stmt };
    }

    fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    fn currentTokenIs(self: *Self, kind: TokenType) bool {
        return self.currentToken.kind == kind;
    }

    fn peekTokenIs(self: *Self, kind: TokenType) bool {
        return self.peekToken.kind == kind;
    }

    fn expectPeek(self: *Self, kind: TokenType) bool {
        if (self.peekTokenIs(kind)) {
            self.nextToken();
            return true;
        } else {
            self.peekError(kind);
            return false;
        }
    }

    fn peekError(self: *Self, kind: TokenType) void {
        // using catch to avoid propagating an error union
        const msg = std.fmt.allocPrint(self.allocator, "Expected next token to be {any}, got {any} instead", .{ kind, &self.peekToken.kind }) catch {
            @panic("Failed to create error message inside peekError");
        };
        self.errors.append(msg) catch {
            @panic("Failed to append error message to Parser.errors");
        };
    }

    pub fn getErrors(self: *Self) [][]const u8 {
        return self.errors.items;
    }
};

fn checkParserErrors(parser: *Parser) !void {
    const errors = parser.getErrors();
    if (errors.len == 0) {
        return;
    }
    std.debug.print("Parser has errors: {d}\n", .{errors.len});
    for (errors) |err| {
        std.debug.print("Parser error: {s}\n", .{err});
    }
    // fail the tests now since we didn't want errors
    try std.testing.expect(errors.len == 0);
}

test "Test let statements" {
    const allocator = std.testing.allocator;
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);
    try std.testing.expect(program.statements.items.len == 3);
}

test "test return statements" {
    const input =
        \\return 5;
        \\return 10;
        \\return 993322;
    ;

    var l = lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);
    try std.testing.expect(program.statements.items.len == 3);
    for (program.statements.items) |stmt| {
        try std.testing.expectEqual(stmt.tokenLiteral(), "return");
    }
}
