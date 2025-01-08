const std = @import("std");
const ast = @import("./ast.zig");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");
const TokenType = lexer.TokenType;
const print = std.debug.print;

const Precedence = enum {
    LOWEST,
    EQUALS, // ==,
    LESSGREATER, // < or >
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunc(X)
};

const Parser = struct {
    const Self = @This();
    lexer: *lexer.Lexer,
    allocator: mem.Allocator,

    currentToken: lexer.Token = undefined,
    peekToken: lexer.Token = undefined,

    errors: std.ArrayList([]const u8),

    // preFixParseFns =

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
            else => self.parseExpressionStatement(),
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

    fn parseExpressionStatement(self: *Self) ast.ExpressionStatement {
        var stmt = ast.ExpressionStatement{ .token = self.currentToken };
        stmt.Expression = self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return stmt;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?ast.Expression {
        _ = precedence;
        const leftExp = self.prefix(self.currentToken.kind);
        return leftExp;
    }

    fn prefix(self: *Self, kind: TokenType) ?ast.Expression {
        _ = self;
        return switch (kind) {
            _ => "thing",
        };
    }

    fn infix(self: *Self, expression: ast.Expression) ast.Expression {
        _ = self;
        _ = expression;
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

// TODO: fix this test homie (by implementing stuff)
test "toString methods" {
    const input =
        \\let x = 5;
        \\return 10;
        \\return 993322;
    ;

    var l = lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);

    var stringList = std.ArrayList(u8).init(std.testing.allocator);
    defer stringList.deinit();
    const writer = stringList.writer();
    try program.toString(writer);
    std.debug.print("What you is buddy: {s}\n\n", .{stringList.items});
    const outcome = try stringList.toOwnedSlice();

    try std.testing.expectEqualSlices(u8, input[0..], outcome);
}

test "identifier expression" {
    const input = "foobar;";
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);

    try std.testing.expectEqual(program.statements.items.len, 1);
    const stmt = program.statements.items[0].expressionStatement;
    const identifier = stmt.expression;
    _ = identifier;
}
