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

    fn parseExpressionStatement(self: *Self) ast.Statement {
        var exprStatement = ast.ExpressionStatement{ .token = self.currentToken };
        exprStatement.expression = self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }

        return ast.Statement{ .expressionStatement = exprStatement };
    }

    fn prefix(self: *Self, kind: TokenType) ?ast.Expression {
        return switch (kind) {
            TokenType.IDENT => self.parseIdentifier(),
            else => null, // TODO: replace this with error handling
        };
    }

    // TODO: implement switch
    fn infix(self: *Self, expression: ast.Expression) ast.Expression {
        _ = self;
        _ = expression;
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?ast.Expression {
        _ = precedence;
        const leftExp = self.prefix(self.currentToken.kind);
        return leftExp;
    }

    fn parseIdentifier(self: *Self) ast.Expression {
        return ast.Expression{ .identifier = ast.Identifier{ .token = self.currentToken, .value = self.currentToken.literal } };
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

test "toString methods" {
    const alloc = std.testing.allocator;
    var program = ast.Program.init(alloc);
    defer program.deinit();
    const letStatement = ast.Statement{
        .letStatement = ast.LetStatement{
            .token = lexer.Token.new(TokenType.LET, "let"),
            .name = "myVar",
            .value = ast.Expression{
                .identifier = ast.Identifier{
                    .token = lexer.Token.new(TokenType.IDENT, "anotherVar"),
                    .value = "anotherVar",
                },
            },
        },
    };
    try program.statements.append(letStatement);
    var actual = std.ArrayList(u8).init(alloc);
    var expected = std.ArrayList(u8).init(alloc);
    defer actual.deinit();
    defer expected.deinit();
    try expected.appendSlice("let myVar = anotherVar;");
    try program.toString(actual.writer());
    try std.testing.expectEqualStrings(expected.items, actual.items);
}

// test "identifier expression" {
//     const input = "foobar;";
//     var l = lexer.Lexer.init(input);
//     var parser = Parser.init(std.testing.allocator, &l);
//     var program = try parser.parseProgram();
//     defer parser.deinit();
//     defer program.deinit();
//     try checkParserErrors(&parser);
//
//     try std.testing.expectEqual(program.statements.items.len, 1);
//     const stmt = program.statements.items[0].expressionStatement;
//     const identifier = stmt.expression;
//     _ = identifier;
// }
