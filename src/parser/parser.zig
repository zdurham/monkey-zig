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
    //
    pub fn lessThan(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    pub fn greaterThan(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) > @intFromEnum(other);
    }
};

fn checkPrecedence(tokenType: TokenType) Precedence {
    return switch (tokenType) {
        TokenType.EQ => Precedence.EQUALS,
        TokenType.NOT_EQ => Precedence.EQUALS,
        TokenType.LT => Precedence.LESSGREATER,
        TokenType.GT => Precedence.LESSGREATER,
        TokenType.PLUS => Precedence.SUM,
        TokenType.MINUS => Precedence.SUM,
        TokenType.ASTERISK => Precedence.PRODUCT,
        TokenType.SLASH => Precedence.PRODUCT,
        else => Precedence.LOWEST,
    };
}

fn isOperator(tokenType: TokenType) bool {
    return switch (tokenType) {
        TokenType.EQ => true,
        TokenType.NOT_EQ => true,
        TokenType.LT => true,
        TokenType.GT => true,
        TokenType.PLUS => true,
        TokenType.MINUS => true,
        TokenType.ASTERISK => true,
        TokenType.SLASH => true,
        else => false,
    };
}

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
        defer self.errors.deinit();
        defer for (self.errors.items) |err| {
            self.allocator.free(err);
        };
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
        // print("currentToken in nextToken: {any} {s}\n", .{ self.currentToken.kind, self.currentToken.literal });
        // print("peekToken in nextToken: {any} {s}\n", .{ self.peekToken.kind, self.peekToken.literal });
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
        self.appendError(msg);
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

    fn prefix(self: *Self, tokenType: TokenType) ?ast.Expression {
        return switch (tokenType) {
            TokenType.IDENT => self.parseIdentifier(),
            TokenType.INT => self.parseIntegerLiteral(),
            TokenType.BANG => self.parsePrefixExpression(),
            TokenType.MINUS => self.parsePrefixExpression(),
            else => blk: {
                self.generateParseError("prefix", tokenType);
                break :blk null;
            },
        };
    }

    fn infix(self: *Self, left: *ast.Expression) ?ast.Expression {
        return switch (self.currentToken.kind) {
            TokenType.EQ => self.parseInfixExpression(left),
            TokenType.NOT_EQ => self.parseInfixExpression(left),
            TokenType.LT => self.parseInfixExpression(left),
            TokenType.GT => self.parseInfixExpression(left),
            TokenType.PLUS => self.parseInfixExpression(left),
            TokenType.MINUS => self.parseInfixExpression(left),
            TokenType.ASTERISK => self.parseInfixExpression(left),
            TokenType.SLASH => self.parseInfixExpression(left),
            else => blk: {
                self.generateParseError("infix", self.currentToken.kind);
                break :blk null;
            },
        };
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?ast.Expression {
        if (self.prefix(self.currentToken.kind)) |left| {
            var leftExpr = left;
            while (!self.peekTokenIs(TokenType.SEMICOLON) and !self.peekTokenIs(TokenType.EOF) and precedence.lessThan(self.peekPrecedence())) {
                if (isOperator(self.peekToken.kind)) {
                    self.nextToken();

                    // create a pointer here to avoid using a pointer to lextExpr
                    // which led me to create a self-referential infix expression (whoops)
                    const pLeft = self.allocator.create(ast.Expression) catch unreachable;
                    pLeft.* = leftExpr;
                    // TODO: we may need to handle error cases when we don't have an infix from self.infix(pLeft);
                    if (self.infix(pLeft)) |ifx| {
                        leftExpr = ifx;
                    }
                } else {
                    return leftExpr;
                }
            }

            return leftExpr;
        } else {
            self.generateParseError("prefix", self.currentToken.kind);
            return null;
        }
    }

    fn generateParseError(self: *Self, errorType: []const u8, tokenType: TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "No {s} parse function for {any}\n", .{ errorType, tokenType }) catch unreachable;
        self.appendError(msg);
    }

    fn parseIdentifier(self: *Self) ast.Expression {
        return ast.Expression{ .identifier = ast.Identifier{ .token = self.currentToken, .value = self.currentToken.literal } };
    }

    fn parseIntegerLiteral(self: *Self) ?ast.Expression {
        const value = std.fmt.parseUnsigned(u64, self.currentToken.literal, 10) catch {
            const msg = std.fmt.allocPrint(self.allocator, "Failed to parse {s} as an unsigned integer", .{self.currentToken.literal}) catch {
                @panic("Failed to generate error while inside parseIntegerLiteral");
            };
            self.appendError(msg);
            return null;
        };

        return ast.Expression{ .integerLiteral = ast.IntegerLiteral{ .token = self.currentToken, .value = value } };
    }

    fn parsePrefixExpression(self: *Self) ?ast.Expression {
        var prefixExpr = ast.PrefixExpression.init(self.allocator, self.currentToken, self.currentToken.literal);

        self.nextToken();
        // create a pointer to the
        if (self.parseExpression(Precedence.PREFIX)) |expr| {
            prefixExpr.createRight(expr) catch unreachable;
        }

        return ast.Expression{ .prefixExpression = prefixExpr };
    }

    fn parseInfixExpression(self: *Self, left: *ast.Expression) ?ast.Expression {
        var expr = ast.InfixExpression.init(self.allocator, self.currentToken, self.currentToken.literal, left);

        const precedence = self.currentPrecedence();

        self.nextToken();

        if (self.parseExpression(precedence)) |right| {
            expr.createRight(right) catch unreachable;
        }

        return ast.Expression{ .infixExpression = expr };
    }
    fn peekPrecedence(self: Self) Precedence {
        return checkPrecedence(self.peekToken.kind);
    }

    fn currentPrecedence(self: Self) Precedence {
        return checkPrecedence(self.currentToken.kind);
    }

    fn appendError(self: *Self, msg: []u8) void {
        self.errors.append(msg) catch {
            @panic("Failed to append error message to Parser.errors");
        };
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
    try std.testing.expectEqual(3, program.statements.items.len);
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
    try std.testing.expectEqual(3, program.statements.items.len);
    for (program.statements.items) |stmt| {
        try std.testing.expectEqual("return", stmt.tokenLiteral());
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

test "identifier expression" {
    const input = "foobar;";
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);

    try std.testing.expectEqual(1, program.statements.items.len);

    const stmt = program.statements.items[0].expressionStatement;
    const identifier = stmt.expression.?.identifier;
    const expected = ast.Identifier{
        .token = lexer.Token.new(TokenType.IDENT, "foobar"),
        .value = "foobar",
    };
    try std.testing.expectEqualDeep(expected, identifier);
}

test "integer literal expression" {
    const input = "5;";
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(std.testing.allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    defer program.deinit();
    try checkParserErrors(&parser);

    try std.testing.expectEqual(1, program.statements.items.len);

    const stmt = program.statements.items[0].expressionStatement;
    if (stmt.expression) |expr| {
        const intLiteral = expr.integerLiteral;
        const expected = ast.IntegerLiteral{
            .token = lexer.Token.new(TokenType.INT, "5"),
            .value = 5,
        };
        try std.testing.expectEqualDeep(expected, intLiteral);
    } else {
        try std.testing.expect(false);
    }
}

const PrefixTestData = struct {
    input: []const u8,
    operator: []const u8,
    integerValue: u64,
};

test "prefix operators" {
    const allocator = std.testing.allocator;
    const tests: [2]PrefixTestData = .{
        PrefixTestData{ .input = "!5;", .operator = "!", .integerValue = 5 },
        PrefixTestData{ .input = "-15;", .operator = "-", .integerValue = 15 },
    };

    // const t = tests[0];
    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var parser = Parser.init(allocator, &l);
        var program = try parser.parseProgram();
        defer parser.deinit();
        defer program.deinit();
        try checkParserErrors(&parser);

        try std.testing.expectEqual(1, program.statements.items.len);
        const expr = program.statements.items[0].expressionStatement.expression.?.prefixExpression;
        try std.testing.expectEqual(t.operator, t.operator);
        const intLit = expr.right.?.integerLiteral;
        try std.testing.expectEqual(t.integerValue, intLit.value);
    }
}

test "precedence comparisons" {
    try std.testing.expect(Precedence.LOWEST.lessThan(Precedence.SUM));
    try std.testing.expect(Precedence.SUM.greaterThan(Precedence.LOWEST));
}

const InfixTestData = struct {
    input: []const u8,
    leftValue: u64,
    operator: []const u8,
    rightValue: u64,

    pub fn new(input: []const u8, leftValue: u64, operator: []const u8, rightValue: u64) InfixTestData {
        return .{ .input = input, .leftValue = leftValue, .operator = operator, .rightValue = rightValue };
    }
};

test "infix operators" {
    const tests: [8]InfixTestData = .{
        InfixTestData.new("5 + 5;", 5, "+", 5),
        InfixTestData.new("5 - 5;", 5, "-", 5),
        InfixTestData.new("5 * 5;", 5, "*", 5),
        InfixTestData.new("5 / 5;", 5, "/", 5),
        InfixTestData.new("5 < 5;", 5, "<", 5),
        InfixTestData.new("5 > 5;", 5, ">", 5),
        InfixTestData.new("5 == 5;", 5, "==", 5),
        InfixTestData.new("5 != 5;", 5, "!=", 5),
    };

    const allocator = std.testing.allocator;

    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var parser = Parser.init(allocator, &l);
        var program = try parser.parseProgram();
        defer parser.deinit();
        defer program.deinit();

        try checkParserErrors(&parser);

        try std.testing.expectEqual(1, program.statements.items.len);

        const exp = program.statements.items[0].expressionStatement.expression.?.infixExpression;
        try std.testing.expectEqual(t.leftValue, exp.left.?.integerLiteral.value);
        try std.testing.expectEqual(t.operator, exp.operator);
        try std.testing.expectEqual(t.rightValue, exp.right.?.integerLiteral.value);
    }
}
