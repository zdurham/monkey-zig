const std = @import("std");
const ast = @import("./ast.zig");
const mem = std.mem;
const lexer = @import("../lexer/lexer.zig");
const TokenType = lexer.TokenType;

const Precedence = enum {
    LOWEST,
    EQUALS, // ==,
    LESSGREATER, // < or >
    SUM, // +
    PRODUCT, // *
    PREFIX, // -X or !X
    CALL, // myFunc(X)

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
        TokenType.LPAREN => Precedence.CALL,
        else => Precedence.LOWEST,
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
            if (self.parseStatement()) |stmt| {
                try program.statements.append(stmt);
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

        // skipping the ASSIGN token
        self.nextToken();

        stmt.value = self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(TokenType.SEMICOLON)) {
            self.nextToken();
        }
        return ast.Statement{ .letStatement = stmt };
    }

    fn parseReturnStatement(self: *Self) ?ast.Statement {
        var stmt = ast.ReturnStatement{ .token = self.currentToken };
        self.nextToken();

        stmt.value = self.parseExpression(Precedence.LOWEST);

        if (self.peekTokenIs(TokenType.SEMICOLON)) {
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
            TokenType.TRUE => self.parseBoolean(),
            TokenType.FALSE => self.parseBoolean(),
            TokenType.LPAREN => self.parseGroupedExpression(),
            TokenType.IF => self.parseIfExpression(),
            TokenType.FUNCTION => self.parseFunctionLiteral(),

            else => blk: {
                self.generateParseError("prefix", tokenType, self.currentToken.literal);
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
            TokenType.LPAREN => self.parseCallExpression(left),
            else => blk: {
                self.generateParseError("infix", self.currentToken.kind, self.currentToken.literal);
                break :blk null;
            },
        };
    }

    fn parseExpression(self: *Self, precedence: Precedence) ?ast.Expression {
        if (self.prefix(self.currentToken.kind)) |left| {
            var leftExpr = left;
            while (!(self.peekTokenIs(TokenType.SEMICOLON) or self.peekTokenIs(TokenType.EOF)) and precedence.lessThan(self.peekPrecedence())) {
                self.nextToken();
                // create a pointer here to avoid using a pointer to lextExpr
                // which led me to create a self-referential infix expression (whoops)
                const pLeft = self.allocator.create(ast.Expression) catch unreachable;
                pLeft.* = leftExpr;
                if (self.infix(pLeft)) |ifx| {
                    leftExpr = ifx;
                } else {
                    self.allocator.destroy(pLeft);
                }
            }

            return leftExpr;
        } else {
            self.generateParseError("prefix", self.currentToken.kind, self.currentToken.literal);
            return null;
        }
    }

    fn parseGroupedExpression(self: *Self) ?ast.Expression {
        self.nextToken();
        const expr = self.parseExpression(Precedence.LOWEST);
        if (!self.expectPeek(TokenType.RPAREN)) {
            return null;
        }

        return expr;
    }

    fn generateParseError(self: *Self, errorType: []const u8, tokenType: TokenType, tokenValue: []const u8) void {
        const msg = std.fmt.allocPrint(self.allocator, "No {s} parse function for {any}: {s}\n", .{ errorType, tokenType, tokenValue }) catch unreachable;
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

    fn parseBoolean(self: *Self) ast.Expression {
        return ast.Expression{ .boolean = ast.Boolean{
            .token = self.currentToken,
            .value = self.currentTokenIs(TokenType.TRUE),
        } };
    }

    fn parseIfExpression(self: *Self) ?ast.Expression {
        var ifExpr = ast.IfExpression.init(self.allocator, self.currentToken);

        if (!self.expectPeek(TokenType.LPAREN)) {
            return null;
        }
        self.nextToken();

        // parse condition expression
        if (self.parseExpression(Precedence.LOWEST)) |condition| {
            ifExpr.createCondition(condition) catch unreachable;
        }

        if (!self.expectPeek(TokenType.RPAREN)) {
            ifExpr.deinit();
            return null;
        }

        if (!self.expectPeek(TokenType.LBRACE)) {
            ifExpr.deinit();
            return null;
        }

        ifExpr.createConsequence(self.parseBlock()) catch unreachable;

        if (self.peekTokenIs(TokenType.ELSE)) {
            self.nextToken();

            if (!self.expectPeek(TokenType.LBRACE)) {
                ifExpr.deinit();
                return null;
            }

            ifExpr.createAlternative(self.parseBlock()) catch unreachable;
        }
        return ast.Expression{ .ifExpression = ifExpr };
    }

    fn parseFunctionLiteral(self: *Self) ?ast.Expression {
        var funcLit = ast.FunctionLiteral.init(self.allocator, self.currentToken);

        if (!self.expectPeek(TokenType.LPAREN)) {
            funcLit.deinit();
            return null;
        }

        self.parseFunctionParameters(&funcLit.parameters);

        if (!self.expectPeek(TokenType.LBRACE)) {
            funcLit.deinit();
            return null;
        }

        funcLit.body = self.parseBlock();
        return ast.Expression{ .functionLiteral = funcLit };
    }

    fn parseFunctionParameters(self: *Self, paramList: *std.ArrayList(ast.Identifier)) void {
        if (self.peekTokenIs(TokenType.RPAREN)) {
            self.nextToken();
            return;
        } else {
            self.nextToken();

            // first identifier
            paramList.append(ast.Identifier{ .token = self.currentToken, .value = self.currentToken.literal }) catch unreachable;

            // iterate through remaining params if they exist
            while (self.peekTokenIs(TokenType.COMMA)) {
                self.nextToken(); // curr token is comma
                self.nextToken(); // curr token is param

                paramList.append(ast.Identifier{ .token = self.currentToken, .value = self.currentToken.literal }) catch unreachable;
            }
        }

        if (!self.expectPeek(TokenType.RPAREN)) {
            // TODO: probably need to deinit each item in list
            // as well as the list itself
            paramList.deinit();
        }
    }

    fn parseCallExpression(self: *Self, function: *ast.Expression) ?ast.Expression {
        var callExpression = ast.CallExpression.init(
            self.allocator,
            self.currentToken,
            function,
        );
        self.parseCallArguments(&callExpression.arguments) catch {
            return null;
        };

        return ast.Expression{ .callExpression = callExpression };
    }

    fn parseCallArguments(self: *Self, arguments: *std.ArrayList(ast.Expression)) !void {
        // empty arguments case
        if (self.peekTokenIs(TokenType.RPAREN)) {
            self.nextToken();
            return;
        }

        // we have some arguments if the next token isn't RPAREN
        self.nextToken();
        // parse first expression
        if (self.parseExpression(Precedence.LOWEST)) |expr| {
            try arguments.append(expr);
        }

        // parse comma separated statements
        while (self.peekTokenIs(TokenType.COMMA)) {
            self.nextToken(); // move to comma
            self.nextToken(); // move to token after comma
            if (self.parseExpression(Precedence.LOWEST)) |expr| {
                try arguments.append(expr);
            }
        }

        if (!self.expectPeek(TokenType.RPAREN)) {
            for (arguments.items) |*arg| {
                arg.*.deinit();
            }
            arguments.deinit();
        }
    }

    fn parseBlock(self: *Self) ast.Block {
        var block = ast.Block.init(self.allocator, self.currentToken);
        self.nextToken();
        while (!self.currentTokenIs(TokenType.RBRACE) and !self.currentTokenIs(TokenType.EOF)) {
            if (self.parseStatement()) |stmt| {
                block.statements.append(stmt) catch unreachable;
            }
            self.nextToken();
        }
        return block;
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

test "prefix operators" {
    const allocator = std.testing.allocator;
    const PrefixTestData = struct {
        input: []const u8,
        operator: []const u8,
        integerValue: u64,
    };
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

fn InfixTestData(comptime T: type) type {
    return struct {
        const Self = @This();
        input: []const u8,
        leftValue: T,
        operator: []const u8,
        rightValue: T,

        pub fn new(input: []const u8, leftValue: T, operator: []const u8, rightValue: T) Self {
            return .{ .input = input, .leftValue = leftValue, .operator = operator, .rightValue = rightValue };
        }
    };
}

fn checkExpressionValue(expression: *ast.Expression, expected: anytype) !void {
    switch (expression.*) {
        .boolean => |expr| try std.testing.expectEqual(expected, expr.value),
        .integerLiteral => |expr| try std.testing.expectEqual(expected, expr.value),
        else => null,
    }
}

test "infix operators" {
    const intTests = [_]InfixTestData(u64){
        InfixTestData(u64).new("5 + 5;", 5, "+", 5),
        InfixTestData(u64).new("5 - 5;", 5, "-", 5),
        InfixTestData(u64).new("5 * 5;", 5, "*", 5),
        InfixTestData(u64).new("5 / 5;", 5, "/", 5),
        InfixTestData(u64).new("5 < 5;", 5, "<", 5),
        InfixTestData(u64).new("5 > 5;", 5, ">", 5),
        InfixTestData(u64).new("5 == 5;", 5, "==", 5),
        InfixTestData(u64).new("5 != 5;", 5, "!=", 5),
    };
    // TODO: implement these tests...but in a reusable way
    // const boolTests = [_]InfixTestData(bool){
    //     InfixTestData(bool).new("true == true", true, "==", true),
    //     InfixTestData(bool).new("true != false", true, "!=", false),
    //     InfixTestData(bool).new("false == false", false, "==", false),
    // };

    const allocator = std.testing.allocator;

    for (intTests) |t| {
        var l = lexer.Lexer.init(t.input);
        var parser = Parser.init(allocator, &l);
        var program = try parser.parseProgram();
        defer parser.deinit();
        defer program.deinit();

        try checkParserErrors(&parser);

        try std.testing.expectEqual(1, program.statements.items.len);

        const exp = program.statements.items[0].expressionStatement.expression.?.infixExpression;
        try std.testing.expectEqual(exp.left.?.integerLiteral.value, t.leftValue);
        try std.testing.expectEqual(t.operator, exp.operator);
        try std.testing.expectEqual(exp.right.?.integerLiteral.value, t.rightValue);
    }

    // for (boolTests) |t| {
    //     var l = lexer.Lexer.init(t.input);
    //     var parser = Parser.init(allocator, &l);
    //     var program = try parser.parseProgram();
    //     defer parser.deinit();
    //     defer program.deinit();
    //
    //     try checkParserErrors(&parser);
    //
    //     try std.testing.expectEqual(1, program.statements.items.len);
    //
    //     const exp = program.statements.items[0].expressionStatement.expression.?.infixExpression;
    //     try checkExpressionValue(exp.left.?, t.leftValue);
    //     try std.testing.expectEqual(t.operator, exp.operator);
    //     try checkExpressionValue(exp.right.?, t.rightValue);
    // }
}

const TestData = struct {
    input: []const u8,
    output: []const u8,

    pub fn new(input: []const u8, output: []const u8) TestData {
        return TestData{ .input = input, .output = output };
    }
};

fn testProgram(tests: []const TestData) !void {
    const allocator = std.testing.allocator;
    for (tests) |t| {
        var l = lexer.Lexer.init(t.input);
        var parser = Parser.init(allocator, &l);
        var program = try parser.parseProgram();
        defer parser.deinit();
        defer program.deinit();

        try checkParserErrors(&parser);

        var actual = std.ArrayList(u8).init(allocator);
        defer actual.deinit();
        try program.toString(actual.writer());
        try std.testing.expectEqualStrings(t.output, actual.items);
    }
}

test "operator precedence parsing" {
    const tests = [_]TestData{
        TestData.new("-a * b", "((-a) * b)"),
        TestData.new("!-a", "(!(-a))"),
        TestData.new("a + b +c", "((a + b) + c)"),
        TestData.new("a + b - c", "((a + b) - c)"),
        TestData.new("a * b / c", "((a * b) / c)"),
        TestData.new("a + b / c", "(a + (b / c))"),
        TestData.new("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        TestData.new("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        TestData.new("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        TestData.new("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        TestData.new("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
        TestData.new("true", "true"),
        TestData.new("false", "false"),
        TestData.new("3 > 5 == false", "((3 > 5) == false)"),
        TestData.new("3 < 5 == true", "((3 < 5) == true)"),
        TestData.new("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        TestData.new("(5 + 5) * 2", "((5 + 5) * 2)"),
        TestData.new("2 / (5 + 5)", "(2 / (5 + 5))"),
        TestData.new("-(5 + 5)", "(-(5 + 5))"),
        TestData.new("!(true == true)", "(!(true == true))"),
    };

    try testProgram(&tests);
}

test "boolean literals" {
    const tests = [_]TestData{
        TestData.new("let isTrue = true;", "let isTrue = true;"),
        TestData.new("let isFalse = false;", "let isFalse = false;"),
    };

    try testProgram(&tests);
}

fn getProgram(allocator: mem.Allocator, input: []const u8) !ast.Program {
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(allocator, &l);
    var program = try parser.parseProgram();
    defer parser.deinit();
    errdefer program.deinit();
    try checkParserErrors(&parser);
    return program;
}

test "if expression" {
    const input = "if (x < y) { x }";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();
    try std.testing.expectEqual(1, program.statements.items.len);

    const ifStatement = program.statements.items[0].expressionStatement.expression.?.ifExpression;
    const condition = ifStatement.condition.?;
    try std.testing.expectEqualStrings("x", condition.infixExpression.left.?.identifier.value);
    try std.testing.expectEqualStrings("<", condition.infixExpression.operator);
    try std.testing.expectEqualStrings("y", condition.infixExpression.right.?.identifier.value);

    const consequence = ifStatement.consequence.?;
    try std.testing.expectEqualStrings("x", consequence.statements.items[0].expressionStatement.expression.?.identifier.value);

    try std.testing.expect(null == ifStatement.alternative);
}

test "if else expression" {
    const input = "if (x < y) { x } else { y }";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();
    try std.testing.expectEqual(1, program.statements.items.len);

    const ifStatement = program.statements.items[0].expressionStatement.expression.?.ifExpression;
    const condition = ifStatement.condition.?;
    try std.testing.expectEqualStrings("x", condition.infixExpression.left.?.identifier.value);
    try std.testing.expectEqualStrings("<", condition.infixExpression.operator);
    try std.testing.expectEqualStrings("y", condition.infixExpression.right.?.identifier.value);

    const consequence = ifStatement.consequence.?;
    try std.testing.expectEqualStrings("x", consequence.statements.items[0].expressionStatement.expression.?.identifier.value);

    const alternative = ifStatement.alternative.?;
    try std.testing.expectEqualStrings("y", alternative.statements.items[0].expressionStatement.expression.?.identifier.value);
}

test "function literal" {
    const input = "fn(x,y) { x + y; }";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();

    try std.testing.expectEqual(1, program.statements.items.len);
    const funcLiteral = program.statements.items[0].expressionStatement.expression.?.functionLiteral;

    try std.testing.expectEqual(2, funcLiteral.parameters.items.len);
    try std.testing.expectEqualStrings("x", funcLiteral.parameters.items[0].value);
    try std.testing.expectEqualStrings("y", funcLiteral.parameters.items[1].value);

    try std.testing.expectEqual(1, funcLiteral.body.?.statements.items.len);
    const expr = funcLiteral.body.?.statements.items[0].expressionStatement.expression.?.infixExpression;

    try std.testing.expectEqualStrings("x", expr.left.?.identifier.value);
    try std.testing.expectEqualStrings("+", expr.operator);
    try std.testing.expectEqualStrings("y", expr.right.?.identifier.value);
}
test "single func param" {
    const input = "fn(x) { return x; }";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();
    const funcLiteral = program.statements.items[0].expressionStatement.expression.?.functionLiteral;

    try std.testing.expectEqual(1, funcLiteral.parameters.items.len);
    try std.testing.expectEqualStrings("x", funcLiteral.parameters.items[0].value);
}

test "empty function params" {
    const input = "fn() { 2 + 4; }";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();

    try std.testing.expectEqual(1, program.statements.items.len);
    const funcLiteral = program.statements.items[0].expressionStatement.expression.?.functionLiteral;

    try std.testing.expectEqual(0, funcLiteral.parameters.items.len);
    try std.testing.expectEqual(1, funcLiteral.body.?.statements.items.len);
    const expr = funcLiteral.body.?.statements.items[0].expressionStatement.expression.?.infixExpression;
    try std.testing.expectEqual(2, expr.left.?.integerLiteral.value);
    try std.testing.expectEqualStrings("+", expr.operator);
    try std.testing.expectEqual(4, expr.right.?.integerLiteral.value);
}

fn testExpressionLiteral(expected: []const u8, expr: ast.Expression) !void {
    var output = std.ArrayList(u8).init(std.testing.allocator);
    defer output.deinit();
    try expr.toString(output.writer());
    try std.testing.expectEqualStrings(expected, output.items);
}

test "call expressions" {
    const input = "add(1, 2 * 3, 4 + 5);";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();

    try std.testing.expectEqual(1, program.statements.items.len);

    const callExpr = program.statements.items[0].expressionStatement.expression.?.callExpression;
    try std.testing.expectEqualStrings("add", callExpr.function.identifier.value);
    const arguments = callExpr.arguments.items;
    try testExpressionLiteral("1", arguments[0]);
    try testExpressionLiteral("(2 * 3)", arguments[1]);
    try testExpressionLiteral("(4 + 5)", arguments[2]);
}

test "nested call expressions" {
    const input = "add(1, add(2 * 3), 4 + 5);";
    const allocator = std.testing.allocator;
    var program = try getProgram(allocator, input);
    defer program.deinit();

    try std.testing.expectEqual(1, program.statements.items.len);

    const callExpr = program.statements.items[0].expressionStatement.expression.?.callExpression;
    try std.testing.expectEqualStrings("add", callExpr.function.identifier.value);
    const arguments = callExpr.arguments.items;
    try testExpressionLiteral("1", arguments[0]);
    try testExpressionLiteral("add((2 * 3))", arguments[1]);
    try testExpressionLiteral("(4 + 5)", arguments[2]);
}
