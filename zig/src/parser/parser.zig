const std = @import("std");
const ast = @import("./ast.zig");
const lexer = @import("../lexer/lexer.zig");

const Parser = struct {
    const Self = @This();
    lexer: *lexer.Lexer,
    currentToken: ?lexer.Token,
    peekToken: ?lexer.Token,

    pub fn init(l: *lexer.Lexer) *Self {
        var parser = Self{
            .lexer = l,
            .currentToken = null,
            .peekToken = null,
        };
        // do this twice so we have both currentToken
        // and peekToken set up
        parser.nextToken();
        parser.nextToken();
        return &parser;
    }

    pub fn parseProgram(self: *Self) ?*ast.Program {
        // TODO: could be refactored to use init/deinit?
        var program = ast.Program{ .statements = std.ArrayList(ast.Statement).init() };
        while (self.currentToken != lexer.Token.EOF) {
            const statement = self.parseStatement();
            if (statement != null) {
                program.statements.append(statement);
            }
            self.nextToken();
        }
        return program;
    }

    fn parseStatement(self: *Self) *ast.Statement {
        return switch (self.ch) {
            .lexer.token.LET => {
                return self.parseLetStatement();
            },
            else => return null,
        };
    }

    fn parseLetStatement(self: *Self) ast.LetStatement {
        const stmt = ast.LetStatement{}
    }

    fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }
};

test "Test let statements" {
    const input =
        \\let x = 5;
        \\let y = 10;
        \\let foobar = 838383;
    ;
    var l = lexer.Lexer.init(input);
    var parser = Parser.init(&l);
    const program = parser.parseProgram();
    try std.testing.expect(program != null);
    // try std.testing.expect(program.statements.items.len == 3);
}
