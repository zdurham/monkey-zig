const std = @import("std");
const ast = @import("./ast.zig");
const lexer = @import("../lexer/lexer.zig");

const Parser = struct {
    const Self = @This();
    lexer: *lexer.Lexer,
    currentToken: ?lexer.Token,
    peekToken: ?lexer.Token,

    pub fn nextToken(self: *Self) void {
        self.currentToken = self.peekToken;
        self.peekToken = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Self) ?*ast.Program {
        _ = self;
        return null;
    }

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
