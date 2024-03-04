const std = @import("std");

const Token = union(enum) { ILLEGAL, EOF, IDENT, INT, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET };

pub const Lexer = struct {
    const Self = @This();

    input: []const u8,
    position: usize = 0,
    readPosition: usize = 0,
    ch: u8 = 0,

    pub fn init(input: []const u8) Self {
        var lexer = Self{ .input = input };

        lexer.readChar();

        return lexer;
    }

    pub fn readChar(self: *Self) void {
        if (self.readPosition >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.readPosition];
        }
        self.position = self.readPosition;
        self.readPosition += 1;
    }

    pub fn nextToken(self: *Self) Token {
        const token: Token = switch (self.ch) {
            '=' => .ASSIGN,
            ';' => .SEMICOLON,
            '(' => .LPAREN,
            ')' => .RPAREN,
            ',' => .COMMA,
            '+' => .PLUS,
            '{' => .LBRACE,
            '}' => .RBRACE,
            0 => .EOF,
            else => .ILLEGAL,
        };

        self.readChar();
        return token;
    }
};

test "Test next_token" {
    const input = "=+(){},;";
    const expectedTokens = [_]Token{ .ASSIGN, .PLUS, .LPAREN, .RPAREN, .LBRACE, .RBRACE, .COMMA, .SEMICOLON, .EOF };

    var lexer = Lexer.init(input);
    for (expectedTokens) |token| {
        const tokenFromLexer = lexer.nextToken();
        try std.testing.expectEqualDeep(tokenFromLexer, token);
    }
}
