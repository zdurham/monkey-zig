const std = @import("std");

const Token = union(enum) { ILLEGAL, EOF, IDENT: []const u8, INT: []const u8, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET };

fn isLetter(ch: u8) bool {
    return std.ascii.isAlphabetic(ch) or ch == '_';
}

fn isDigit(ch: u8) bool {
    return std.ascii.isDigit(ch);
}

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
        self.skipWhitespace();
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
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.readIdentifier();
                if (self.lookupKeyword(ident)) |token| {
                    return token;
                }
                return .{ .IDENT = ident };
            },
            '0'...'9' => {
                const int = self.readDigit();
                return .{ .INT = int };
            },
            else => return .ILLEGAL,
        };

        self.readChar();
        return token;
    }

    fn lookupKeyword(self: *Self, ident: []const u8) ?Token {
        _ = self;
        const map = std.ComptimeStringMap(Token, .{
            .{ "let", .LET },
            .{ "fn", .FUNCTION },
        });
        return map.get(ident);
    }

    fn readIdentifier(self: *Self) []const u8 {
        const initialPosition = self.position;
        while (isLetter(self.ch)) {
            self.readChar();
        }
        return self.input[initialPosition..self.position];
    }

    fn readDigit(self: *Self) []const u8 {
        const initialPosition = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        return self.input[initialPosition..self.position];
    }

    fn skipWhitespace(self: *Self) void {
        while (self.ch == '\r' or self.ch == ' ' or self.ch == '\n' or self.ch == '\t') {
            self.readChar();
        }
    }
};

// TODO: turn multi line strings INTo files
// that we can test via reading monkey.ml files
test "Test next_token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;
    const expectedTokens = [_]Token{ .LET, .{ .IDENT = "five" }, .ASSIGN, .{ .INT = "5" }, .SEMICOLON, .LET, .{ .IDENT = "ten" }, .ASSIGN, .{ .INT = "10" }, .SEMICOLON, .LET, .{ .IDENT = "add" }, .ASSIGN, .FUNCTION, .LPAREN, .{ .IDENT = "x" }, .COMMA, .{ .IDENT = "y" }, .RPAREN, .LBRACE, .{ .IDENT = "x" }, .PLUS, .{ .IDENT = "y" }, .SEMICOLON, .RBRACE, .SEMICOLON, .LET, .{ .IDENT = "result" }, .ASSIGN, .{ .IDENT = "add" }, .LPAREN, .{ .IDENT = "five" }, .COMMA, .{ .IDENT = "ten" }, .RPAREN, .SEMICOLON, .EOF };

    std.log.warn("\n{s}\n", .{input});
    var lexer = Lexer.init(input);
    for (expectedTokens) |token| {
        const tokenFromLexer = lexer.nextToken();
        try std.testing.expectEqualDeep(token, tokenFromLexer);
    }
}
