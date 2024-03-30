const std = @import("std");

const Token = union(enum) { ILLEGAL, EOF, IDENT: []const u8, INT: []const u8, ASSIGN, PLUS, COMMA, SEMICOLON, LPAREN, RPAREN, LBRACE, RBRACE, FUNCTION, LET, MINUS, BANG, ASTERISK, SLASH, GT, LT, TRUE, FALSE, ELSE, IF, RETURN, EQ, NOT_EQ };

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
            '=' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk .EQ;
                } else {
                    break :blk .ASSIGN;
                }
            },
            ';' => .SEMICOLON,
            '(' => .LPAREN,
            ')' => .RPAREN,
            ',' => .COMMA,
            '+' => .PLUS,
            '{' => .LBRACE,
            '}' => .RBRACE,
            '/' => .SLASH,
            '!' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk .NOT_EQ;
                } else {
                    break :blk .BANG;
                }
            },
            '-' => .MINUS,
            '>' => .GT,
            '<' => .LT,
            '*' => .ASTERISK,
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
            0 => .EOF,
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
            .{ "return", .RETURN },
            .{ "if", .IF },
            .{ "else", .ELSE },
            .{ "true", .TRUE },
            .{ "false", .FALSE },
        });
        return map.get(ident);
    }

    fn peekChar(self: *Self) u8 {
        if (self.readPosition >= self.input.len) {
            return 0;
        } else {
            return self.input[self.readPosition];
        }
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

test "Test all syntax" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\
        \\if (5 < 10) {
        \\   return true;
        \\} else {
        \\    return false;
        \\}
        \\10 == 10;
        \\10 != 9;
    ;
    const expectedTokens = [_]Token{ .LET, .{ .IDENT = "five" }, .ASSIGN, .{ .INT = "5" }, .SEMICOLON, .LET, .{ .IDENT = "ten" }, .ASSIGN, .{ .INT = "10" }, .SEMICOLON, .LET, .{ .IDENT = "add" }, .ASSIGN, .FUNCTION, .LPAREN, .{ .IDENT = "x" }, .COMMA, .{ .IDENT = "y" }, .RPAREN, .LBRACE, .{ .IDENT = "x" }, .PLUS, .{ .IDENT = "y" }, .SEMICOLON, .RBRACE, .SEMICOLON, .LET, .{ .IDENT = "result" }, .ASSIGN, .{ .IDENT = "add" }, .LPAREN, .{ .IDENT = "five" }, .COMMA, .{ .IDENT = "ten" }, .RPAREN, .SEMICOLON, .BANG, .MINUS, .SLASH, .ASTERISK, .{ .INT = "5" }, .SEMICOLON, .{ .INT = "5" }, .LT, .{ .INT = "10" }, .GT, .{ .INT = "5" }, .SEMICOLON, .IF, .LPAREN, .{ .INT = "5" }, .LT, .{ .INT = "10" }, .RPAREN, .LBRACE, .RETURN, .TRUE, .SEMICOLON, .RBRACE, .ELSE, .LBRACE, .RETURN, .FALSE, .SEMICOLON, .RBRACE, .{ .INT = "10" }, .EQ, .{ .INT = "10" }, .SEMICOLON, .{ .INT = "10" }, .NOT_EQ, .{ .INT = "9" }, .SEMICOLON, .EOF };

    var lexer = Lexer.init(input);
    for (expectedTokens) |token| {
        const tokenFromLexer = lexer.nextToken();
        try std.testing.expectEqualDeep(token, tokenFromLexer);
    }
}
