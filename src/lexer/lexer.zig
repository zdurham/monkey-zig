const std = @import("std");

pub const TokenType = enum {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    GT,
    LT,
    TRUE,
    FALSE,
    ELSE,
    IF,
    RETURN,
    EQ,
    NOT_EQ,
};

pub const Token = struct {
    const Self = @This();
    kind: TokenType,
    literal: []const u8,

    pub fn new(kind: TokenType, literal: []const u8) Self {
        return Self{
            .kind = kind,
            .literal = literal,
        };
    }
};

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
                    break :blk Token.new(.EQ, "==");
                } else {
                    break :blk Token.new(.ASSIGN, "=");
                }
            },
            ';' => Token.new(.SEMICOLON, ";"),
            '(' => Token.new(.LPAREN, "("),
            ')' => Token.new(.RPAREN, ")"),
            ',' => Token.new(.COMMA, ","),
            '+' => Token.new(.PLUS, "+"),
            '{' => Token.new(.LBRACE, "{"),
            '}' => Token.new(.RBRACE, "}"),
            '/' => Token.new(.SLASH, "/"),
            '!' => blk: {
                if (self.peekChar() == '=') {
                    self.readChar();
                    break :blk Token.new(.NOT_EQ, "!=");
                } else {
                    break :blk Token.new(.BANG, "!");
                }
            },
            '-' => Token.new(.MINUS, "-"),
            '>' => Token.new(.GT, ">"),
            '<' => Token.new(.LT, "<"),
            '*' => Token.new(.ASTERISK, "*"),
            'a'...'z', 'A'...'Z', '_' => {
                const ident = self.readIdentifier();
                if (self.lookupKeyword(ident)) |token| {
                    return token;
                }
                return Token.new(.IDENT, ident);
            },
            '0'...'9' => {
                const int = self.readDigit();
                return Token.new(.INT, int);
            },
            0 => Token.new(.EOF, ""),
            else => return Token.new(.ILLEGAL, ""),
        };

        self.readChar();
        return token;
    }

    fn lookupKeyword(self: *Self, ident: []const u8) ?Token {
        _ = self;
        const map = std.StaticStringMap(Token).initComptime(.{
            .{ "let", Token.new(.LET, "let") },
            .{ "fn", Token.new(.FUNCTION, "fn") },
            .{ "return", Token.new(.RETURN, "return") },
            .{ "if", Token.new(.IF, "if") },
            .{ "else", Token.new(.ELSE, "else") },
            .{ "true", Token.new(.TRUE, "true") },
            .{ "false", Token.new(.FALSE, "false") },
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
    const expectedTokens = [_]Token{
        Token.new(.LET, "let"),
        Token.new(.IDENT, "five"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "ten"),
        Token.new(.ASSIGN, "="),
        Token.new(.INT, "10"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "add"),
        Token.new(.ASSIGN, "="),
        Token.new(.FUNCTION, "fn"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "x"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "y"),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.IDENT, "x"),
        Token.new(.PLUS, "+"),
        Token.new(.IDENT, "y"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.LET, "let"),
        Token.new(.IDENT, "result"),
        Token.new(.ASSIGN, "="),
        Token.new(.IDENT, "add"),
        Token.new(.LPAREN, "("),
        Token.new(.IDENT, "five"),
        Token.new(.COMMA, ","),
        Token.new(.IDENT, "ten"),
        Token.new(.RPAREN, ")"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.BANG, "!"),
        Token.new(.MINUS, "-"),
        Token.new(.SLASH, "/"),
        Token.new(.ASTERISK, "*"),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.INT, "5"),
        Token.new(.LT, "<"),
        Token.new(.INT, "10"),
        Token.new(.GT, ">"),
        Token.new(.INT, "5"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.IF, "if"),
        Token.new(.LPAREN, "("),
        Token.new(.INT, "5"),
        Token.new(.LT, "<"),
        Token.new(.INT, "10"),
        Token.new(.RPAREN, ")"),
        Token.new(.LBRACE, "{"),
        Token.new(.RETURN, "return"),
        Token.new(.TRUE, "true"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.ELSE, "else"),
        Token.new(.LBRACE, "{"),
        Token.new(.RETURN, "return"),
        Token.new(.FALSE, "false"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.RBRACE, "}"),
        Token.new(.INT, "10"),
        Token.new(.EQ, "=="),
        Token.new(.INT, "10"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.INT, "10"),
        Token.new(.NOT_EQ, "!="),
        Token.new(.INT, "9"),
        Token.new(.SEMICOLON, ";"),
        Token.new(.EOF, ""),
    };

    var lexer = Lexer.init(input);
    for (expectedTokens) |token| {
        const tokenFromLexer = lexer.nextToken();

        try std.testing.expectEqualDeep(token, tokenFromLexer);
    }
}
