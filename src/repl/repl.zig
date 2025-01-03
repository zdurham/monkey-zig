const std = @import("std");
const lexer = @import("../lexer/lexer.zig");

const TokenType = lexer.TokenType;
const PROMPT = ">> ";

pub fn start() !void {
    const stdin = std.io.getStdIn().reader();
    std.debug.print(PROMPT, .{});

    var buffer: [1028]u8 = undefined;
    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var l = lexer.Lexer.init(line);
        var token = l.nextToken();
        while (token.kind != TokenType.EOF and token.kind != TokenType.ILLEGAL) {
            std.debug.print("{}\n", .{token});
            token = l.nextToken();
        }
        std.debug.print(PROMPT, .{});
    }
}
