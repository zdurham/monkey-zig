const std = @import("std");
const lexer = @import("../lexer/lexer.zig");
const parser = @import("../parser/parser.zig");

const TokenType = lexer.TokenType;
const PROMPT = ">> ";

pub fn start(allocator: std.mem.Allocator) !void {
    const stdin = std.io.getStdIn().reader();
    std.debug.print(PROMPT, .{});

    var buffer: [1028]u8 = undefined;
    while (try stdin.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var l = lexer.Lexer.init(line);
        var p = parser.Parser.init(allocator, &l);
        var program = try p.parseProgram();
        // TODO: nicer errors for repl
        if (p.errors.items.len > 0) {
            const errors = p.getErrors();
            for (errors) |err| {
                std.debug.print("Parser error: {s}\n", .{err});
            }
        }

        var output = std.ArrayList(u8).init(allocator);
        defer output.deinit();
        try program.toString(output.writer());
        std.debug.print("{s}\n", .{output.items});
        std.debug.print(PROMPT, .{});
    }
}
