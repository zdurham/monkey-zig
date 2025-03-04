const std = @import("std");
const repl = @import("repl/repl.zig");
const lexer = @import("lexer/lexer.zig");

pub fn main() !void {
    std.debug.print("Welcome to the Monkey Programming language!\n", .{});
    std.debug.print("Type CTRL+C to exit: \n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    try repl.start(allocator);
}
