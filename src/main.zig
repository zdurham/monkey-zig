const std = @import("std");
const repl = @import("repl/repl.zig");

pub fn main() !void {
    std.debug.print("Welcome to the Monkey Programming language!\n", .{});
    std.debug.print("Type CTRL+C to exit: \n", .{});
    try repl.start();
}
