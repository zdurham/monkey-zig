const std = @import("std");

test {
    _ = @import("lexer/lexer.zig");
}

pub fn main() !void {
    std.debug.print("Start of the program", .{});
}
