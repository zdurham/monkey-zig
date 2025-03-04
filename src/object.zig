const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ObjectType = enum {
    INTEGER,
    BOOLEAN,
    NULL,
};

pub const Object = union(enum) {
    integer: Integer,

    pub fn getType(self: Object) void {
        switch (self.*) {
            inline else => |obj| obj.getType(),
        }
    }

    pub fn inspect(self: Object) void {
        switch (self.*) {
            inline else => |obj| obj.inspect(),
        }
    }
};

pub const Integer = struct {
    value: usize,

    pub fn inspect(self: Integer, allocator: Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.value});
    }

    pub fn getType() ObjectType {
        return ObjectType.INTEGER;
    }
};

pub const Boolean = struct {
    value: bool,

    pub fn inspect(self: Boolean, allocator: Allocator) []const u8 {
        return std.fmt.allocPrint(allocator, "{s}", .{self.value});
    }

    pub fn getType() ObjectType {
        return ObjectType.BOOLEAN;
    }
};

pub const Null = struct {
    pub fn inspect() []const u8 {
        return "null"
    }

    pub fn getType() ObjectType {
        return ObjectType.NULL
    }
}
