const std = @import("std");

const ValueKind = enum {
    bool,
    nil,
    number,
};

pub const Value = union(ValueKind) {
    bool: bool,
    nil: f64,
    number: f64,

    pub fn boolVal(value: bool) Value {
        return .{ .bool = value };
    }
    pub fn nilVal() Value {
        return .{ .nil = 0 };
    }
    pub fn numberVal(value: f64) Value {
        return .{ .number = value };
    }

    pub fn is_bool(self: Value) bool {
        return self == Value.bool;
    }
    pub fn is_nil(self: Value) bool {
        return self == Value.nil;
    }
    pub fn is_number(self: Value) bool {
        return self == Value.number;
    }
};

pub const Array = std.ArrayList(Value);

pub fn print(value: Value) void {
    switch (value) {
        .bool => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        .nil => std.debug.print("nil", .{}),
        .number => |n| std.debug.print("{d}", .{n}),
    }
}
