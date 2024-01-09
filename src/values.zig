const std = @import("std");

pub const Value = f64;

pub const Array = std.ArrayList(Value);

pub fn print(value: Value) void {
    std.debug.print("{d}", .{value});
}
