const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const ValueArray = @import("./value.zig").ValueArray;

pub const OpCode = enum {
    op_constant,
    op_return,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(usize),
    constants: ValueArray,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(usize).init(allocator),
            .constants = ValueArray.init(allocator),
        };
    }
    pub fn write_byte(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
        try self.code.append(byte);
        try self.lines.append(line);
    }
    pub fn write_op_code(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!void {
        try self.write_byte(@intFromEnum(op_code), line);
    }
    pub fn free(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub inline fn idx(self: *Chunk, i: usize) u8 {
        return self.code.items[i];
    }
    pub inline fn count(self: *Chunk) usize {
        return self.code.items.len;
    }
    pub inline fn constIdx(self: *Chunk, i: usize) Value {
        return self.constants.items[i];
    }
    pub inline fn addConst(self: *Chunk, value: Value) Allocator.Error!u8 {
        try self.constants.append(value);
        if (self.constants.items.len > 256) {
            @panic("over 256 constants is unsupported");
        }
        return @intCast(self.constants.items.len - 1);
    }
};
