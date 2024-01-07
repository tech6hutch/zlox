const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

pub const OpCode = enum {
    op_return,
};

pub const Chunk = struct {
    code: ArrayList(u8),

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
        };
    }
    pub fn write_byte(self: *Chunk, byte: u8) Allocator.Error!void {
        try self.code.append(byte);
    }
    pub fn write_op_code(self: *Chunk, op_code: OpCode) Allocator.Error!void {
        try self.write_byte(@intFromEnum(op_code));
    }
    pub fn free(self: *Chunk) void {
        self.code.deinit();
    }

    pub inline fn idx(self: *Chunk, i: usize) u8 {
        return self.code.items[i];
    }
    pub inline fn count(self: *Chunk) usize {
        return self.code.items.len;
    }
};
