const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const values = @import("./values.zig");
const Value = values.Value;
const ValueArray = values.Array;

const PACK_LINE_NUMBERS = false;
const PackedLineNum = if (PACK_LINE_NUMBERS) struct {
    line_num: u16,
    repeated_times: u16 = 0,
};

const Chunk = @This();

code: ArrayList(u8),
lines: ArrayList(if (PACK_LINE_NUMBERS) PackedLineNum else usize),
constants: ValueArray,

pub fn init(allocator: Allocator) Chunk {
    return Chunk{
        .code = ArrayList(u8).init(allocator),
        .lines = ArrayList(
            if (PACK_LINE_NUMBERS) PackedLineNum else usize
        ).init(allocator),
        .constants = ValueArray.init(allocator),
    };
}
pub fn deinit(self: *Chunk) void {
    self.code.deinit();
    self.lines.deinit();
    self.constants.deinit();
}

pub fn write_byte(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
    try self.code.append(byte);
    try self.append_line(line, 1);
}
pub fn write_bytes(self: *Chunk, bytes: []const u8, line: usize) Allocator.Error!void {
    try self.code.appendSlice(bytes);
    try append_line(self, line, bytes.len);
}
fn append_line(self: *Chunk, line: usize, times: usize) Allocator.Error!void {
    if (PACK_LINE_NUMBERS) {
        var line_info: *PackedLineNum = blk: {
            if (self.lines.items.len > 0) {
                const prev_line_info = &self.lines.items[self.lines.items.len - 1];
                if (prev_line_info.line_num == line) {
                    break :blk prev_line_info;
                }
            }
            const new_line_info = PackedLineNum{
                .line_num = u16_cast(line) catch std.debug.panic(
                    "can't handle more than {d} lines",
                    .{U16_MAX}),
            };
            try self.lines.append(new_line_info);
            break :blk &self.lines.items[self.lines.items.len - 1];
        };
        line_info.repeated_times = u16_cast(line_info.repeated_times + times) catch std.debug.panic(
            "can't compile more than {d} instructions per line",
            .{U16_MAX});
    } else {
        try self.lines.appendNTimes(line, times);
    }
}
pub fn write_op_code(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!void {
    try self.write_byte(@intFromEnum(op_code), line);
}
pub fn write_const(self: *Chunk, value: Value, line: usize) Allocator.Error!void {
    const constant: u24 = try self.addConst(value);
    if (constant < 256) {
        try self.write_op_code(Op.constant, line);
        try self.write_byte(@intCast(constant), line);
    } else {
        const constant_bytes: [3]u8 = @bitCast(constant);
        try self.write_op_code(Op.constant_long, line);
        try self.write_bytes(&constant_bytes, line);
    }
}

pub fn get_line(self: *Chunk, code_idx: usize) usize {
    if (PACK_LINE_NUMBERS) {
        var i: usize = 0;
        for (self.lines.items) |line| {
            i += line.repeated_times;
            i += 1;
            if (i > code_idx) return line.line_num;
        }
        std.debug.panic(
            "cannot retrieve line number for instruction at index {d} because it does not exist",
            .{code_idx});
    } else {
        return self.lines.items[code_idx];
    }
}

pub inline fn idx(self: *Chunk, i: usize) u8 {
    return self.code.items[i];
}
pub inline fn idx_array(self: *Chunk, start: usize, comptime N: usize) []const u8 {
    return self.code.items[start..start+N];
}
pub inline fn count(self: *Chunk) usize {
    return self.code.items.len;
}
pub inline fn constIdx(self: *Chunk, i: usize) Value {
    return self.constants.items[i];
}
pub inline fn addConst(self: *Chunk, value: Value) Allocator.Error!u24 {
    try self.constants.append(value);
    const i = self.constants.items.len - 1;
    const i_u24: u24 = @intCast(i);
    if (i_u24 != i) {
        std.debug.panic("use fewer constants (less than 2^24)", .{});
    }
    return i_u24;
}

const U16_MAX = 65535;
fn u16_cast(n: usize) error{TooDamnHigh}!u16 {
    return if (n > U16_MAX) error.TooDamnHigh else @intCast(n);
}

pub const OpCode = enum(u8) {
    constant,
    constant_long,
    add,
    subtract,
    multiply,
    divide,
    negate,
    @"return",
    pub fn int(comptime self: OpCode) u8 {
        return @intFromEnum(self);
    }
};
const Op = OpCode;