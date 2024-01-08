const std = @import("std");
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Value = @import("./value.zig").Value;
const ValueArray = @import("./value.zig").ValueArray;

const PACK_LINE_NUMBERS = false;
const PackedLineNum = if (PACK_LINE_NUMBERS) struct {
    line_num: u16,
    repeated_times: u16 = 0,
};

pub const OpCode = enum {
    op_constant,
    op_return,
};

pub const Chunk = struct {
    code: ArrayList(u8),
    lines: ArrayList(if (PACK_LINE_NUMBERS) PackedLineNum else usize),
    constants: ValueArray,

    pub fn init(allocator: Allocator) Chunk {
        return Chunk{
            .code = ArrayList(u8).init(allocator),
            .lines = ArrayList(if (PACK_LINE_NUMBERS) PackedLineNum else usize).init(allocator),
            .constants = ValueArray.init(allocator),
        };
    }
    pub fn write_byte(self: *Chunk, byte: u8, line: usize) Allocator.Error!void {
        try self.code.append(byte);
        if (PACK_LINE_NUMBERS) {
            var last =
                if (self.lines.items.len > 0) &self.lines.items[self.lines.items.len - 1]
                else null;
            if (last != null and last.?.line_num == line) {
                last.?.repeated_times +%= 1;
                if (last.?.repeated_times == 0) std.debug.panic(
                    "can't compile more than {d} instructions per line",
                    .{U16_MAX});
            } else {
                const next = PackedLineNum {
                    .line_num = u16_cast(line) catch std.debug.panic(
                        "can't handle more than {d} lines",
                        .{U16_MAX}),
                };
                try self.lines.append(next);
            }
        } else {
            try self.lines.append(line);
        }
    }
    pub fn write_op_code(self: *Chunk, op_code: OpCode, line: usize) Allocator.Error!void {
        try self.write_byte(@intFromEnum(op_code), line);
    }
    pub fn free(self: *Chunk) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
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

const U16_MAX = 65535;
fn u16_cast(n: usize) error{TooDamnHigh}!u16 {
    return if (n > U16_MAX) error.TooDamnHigh else @intCast(n);
}
