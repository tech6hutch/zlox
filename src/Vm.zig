//! Don't move a VM after init'ing it, or its pointer fields will break.

const std = @import("std");
const Allocator = std.mem.Allocator;
const common = @import("./common.zig");
const dbg = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const values = @import("./values.zig");
const Value = values.Value;
const printValue = values.print;
const compiler = @import("./compiler.zig");

const STACK_MAX: usize = 256;

const Self = @This();

chunk: ?*Chunk,
ip: ?[*]u8,
stack: [STACK_MAX]Value,
stack_top: ?[*]Value,
allocator: Allocator,

pub fn init(self: *Self, allocator: Allocator) void {
    self.chunk = null;
    self.ip = null;
    self.stack_top = null;
    self.resetStack();
	self.allocator = allocator;
}
fn resetStack(self: *Self) void {
    self.stack_top = self.stack[0..];
}
pub fn deinit(self: *Self) void {
    _ = self;
}

pub fn interpret(self: *Self, source: [*:0]const u8) InterpretError!void {
	var chunk = Chunk.init(self.allocator);

	if (!compiler.compile(source, chunk)) {
		chunk.deinit();
		return .CompileError;
	}

	self.chunk = chunk;
	vm.ip = chunk.code.items[0];

	const result = self.run();

	chunk.deinit();
	return result;
}

fn run(self: *Self) InterpretError!void {
    const Op = Chunk.OpCode;
    while (true) {
        if (common.DEBUG_TRACE_EXECUTION) {
            std.debug.print("          ", .{});
            var slot: [*]Value = &self.stack;
            while (@intFromPtr(slot) < @intFromPtr(self.stack_top)) : (slot += 1) {
                std.debug.print("[ ", .{});
                printValue(slot[0]);
                std.debug.print(" ]", .{});
            }
            std.debug.print("\n", .{});
            _ = dbg.disassembleInstruction(
                self.chunk.?,
                @intFromPtr(self.ip.?) - @intFromPtr(self.chunk.?.code.items.ptr));
        }

        const instruction = self.readByte();
        switch (instruction) {
            Op.constant.int(), Op.constant_long.int() => {
                const constant: Value = self.readConst();
                self.push(constant);
            },
            Op.add.int() => self.binaryOp(.add),
            Op.subtract.int() => self.binaryOp(.subtract),
            Op.multiply.int() => self.binaryOp(.multiply),
            Op.divide.int() => self.binaryOp(.divide),
            Op.negate.int() => self.peek().* *= -1,
            Op.@"return".int() => {
                printValue(self.pop());
                std.debug.print("\n", .{});
                return;
            },
            else => {
                std.debug.panic("unknown opcode {d}", .{instruction});
            },
        }
    }
}
inline fn readByte(self: *Self) u8 {
    const byte = self.ip.?[0];
    self.ip.? += 1;
    return byte;
}
inline fn readConst(self: *Self) Value {
    return self.chunk.?.constIdx(self.readByte());
}
const BinaryOp = enum {
    add,
    subtract,
    multiply,
    divide,
};
inline fn binaryOp(self: *Self, comptime op: BinaryOp) void {
    const b = self.pop();
    switch (op) {
        .add      => self.peek().* += b,
        .subtract => self.peek().* -= b,
        .multiply => self.peek().* *= b,
        .divide   => self.peek().* /= b,
    }
}

fn push(self: *Self, value: Value) void {
    self.stack_top.?[0] = value;
    self.stack_top.? += 1;
}
fn pop(self: *Self) Value {
    self.stack_top.? -= 1;
    return self.stack_top.?[0];
}
inline fn peek(self: *Self) *Value {
    return &(self.stack_top.? - 1)[0];
}

pub const InterpretError = error{ CompileError, RuntimeError };
