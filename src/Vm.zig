//! Don't move a VM after init'ing it, or its pointer fields will break.

const std = @import("std");
const Allocator = std.mem.Allocator;
const common = @import("./common.zig");
const dbg = @import("./debug.zig");
const Chunk = @import("./Chunk.zig");
const values = @import("./values.zig");
const Value = values.Value;
const printValue = values.print;
const valuesEqual = values.equal;
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
pub fn deinit(self: *Self) void {
    _ = self;
}
fn resetStack(self: *Self) void {
    self.stack_top = self.stack[0..];
}
fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
    var stderr = std.io.getStdErr().writer();
    stderr.print(format, args) catch {};
    stderr.writeByte('\n') catch {};
    const instruction = self.codeIndex() - 1;
    const line = self.chunk.?.getLine(instruction);
    stderr.print("[line {d}] in script\n", .{line}) catch {};
    self.resetStack();
}

pub fn interpret(self: *Self, source: [*:0]const u8) InterpretError!void {
    var chunk = Chunk.init(self.allocator);

    if (!compiler.compile(source, &chunk)) {
        chunk.deinit();
        return error.CompileError;
    }

    self.chunk = &chunk;
    self.ip = chunk.code.items.ptr;

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
            _ = dbg.disassembleInstruction(self.chunk.?, self.codeIndex());
        }

        const instruction = self.readByte();
        switch (instruction) {
            Op.constant.int(), Op.constant_long.int() => {
                const constant: Value = self.readConst();
                self.push(constant);
            },
            Op.nil.int() => self.push(Value.nilVal()),
            Op.true.int() => self.push(Value.boolVal(true)),
            Op.false.int() => self.push(Value.boolVal(false)),
            Op.equal.int() => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolVal(valuesEqual(a, b)));
            },
            Op.greater.int() =>  try self.binaryOp(bool, Value.boolVal, .greater),
            Op.less.int() =>     try self.binaryOp(bool, Value.boolVal, .less),
            Op.add.int() =>      try self.binaryOp(f64, Value.numberVal, .add),
            Op.subtract.int() => try self.binaryOp(f64, Value.numberVal, .subtract),
            Op.multiply.int() => try self.binaryOp(f64, Value.numberVal, .multiply),
            Op.divide.int() =>   try self.binaryOp(f64, Value.numberVal, .divide),
            Op.not.int() => self.push(Value.boolVal(isFalsey(self.pop()))),
            Op.negate.int() => {
                switch (self.peek(0).*) {
                    .number => |*n| n.* *= -1,
                    else => {
                        self.runtimeError("Operand must be a number.", .{});
                        return InterpretError.RuntimeError;
                    }
                }
            },
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
    greater,
    less,
};
inline fn binaryOp(
    self: *Self,
    comptime T: type,
    valueKind: *const fn(T) Value,
    comptime op: BinaryOp,
) InterpretError!void
{
    if (!self.peek(0).is_number() or !self.peek(1).is_number()) {
        self.runtimeError("Operands must be numbers.", .{});
        return InterpretError.RuntimeError;
    }
    const b = self.pop().number;
    switch (op) {
        .add      => self.peek(0).*.number += b,
        .subtract => self.peek(0).*.number -= b,
        .multiply => self.peek(0).*.number *= b,
        .divide   => self.peek(0).*.number /= b,
        .greater => {
            const a = self.pop().number;
            self.push(valueKind(a > b));
        },
        .less => {
            const a = self.pop().number;
            self.push(valueKind(a < b));
        },
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
inline fn peek(self: *Self, distance: isize) *Value {
    // Zig doesn't allow negative indices. Sad.
    return &(self.stack_top.? - 1 - distance)[0];
}
fn isFalsey(value: Value) bool {
    return value.is_nil() or (value.is_bool() and !value.bool);
}

fn codeIndex(self: *Self) usize {
    return @intFromPtr(self.ip.?) - @intFromPtr(self.chunk.?.code.items.ptr);
}

pub const InterpretError = error{ CompileError, RuntimeError };
