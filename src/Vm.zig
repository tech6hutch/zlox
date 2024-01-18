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
const loxmem = @import("./memory.zig");
const objects = @import("./objects.zig");
const Obj = objects.Obj;
const Table = @import("./Table.zig");

const STACK_MAX: usize = 256;

const Self = @This();
pub var vm: Self = undefined;

chunk: ?*Chunk,
ip: ?[*]u8,
stack: [STACK_MAX]Value,
stack_top: ?[*]Value,
globals: Table,
strings: Table,
objs: ?*Obj,

pub fn init(self: *Self) void {
    self.chunk = null;
    self.ip = null;
    self.stack_top = null;
    self.resetStack();
    self.globals = Table.init();
    self.strings = Table.init();
    self.objs = null;
}
pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.strings.deinit();
    loxmem.freeObjects(self);
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
    var chunk = Chunk.init(loxmem.allocator);

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
            Op.pop.int() => _ = self.pop(),
            Op.popn.int() => {
                const n = self.readByte();
                self.stack_top.? -= n;
            },
            Op.get_local.int() => {
                const slot: u8 = self.readByte();
                self.push(self.stack[slot]);
            },
            Op.set_local.int() => {
                const slot: u8 = self.readByte();
                self.stack[slot] = self.peek(0).*;
            },
            Op.get_global.int() => {
                const name = self.readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    return InterpretError.RuntimeError;
                }
                self.push(value);
            },
            Op.define_global.int() => {
                const name = self.readString();
                _ = self.globals.set(name, self.peek(0).*);
                _ = self.pop();
            },
            Op.set_global.int() => {
                const name = self.readString();
                if (self.globals.set(name, self.peek(0).*)) {
                    _ = self.globals.delete(name);
                    self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    return InterpretError.RuntimeError;
                }
            },
            Op.equal.int() => {
                const b = self.pop();
                const a = self.pop();
                self.push(Value.boolVal(valuesEqual(a, b)));
            },
            Op.greater.int() =>  try self.binaryOp(bool, Value.boolVal, .greater),
            Op.less.int() =>     try self.binaryOp(bool, Value.boolVal, .less),
            Op.add.int() => {
                if (self.peek(0).isString() and self.peek(1).isString()) {
                    self.concatenate();
                } else if (self.peek(0).isNumber() and self.peek(1).isNumber()) {
                    const b = self.pop().number;
                    self.peek(0).*.number += b;
                } else {
                    self.runtimeError("Operands must be two numbers or two strings.", .{});
                    return InterpretError.RuntimeError;
                }
            },
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
            Op.print.int() => {
                printValue(self.pop());
                std.debug.print("\n", .{});
            },
            Op.jump.int() => {
                const offset: u16 = self.readTwoBytes();
                self.ip.? += offset;
            },
            Op.jump_if_false.int() => {
                const offset: u16 = self.readTwoBytes();
                if (isFalsey(self.peek(0).*)) self.ip.? += offset;
            },
            Op.jump_if_false_pop.int() => {
                const offset: u16 = self.readTwoBytes();
                if (isFalsey(self.pop())) self.ip.? += offset;
            },
            Op.loop.int() => {
                const offset: u16 = self.readTwoBytes();
                self.ip.? -= offset;
            },
            Op.case.int() => {
                const offset: u16 = self.readTwoBytes();
                const b = self.pop();
                if (valuesEqual(self.peek(0).*, b)) {
                    _ = self.pop(); // ^^^^^^^ pop this (the switch value)
                } else {
                    self.ip.? += offset; // jump over the case body
                }
            },
            Op.@"return".int() => {
                // Exit interpreter.
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
inline fn readTwoBytes(self: *Self) u16 {
    const bytes = [2]u8{ self.ip.?[0], self.ip.?[1] };
    self.ip.? += 2;
    return @bitCast(bytes);
}
inline fn readString(self: *Self) *objects.ObjString {
    return self.readConst().asString();
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
    if (!self.peek(0).isNumber() or !self.peek(1).isNumber()) {
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
    return value.isNil() or (value.isBool() and !value.bool);
}
fn concatenate(self: *Self) void {
    const b = self.pop().asString();
    const a = self.pop().asString();

    const ab_len = a.len() + b.len();
    // a + b + null
    var chars = loxmem.allocate(u8, ab_len + 1);
    @memcpy(chars[0..a.len()], a.chars);
    @memcpy(chars[a.len()..ab_len], b.chars);

    const result = objects.takeString(loxmem.null_terminate(chars));
    self.push(Value.objVal(objects.ObjString, result));
}

fn codeIndex(self: *Self) usize {
    return @intFromPtr(self.ip.?) - @intFromPtr(self.chunk.?.code.items.ptr);
}

pub const InterpretError = error{ CompileError, RuntimeError };
