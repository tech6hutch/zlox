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
const ObjFunction = objects.ObjFunction;
const ObjNative = objects.ObjNative;
const ObjString = objects.ObjString;
const Table = @import("./Table.zig");

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * 256;

const CallFrame = struct {
    function: *ObjFunction,
    ip: [*]u8,
    slots: [*]Value,

    // Pray to God and Andrew Kelley that `self` becomes `frame` when these get
    // inlined in run() below and that `frame` is in a register 🙏
    inline fn readByte(self: *CallFrame) u8 {
        const byte = self.ip[0];
        self.ip += 1;
        return byte;
    }
    inline fn readConst(self: *CallFrame) Value {
        return self.function.chunk.constIdx(self.readByte());
    }
    inline fn readTwoBytes(self: *CallFrame) u16 {
        const bytes = [2]u8{ self.ip[0], self.ip[1] };
        self.ip += 2;
        return @bitCast(bytes);
    }
    inline fn readString(self: *CallFrame) *ObjString {
        return self.readConst().asString();
    }

    fn codeIndex(self: *CallFrame) usize {
        return @intFromPtr(self.ip) - @intFromPtr(self.function.chunk.code.items.ptr);
    }
};

const Self = @This();
pub var vm: Self = undefined;

fn clockNative(_: []const Value) Value {
    return Value.numberVal(@as(f64, @floatFromInt(std.time.microTimestamp())) / std.time.us_per_s);
}

frames: [FRAMES_MAX]CallFrame,
frame_count: std.math.IntFittingRange(0, FRAMES_MAX),

stack: [STACK_MAX]Value,
stack_top: ?[*]Value,
globals: Table,
strings: Table,
objs: ?*Obj,

pub fn init(self: *Self) void {
    self.stack_top = null;
    self.resetStack();
    self.globals = Table.init();
    self.strings = Table.init();
    self.objs = null;

    self.defineNative("clock", clockNative);
}
pub fn deinit(self: *Self) void {
    self.globals.deinit();
    self.strings.deinit();
    loxmem.freeObjects(self);
}
fn resetStack(self: *Self) void {
    self.stack_top = self.stack[0..];
    self.frame_count = 0;
}
/// If negative, something has gone very wrong.
fn stackSlotsInUse(self: *Self) i64 {
    return @intCast(@intFromPtr(self.stack_top) - @intFromPtr(&self.stack[0]));
}
fn runtimeError(self: *Self, comptime format: []const u8, args: anytype) void {
    var stderr = std.io.getStdErr().writer();
    stderr.print(format, args) catch {};
    stderr.writeByte('\n') catch {};

    var i = self.frame_count;
    while (i > 0) {
        i -= 1;
        const frame = &self.frames[i];
        const function = frame.function;
        const instruction = frame.codeIndex() - 1;
        stderr.print("[line {d}] in ",
            .{function.chunk.getLine(instruction)}) catch {};
        if (function.name) |name| {
            stderr.print("{s}()\n", .{name.chars}) catch {};
        } else {
            stderr.print("script\n", .{}) catch {};
        }
    }

    self.resetStack();
}

fn defineNative(self: *Self, name: []const u8, function: objects.NativeFn) void {
    // Store them on the stack temporarily so the GC won't think they're unused.
    self.push(Value.objVal(ObjString, objects.copyString(name)));
    self.push(Value.objVal(ObjNative, objects.newNative(function)));
    _ = self.globals.set(self.stack[0].asString(), self.stack[1]);
    _ = self.pop();
    _ = self.pop();
}

pub fn interpret(self: *Self, source: [*:0]const u8) InterpretError!void {
    const function = compiler.compile(source) orelse {
        return InterpretError.CompileError;
    };

    self.push(Value.objVal(ObjFunction, function));
    const script_call_succeeded = self.call(function, 0);
    std.debug.assert(script_call_succeeded);

    if (self.run()) |_| {
        if (self.stackSlotsInUse() > 0) {
            self.runtimeError(
                "Internal error: something went wrong, not all stack values were popped. Attempting to dump the stack below:",
                .{});
            self.dumpStack();
        } else if (self.stackSlotsInUse() < 0) {
            @panic("Popped too many stack values, somehow.");
        }
        return;
    } else |e| {
        return e;
    }
}

fn run(self: *Self) InterpretError!void {
    var frame: *CallFrame = &self.frames[self.frame_count - 1];

    while (true) {
        if (common.DEBUG_TRACE_EXECUTION) {
            self.dumpStack();
            _ = dbg.disassembleInstruction(&frame.function.chunk, frame.codeIndex());
        }

        const Op = Chunk.OpCode;
        const instruction = frame.readByte();
        switch (instruction) {
            Op.constant.int(), Op.constant_long.int() => {
                // TODO: y'know, I don't think this actually properly gets the constant for constant_long
                const constant: Value = frame.readConst();
                self.push(constant);
            },
            Op.nil.int() => self.push(Value.nilVal()),
            Op.true.int() => self.push(Value.boolVal(true)),
            Op.false.int() => self.push(Value.boolVal(false)),
            Op.pop.int() => _ = self.pop(),
            Op.popn.int() => {
                const n = frame.readByte();
                if (std.debug.runtime_safety and
                    @intFromPtr(self.stack_top) - n < @intFromPtr(&self.stack[0]))
                {
                    @panic("Tried to pop an empty stack.");
                }
                self.stack_top.? -= n;
            },
            Op.get_local.int() => {
                const slot: u8 = frame.readByte();
                self.push(frame.slots[slot]);
            },
            Op.set_local.int() => {
                const slot: u8 = frame.readByte();
                frame.slots[slot] = self.peek(0).*;
            },
            Op.get_global.int() => {
                const name = frame.readString();
                var value: Value = undefined;
                if (!self.globals.get(name, &value)) {
                    self.runtimeError("Undefined variable '{s}'.", .{name.chars});
                    return InterpretError.RuntimeError;
                }
                self.push(value);
            },
            Op.define_global.int() => {
                const name = frame.readString();
                _ = self.globals.set(name, self.peek(0).*);
                _ = self.pop();
            },
            Op.set_global.int() => {
                const name = frame.readString();
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
                const offset: u16 = frame.readTwoBytes();
                frame.ip += offset;
            },
            Op.jump_if_false.int() => {
                const offset: u16 = frame.readTwoBytes();
                if (isFalsey(self.peek(0).*)) frame.ip += offset;
            },
            Op.jump_if_false_pop.int() => {
                const offset: u16 = frame.readTwoBytes();
                if (isFalsey(self.pop())) frame.ip += offset;
            },
            Op.loop.int() => {
                const offset: u16 = frame.readTwoBytes();
                frame.ip -= offset;
            },
            Op.case.int() => {
                const offset: u16 = frame.readTwoBytes();
                const b = self.pop();
                if (valuesEqual(self.peek(0).*, b)) {
                    _ = self.pop(); // ^^^^^^^ pop this (the switch value)
                } else {
                    frame.ip += offset; // jump over the case body
                }
            },
            Op.call.int() => {
                const arg_count = frame.readByte();
                if (!self.callValue(self.peek(arg_count).*, arg_count)) {
                    return InterpretError.RuntimeError;
                }
                frame = &self.frames[self.frame_count - 1];
            },
            Op.@"return".int() => {
                const result: Value = self.pop();
                self.frame_count -= 1;
                if (self.frame_count == 0) {
                    _ = self.pop(); // the script itself
                    return;
                }

                self.stack_top = frame.slots;
                self.push(result);
                frame = &self.frames[self.frame_count - 1];
            },
            Op.debug.int() => {
                const len = frame.readByte();
                frame.ip += len;
            },
            else => {
                std.debug.panic("unknown opcode {d}", .{instruction});
            },
        }
    }
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

fn dumpStack(self: *Self) void {
    std.debug.print("          ", .{});
    var slot: [*]Value = &self.stack;
    while (@intFromPtr(slot) < @intFromPtr(self.stack_top)) : (slot += 1) {
        std.debug.print("[ ", .{});
        printValue(slot[0]);
        std.debug.print(" ]", .{});
    }
    std.debug.print("\n", .{});
}

fn push(self: *Self, value: Value) void {
    self.stack_top.?[0] = value;
    self.stack_top.? += 1;
}
fn pop(self: *Self) Value {
    if (std.debug.runtime_safety and
        @intFromPtr(self.stack_top) == @intFromPtr(&self.stack[0]))
    {
        @panic("Tried to pop an empty stack.");
    }
    self.stack_top.? -= 1;
    return self.stack_top.?[0];
}
inline fn peek(self: *Self, distance: usize) *Value {
    // Zig doesn't allow negative indices. Sad.
    return &(self.stack_top.? - 1 - distance)[0];
}
fn callValue(self: *Self, callee: Value, arg_count: u8) bool {
    if (callee.isObj()) {
        switch (callee.objKind()) {
            .function => return self.call(callee.asFunction(), arg_count),
            .native => {
                const native = callee.asNative();
                const result: Value = native((self.stack_top.? - arg_count)[0..arg_count]);
                self.stack_top.? -= arg_count + 1;
                self.push(result);
                return true;
            },
            else => {} // Non-callable object type.
        }
    }
    self.runtimeError("Can only call functions and classes.", .{});
    return false;
}
fn call(self: *Self, function: *ObjFunction, arg_count: u8) bool {
    if (arg_count != function.arity) {
        self.runtimeError("Expected {d} arguments but got {d}.",
            .{function.arity, arg_count});
        return false;
    }

    if (self.frame_count == FRAMES_MAX) {
        self.runtimeError("Stack overflow.", .{});
        return false;
    }

    var frame = &self.frames[self.frame_count];
    self.frame_count += 1;
    frame.function = function;
    frame.ip = function.chunk.code.items.ptr;
    frame.slots = self.stack_top.? - arg_count - 1;
    return true;
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
    self.push(Value.objVal(ObjString, result));
}

fn codeIndex(self: *Self) usize {
    return @intFromPtr(self.ip.?) - @intFromPtr(self.chunk.?.code.items.ptr);
}

pub const InterpretError = error{ CompileError, RuntimeError };
