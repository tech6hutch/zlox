const std = @import("std");
const Chunk = @import("./Chunk.zig");
const values = @import("./values.zig");
const Value = values.Value;
const printValue = values.print;

const STACK_MAX: usize = 256;

const Self = @This();

chunk: ?*Chunk,
ip: ?[*]u8,
stack: [STACK_MAX]Value,
stack_top: ?[*]Value,

pub fn init() Self {
    var vm: Self = .{
        .chunk = null,
        .ip = null,
        .stack = undefined,
        .stack_top = null,
    };
    vm.resetStack();
    return vm;
}
pub fn deinit(self: *Self) void {
    _ = self;
}

pub fn interpret(self: *Self, chunk: *Chunk) InterpretError!void {
    self.chunk = chunk;
    self.ip = chunk.code.items.ptr;
    return self.run();
}

fn run(self: *Self) InterpretError!void {
    const Op = Chunk.OpCode;
    while (true) {
        const instruction = self.readByte();
        switch (instruction) {
            Op.constant.int(), Op.constant_long.int() => {
                const constant = self.readConst();
                printValue(constant);
                std.debug.print("\n", .{});
            },
            Op.@"return".int() => {
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

fn resetStack(self: *Self) void {
    self.stack_top = self.stack[0..];
}

pub const InterpretError = error{ CompileError, RuntimeError };
