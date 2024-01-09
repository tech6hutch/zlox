const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const Value = @import("./value.zig").Value;
const printValue = @import("./value.zig").printValue;

const STACK_MAX: usize = 256;

pub const VM = struct {
    chunk: ?*Chunk,
    ip: ?[*]u8,
    stack: [STACK_MAX]Value,
    stack_top: ?[*]Value,

    pub fn init() VM {
        var vm: VM = .{
            .chunk = null,
            .ip = null,
            .stack = undefined,
            .stack_top = null,
        };
        vm.resetStack();
        return vm;
    }
    pub fn deinit(self: *VM) void {
        _ = self;
    }

    pub fn interpret(self: *VM, chunk: *Chunk) InterpretError!void {
        self.chunk = chunk;
        self.ip = chunk.code.items.ptr;
        return self.run();
    }

    fn run(self: *VM) InterpretError!void {
        while (true) {
            const instruction = self.readByte();
            switch (instruction) {
                @intFromEnum(OpCode.op_constant), @intFromEnum(OpCode.op_constant_long) => {
                    const constant = self.readConst();
                    printValue(constant);
                    std.debug.print("\n", .{});
                },
                @intFromEnum(OpCode.op_return) => {
                    return;
                },
                else => {
                    std.debug.panic("unknown opcode {d}", .{instruction});
                },
            }
        }
    }
    inline fn readByte(self: *VM) u8 {
        const byte = self.ip.?[0];
        self.ip.? += 1;
        return byte;
    }
    inline fn readConst(self: *VM) Value {
        return self.chunk.?.constIdx(self.readByte());
    }

    fn resetStack(self: *VM) void {
        self.stack_top = self.stack[0..];
    }
};

pub const InterpretError = error{ CompileError, RuntimeError };
