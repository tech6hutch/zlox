const std = @import("std");
const Chunk = @import("./Chunk.zig");
const OpCode = Chunk.OpCode;
const values = @import("./values.zig");
const printValue = values.print;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count()) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    const line = chunk.getLine(offset);
    if (offset > 0 and line == chunk.getLine(offset - 1)) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{line});
    }

    const instruction_byte: u8 = chunk.idx(offset);
    const instruction: OpCode = std.meta.intToEnum(OpCode, instruction_byte) catch {
        std.debug.print("Unknown opcode {d}\n", .{offset});
        return offset + 1;
    };
    return switch (instruction) {
        .constant => constantInstruction("OP_CONSTANT", chunk, offset),
        .constant_long => constantInstructionLong("OP_CONSTANT_LONG", chunk, offset),
        .add => simpleInstruction("OP_ADD", offset),
        .subtract => simpleInstruction("OP_SUBTRACT", offset),
        .multiply => simpleInstruction("OP_MULTIPLY", offset),
        .divide => simpleInstruction("OP_DIVIDE", offset),
        .negate => simpleInstruction("OP_NEGATE", offset),
        .@"return" => simpleInstruction("OP_RETURN", offset),
    };
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.idx(offset + 1);
    std.debug.print("{s:<16} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    std.debug.print("'\n", .{});
    return offset + 2;
}
fn constantInstructionLong(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant: u24 = @bitCast([3]u8{chunk.idx(offset+1), chunk.idx(offset+2), chunk.idx(offset+3)});
    std.debug.print("{s:<16} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    std.debug.print("'\n", .{});
    return offset + 4;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
