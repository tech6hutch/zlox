const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const printValue = @import("./value.zig").printValue;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    std.debug.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count()) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{chunk.lines.items[offset]});
    }

    const instruction_byte: u8 = chunk.idx(offset);
    const instruction: OpCode = std.meta.intToEnum(OpCode, instruction_byte) catch {
        std.debug.print("Unknown opcode {d}\n", .{offset});
        return offset + 1;
    };
    return switch (instruction) {
        OpCode.op_constant => constantInstruction("OP_CONSTANT", chunk, offset),
        OpCode.op_return => simpleInstruction("OP_RETURN", offset),
    };
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.idx(offset + 1);
    std.debug.print("{s:<16} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    std.debug.print("'\n", .{});
    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
