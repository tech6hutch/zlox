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
        .nil => simpleInstruction("OP_NIL", offset),
        .true => simpleInstruction("OP_TRUE", offset),
        .false => simpleInstruction("OP_FALSE", offset),
        .pop => simpleInstruction("OP_POP", offset),
        .popn => byteInstruction("OP_POPN", chunk, offset),
        .get_local => byteInstruction("OP_GET_LOCAL", chunk, offset),
        .set_local => byteInstruction("OP_SET_LOCAL", chunk, offset),
        .get_global => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .define_global => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .set_global => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .equal => simpleInstruction("OP_EQUAL", offset),
        .greater => simpleInstruction("OP_GREATER", offset),
        .less => simpleInstruction("OP_LESS", offset),
        .add => simpleInstruction("OP_ADD", offset),
        .subtract => simpleInstruction("OP_SUBTRACT", offset),
        .multiply => simpleInstruction("OP_MULTIPLY", offset),
        .divide => simpleInstruction("OP_DIVIDE", offset),
        .not => simpleInstruction("OP_NOT", offset),
        .negate => simpleInstruction("OP_NEGATE", offset),
        .print => simpleInstruction("OP_PRINT", offset),
        .jump => jumpInstruction("OP_JUMP", 1, chunk, offset),
        .jump_if_false => jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        .jump_if_false_pop => jumpInstruction("OP_JUMP_IF_FALSE_POP", 1, chunk, offset),
        .loop => jumpInstruction("OP_LOOP", -1, chunk, offset),
        .@"return" => simpleInstruction("OP_RETURN", offset),
    };
}

const NAME_PADDING = "20";

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.idx(offset + 1);
    std.debug.print("{s:<"++NAME_PADDING++"} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    std.debug.print("'\n", .{});
    return offset + 2;
}
fn constantInstructionLong(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant: u24 = @bitCast([3]u8{chunk.idx(offset+1), chunk.idx(offset+2), chunk.idx(offset+3)});
    std.debug.print("{s:<"++NAME_PADDING++"} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    std.debug.print("'\n", .{});
    return offset + 4;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot: u8 = chunk.idx(offset + 1);
    std.debug.print("{s:<"++NAME_PADDING++"} {d:>4}\n", .{name, slot});
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i2, chunk: *Chunk, offset: usize) usize {
    const jump: u16 = @bitCast([2]u8{chunk.idx(offset+1), chunk.idx(offset+2)});
    const jumpAddr: isize = @as(isize, @intCast(offset)) + 3 + @as(isize, sign) * @as(isize, jump);
    std.debug.print("{s:<"++NAME_PADDING++"} {d:>4} -> {d}\n",
        .{name, offset, jumpAddr});
    return offset + 3;
}
