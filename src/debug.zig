const std = @import("std");
const print = std.debug.print;
const Chunk = @import("./Chunk.zig");
const OpCode = Chunk.OpCode;
const values = @import("./values.zig");
const printValue = values.print;
const StaticType = @import("compiler.zig").StaticType;

pub fn disassembleChunk(chunk: *Chunk, name: []const u8) void {
    print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.count()) {
        offset = disassembleInstruction(chunk, offset);
    }
}

pub fn disassembleInstruction(chunk: *Chunk, starting_offset: usize) usize {
    var offset = starting_offset;

    print("{d:0>4} ", .{offset});
    const line = chunk.getLine(offset);
    if (offset > 0 and line == chunk.getLine(offset - 1)) {
        print("   | ", .{});
    } else {
        print("{d:>4} ", .{line});
    }

    const instruction_byte: u8 = chunk.idx(offset);
    const instruction: OpCode = std.meta.intToEnum(OpCode, instruction_byte) catch {
        print("Unknown opcode {d}\n", .{instruction_byte});
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
        .assert_type => {
            const type_num = chunk.idx(offset + 1);
            const type_name = @tagName(StaticType.fromInt(type_num));
            print("{s:<"++NAME_PADDING_N++"} is {s}\n",
                .{"OP_ASSERT_TYPE", type_name});
            return offset + 2;
        },
        .get_global => constantInstruction("OP_GET_GLOBAL", chunk, offset),
        .define_global => constantInstruction("OP_DEFINE_GLOBAL", chunk, offset),
        .set_global => constantInstruction("OP_SET_GLOBAL", chunk, offset),
        .get_upvalue => byteInstruction("OP_GET_UPVALUE", chunk, offset),
        .set_upvalue => byteInstruction("OP_SET_UPVALUE", chunk, offset),
        .get_property => constantInstruction("OP_GET_PROPERTY", chunk, offset),
        .set_property => constantInstruction("OP_SET_PROPERTY", chunk, offset),
        .get_super => constantInstruction("OP_GET_SUPER", chunk, offset),
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
        .case => jumpInstruction("OP_CASE", 1, chunk, offset),
        .call => byteInstruction("OP_CALL", chunk, offset),
        .invoke => invokeInstruction("OP_INVOKE", chunk, offset),
        .super_invoke => invokeInstruction("OP_SUPER_INVOKE", chunk, offset),
        .closure => {
            const constant: u8 = chunk.idx(offset + 1);
            offset += 2;
            print("{s:<"++NAME_PADDING_N++"} {d:>4} ", .{"OP_CLOSURE", constant});
            printValue(chunk.constIdx(constant));
            print("\n", .{});

            const function = chunk.constIdx(constant).asFunction();
            for (0..function.upvalue_count) |_| {
                const is_local = chunk.idx(offset);
                const index = chunk.idx(offset + 1);
                offset += 2;
                print("{d:0>4}      |     "++NAME_PADDING_S++"{s} {d}\n",
                    .{offset - 2, if (is_local != 0) "local" else "upvalue", index});
            }
            return offset;
        },
        .close_upvalue => simpleInstruction("OP_CLOSE_UPVALUE", offset),
        .@"return" => simpleInstruction("OP_RETURN", offset),
        .class => constantInstruction("OP_CLASS", chunk, offset),
        .inherit => simpleInstruction("OP_INHERIT", offset),
        .method => constantInstruction("OP_METHOD", chunk, offset),
        .debug => {
            const len = chunk.idx(offset + 1);
            const str = chunk.code.items[offset+2..offset+2+len];
            print("{s:<"++NAME_PADDING_N++"} // {s}\n", .{"OP_DEBUG", str});
            return offset + 2 + len;
        }
    };
}

const NAME_PADDING_S = [1]u8{' '} ** 20;
const NAME_PADDING_N = "20";

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.idx(offset + 1);
    print("{s:<"++NAME_PADDING_N++"} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    print("'\n", .{});
    return offset + 2;
}
fn constantInstructionLong(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant: u24 = @bitCast([3]u8{chunk.idx(offset+1), chunk.idx(offset+2), chunk.idx(offset+3)});
    print("{s:<"++NAME_PADDING_N++"} {d:>4} '", .{name, constant});
    printValue(chunk.constIdx(constant));
    print("'\n", .{});
    return offset + 4;
}

fn invokeInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const constant = chunk.idx(offset + 1);
    const arg_count = chunk.idx(offset + 2);
    print("{s:<"++NAME_PADDING_N++"} ({d} args) {d:>4} '", .{name, arg_count, constant});
    printValue(chunk.constIdx(constant));
    print("'\n", .{});
    return offset + 3;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    print("{s}\n", .{name});
    return offset + 1;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize) usize {
    const slot: u8 = chunk.idx(offset + 1);
    print("{s:<"++NAME_PADDING_N++"} {d:>4}\n", .{name, slot});
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: i2, chunk: *Chunk, offset: usize) usize {
    const jump: u16 = @bitCast([2]u8{chunk.idx(offset+1), chunk.idx(offset+2)});
    const jumpAddr: isize = @as(isize, @intCast(offset)) + 3 + @as(isize, sign) * @as(isize, jump);
    print("{s:<"++NAME_PADDING_N++"} {d:>4} -> {d}\n",
        .{name, offset, jumpAddr});
    return offset + 3;
}
