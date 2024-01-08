const std = @import("std");
const Chunk = @import("./chunk.zig").Chunk;
const OpCode = @import("./chunk.zig").OpCode;
const dbg = @import("./debug.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var chunk = Chunk.init(allocator);

    try chunk.write_const(1.2, 123);

    try chunk.write_op_code(OpCode.op_return, 123);

    dbg.disassembleChunk(&chunk, "test chunk");
    chunk.free();
}
