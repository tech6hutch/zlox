const std = @import("std");
const Chunk = @import("./Chunk.zig");
const Op = Chunk.OpCode;
const dbg = @import("./debug.zig");
const Vm = @import("./Vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var vm: Vm = undefined;
    vm.init();

    var chunk = Chunk.init(allocator);

    try chunk.write_const(1.2, 123);
    try chunk.write_const(3.4, 123);
    try chunk.write_op_code(Op.add, 123);

    try chunk.write_const(5.6, 123);
    try chunk.write_op_code(Op.divide, 123);
    try chunk.write_op_code(Op.negate, 123);

    try chunk.write_op_code(Op.@"return", 123);

    // dbg.disassembleChunk(&chunk, "test chunk");
    try vm.interpret(&chunk);

    vm.deinit();
    chunk.deinit();
}
