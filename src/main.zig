const std = @import("std");
const Chunk = @import("./Chunk.zig");
const Op = Chunk.OpCode;
const Vm = @import("./Vm.zig");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

pub fn main() !void {
    var vm: Vm = undefined;
    vm.init();

    var args = std.process.argsAlloc(allocator);
    switch (args.len()) {
        1 => repl(),
        2 => runFile(args[1]),
        else => {
            const stderr = std.io.getStdErr().writer();
            stderr.print("Usage: zlox [path]\n", .{});
            std.process.exit(64);
        }
    }

    vm.deinit();
    chunk.deinit();
}

fn repl() void {
	var stdout = std.io.getStdOut().writer();
	var stdin = std.io.getStdIn().reader();
	var line_buf: [1024]u8 = undefined;
	while (true) {
		stdout.print("> ", .{});

		const line = try stdin.readUntilDelimiter(&line_buf, "\n");
		if (line.len() == 0) {
			stdout.print("\n", .{});
			break;
		}

		interpret(line);
	}
}

fn runFile(path: *const u8) void {
	const source = readFile(path);
	const result = interpret(source);
	// todo: free(source)

	result catch |e| switch (e) {
		.CompileError => std.process.exit(65),
		.RuntimeError => std.process.exit(70),
	};
}
