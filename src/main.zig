const std = @import("std");
const Chunk = @import("./Chunk.zig");
const Op = Chunk.OpCode;
const Vm = @import("./Vm.zig");

const MAX_FILE_SIZE = 1_000_000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

var vm: Vm = undefined;

pub fn main() !void {
    vm.init(allocator);

    const args = try std.process.argsAlloc(allocator);
    switch (args.len) {
        1 => try repl(),
        2 => try runFile(args[1]),
        else => {
            const stderr = std.io.getStdErr().writer();
            try stderr.print("Usage: zlox [path]\n", .{});
            std.process.exit(64);
        }
    }

    vm.deinit();
}

fn repl() !void {
    var stdout = std.io.getStdOut().writer();
    var stderr = std.io.getStdErr().writer();
    var stdin = std.io.getStdIn().reader();
    var line_buf: [1024]u8 = undefined;
    while (true) {
        try stdout.print("> ", .{});

        var line: []const u8 = try (stdin.readUntilDelimiter(line_buf[0..line_buf.len-1], '\n')
            catch |e| if (e == error.EndOfStream) @as([]const u8, line_buf[0..0]) else e);
        line = std.mem.trimRight(u8, line, "\r"); // Windows line ending
        if (line.len == 0) {
            try stdout.print("\n", .{});
            break;
        }

        line_buf[line.len] = 0;
        const line_terminated: [:0]u8 = line_buf[0..line.len:0];

        vm.interpret(line_terminated) catch |e|
            try stderr.print("{s}\n", .{switch (e) {
                error.CompileError => "compile error",
                error.RuntimeError => "runtime error",
            }});
    }
}

fn runFile(path: []const u8) !void {
    const source = try readFile(path);
    defer allocator.free(source);
    vm.interpret(source) catch |e| switch (e) {
        error.CompileError => std.process.exit(65),
        error.RuntimeError => std.process.exit(70),
    };
}

fn readFile(path: []const u8) ![:0]u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch {
        try std.io.getStdErr().writer().print("Could not open file \"{s}\".\n", .{path});
        std.process.exit(74);
    };
    defer file.close();

    const contents: [:0]u8 = file.readToEndAllocOptions(
        allocator,
        MAX_FILE_SIZE,
        null,
        @alignOf(u8),
        0
    ) catch |e| {
        var stderr = std.io.getStdErr().writer();
        switch (e) {
            error.FileTooBig => try stderr.print(
                "Refuse to read file \"{s}\" because it's larger than {d} bytes.\n",
                .{path, MAX_FILE_SIZE}),
            else => try stderr.print(
                "Could not read file \"{s}\".\n", .{path})
        }
        std.process.exit(74);
    };

    return contents;
}
