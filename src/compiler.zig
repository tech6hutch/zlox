const std = @import("std");
const Scanner = @import("./Scanner.zig");

var scanner: Scanner = undefined;

pub fn compile(source: [*:0]const u8) void {
    scanner = Scanner.init(source);
    var line: usize = 0;
    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            std.debug.print("{d:>4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("   | ", .{});
        }
        if (token.kind == .err) {
            std.debug.print(
                "{d:>2}{s} '{s}' (at char '{s}')\n",
                .{@intFromEnum(token.kind), @tagName(token.kind), token.err_msg, token.lexeme});
        } else {
            std.debug.print(
                "{d:>2}{s} '{s}'\n",
                .{@intFromEnum(token.kind), @tagName(token.kind), token.lexeme});
        }

        if (token.kind == .eof) break;
    }
}
