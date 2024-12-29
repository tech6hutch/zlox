const std = @import("std");
const common = @import("common.zig");

start: [*:0]const u8,
current: [*:0]const u8,
line: usize,

const Self = @This();

pub fn init(source: [*:0]const u8) Self {
    return .{
        .start = source,
        .current = source,
        .line = 1,
    };
}

fn isAlpha(c: u8) bool {
    return switch (c) {
        'a'...'z', 'A'...'Z', '_' => true,
        else => false
    };
}
fn isDigit(c: u8) bool {
    return switch (c) {
        '0'...'9' => true,
        else => false
    };
}
pub fn scanToken(self: *Self) Token {
    self.skipWhitespace();
    self.start = self.current;

    if (self.isAtEnd()) return self.makeToken(.eof);

    const c = self.advance();
    if (isAlpha(c)) return self.identifier();
    if (isDigit(c)) return self.number();

    return switch (c) {
        '(' => self.makeToken(.left_paren),
        ')' => self.makeToken(.right_paren),
        '{' => self.makeToken(.left_brace),
        '}' => self.makeToken(.right_brace),
        ';' => self.makeToken(.semicolon),
        ',' => self.makeToken(.comma),
        '.' => self.makeToken(.dot),
        '-' => self.makeToken(.minus),
        '+' => self.makeToken(.plus),
        '/' => self.makeToken(.slash),
        '*' => self.makeToken(.star),
        '!' => self.makeToken(
            if (self.match('=')) .bang_equal else .bang),
        '=' => self.makeToken(
            if (self.match('=')) .equal_equal else .equal),
        '<' => self.makeToken(
            if (self.match('=')) .less_equal else .less),
        '>' => self.makeToken(
            if (self.match('=')) .greater_equal else .greater),
        '"' => self.string(),
        ':' => self.makeToken(.colon),
        else => self.errorToken("Unexpected character.")
    };
}
fn isAtEnd(self: *Self) bool {
    return self.current[0] == 0;
}
fn advance(self: *Self) u8 {
    const c = self.current[0];
    self.current += 1;
    return c;
}
fn peek(self: *Self) u8 {
    return self.current[0];
}
fn peekNext(self: *Self) u8 {
    return if (self.isAtEnd()) 0 else self.current[1];
}
fn match(self: *Self, expected: u8) bool {
    if (self.isAtEnd()) return false;
    if (self.current[0] != expected) return false;
    self.current += 1;
    return true;
}
fn currentSlice(self: *Self) []const u8 {
    return self.start[0..(@intFromPtr(self.current) - @intFromPtr(self.start))];
}
fn makeToken(self: *Self, kind: TokenKind) Token {
    return .{
        .kind = kind,
        .lexeme = self.currentSlice(),
        .line = self.line,
    };
}
fn errorToken(self: *Self, comptime message: []const u8) Token {
    return .{
        .kind = .err,
        .lexeme = self.currentSlice(),
        .err_msg = message,
        .line = self.line,
    };
}
fn skipWhitespace(self: *Self) void {
    while (true) {
        const c = self.peek();
        switch (c) {
            ' ', '\r', '\t' => _ = self.advance(),
            '\n' => {
                self.line += 1;
                _ = self.advance();
            },
            '/' =>
                if (self.peekNext() == '/') {
                    // A comment goes until the end of the line.
                    while (self.peek() != '\n' and !self.isAtEnd()) {
                        _ = self.advance();
                    }
                } else {
                    return;
                },
            else => return
        }
    }
}
fn checkKeyword(self: *Self, comptime start: usize, comptime length: usize,
        comptime rest: []const u8, comptime kind: TokenKind) TokenKind {
    comptime {
        std.debug.assert(rest.len == length);
    }
    const current = self.currentSlice();
    if (current.len == start + length and
            std.mem.eql(u8, current[start..], rest)) {
        return kind;
    }

    return .identifier;
}
fn identifierKind(self: *Self) TokenKind {
    return switch (self.start[0]) {
        'a' => self.checkKeyword(1, 2, "nd", .@"and"),
        'b' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    'o' => self.checkKeyword(2, 2, "ol", .type_bool),
                    'r' => self.checkKeyword(2, 3, "eak", .@"break"),
                    else => .identifier
                }
            else .identifier,
        'c' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    'a' => self.checkKeyword(2, 2, "se", .case),
                    'l' => self.checkKeyword(2, 3, "ass", .class),
                    'o' => self.checkKeyword(2, 6, "ntinue", .@"continue"),
                    else => .identifier
                }
            else .identifier,
        'e' => self.checkKeyword(1, 3, "lse", .@"else"),
        'f' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    'a' => self.checkKeyword(2, 3, "lse", .false),
                    'l' => self.checkKeyword(2, 3, "oat", .type_float),
                    'o' => self.checkKeyword(2, 1, "r", .@"for"),
                    'u' => self.checkKeyword(2, 1, "n", .fun),
                    else => .identifier
                }
            else .identifier,
        'i' => self.checkKeyword(1, 1, "f", .@"if"),
        'n' => self.checkKeyword(1, 2, "il", .nil),
        'o' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    'b' => self.checkKeyword(2, 4, "ject", .type_object),
                    'r' => self.checkKeyword(2, 0, "", .@"or"),
                    else => .identifier
                }
            else .identifier,
        'p' =>
            if (
                common.USE_PRINT_STMT and
                self.checkKeyword(1, 4, "rint", .print) == .print
            ) .print
            else .identifier,
        'r' => self.checkKeyword(1, 5, "eturn", .@"return"),
        's' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    't' => self.checkKeyword(2, 4, "ring", .type_string),
                    'u' => self.checkKeyword(2, 3, "per", .super),
                    'w' => self.checkKeyword(2, 4, "itch", .@"switch"),
                    else => .identifier
                }
            else .identifier,
        't' =>
            if (self.currentSlice().len > 1)
                switch (self.start[1]) {
                    'h' => self.checkKeyword(2, 2, "is", .this),
                    'r' => self.checkKeyword(2, 2, "ue", .true),
                    else => .identifier
                }
            else .identifier,
        'v' => self.checkKeyword(1, 2, "ar", .@"var"),
        'w' => self.checkKeyword(1, 4, "hile", .@"while"),
        else => .identifier
    };
}
fn identifier(self: *Self) Token {
    while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();
    return self.makeToken(self.identifierKind());
}
fn number(self: *Self) Token {
    while (isDigit(self.peek())) _ = self.advance();

    // Look for a fractional part.
    if (self.peek() == '.' and isDigit(self.peekNext())) {
        // Consume the ".".
        _ = self.advance();

        while (isDigit(self.peek())) _ = self.advance();
    }

    return self.makeToken(.number);
}
fn string(self: *Self) Token {
    while (self.peek() != '"' and !self.isAtEnd()) {
        if (self.peek() == '\n') self.line += 1;
        _ = self.advance();
    }

    if (self.isAtEnd()) return self.errorToken("Unterminated string.");

    // The closing quote.
    _ = self.advance();
    return self.makeToken(.string);
}

pub const Token = struct {
    kind: TokenKind,
    lexeme: []const u8,
    err_msg: []const u8 = "",
    line: usize,
};

pub const TokenKind = enum {
    // Single-character tokens.
    left_paren, right_paren,
    left_brace, right_brace,
    comma, dot, minus, plus,
    semicolon, slash, star,
    // One or two character tokens.
    bang, bang_equal,
    equal, equal_equal,
    greater, greater_equal,
    less, less_equal,
    // Literals.
    identifier, string, number,
    // Keywords.
    @"and", class, @"else", false,
    @"for", fun, @"if", nil, @"or",
    print, @"return", super, this,
    true, @"var", @"while",
    // Extensions to Lox.
    @"switch", case,
    @"break", @"continue",
    colon, type_bool, type_float, type_string, type_object,

    err, eof
};
