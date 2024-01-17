const std = @import("std");
const EnumArray = std.enums.EnumArray;
const Scanner = @import("./Scanner.zig");
const Token = Scanner.Token;
const TokenKind = Scanner.TokenKind;
const Chunk = @import("./Chunk.zig");
const Op = Chunk.OpCode;
const values = @import("./values.zig");
const Value = values.Value;
const common = @import("./common.zig");
const dbg = @import("./debug.zig");
const objects = @import("./objects.zig");
const copyString = objects.copyString;

var scanner: Scanner = undefined;

pub fn compile(source: [*:0]const u8, chunk: *Chunk) bool {
    scanner = Scanner.init(source);
    compiling_chunk = chunk;

    parser.had_error = false;
    parser.panic_mode = false;

    advance();
    expression();
    consume(.eof, "Expect end of expression.");
    endCompiler();
    return !parser.had_error;
}

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool = false,
    panic_mode: bool = false,
};

const Precedence = enum {
    none,
    assignment,  // =
    @"or",       // or
    @"and",      // and
    equality,    // == !=
    comparison,  // < > <= >=
    term,        // + -
    factor,      // * /
    unary,       // ! -
    call,        // . ()
    primary
};

const ParseFn = *const fn() void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
    fn init(pre: ?ParseFn, in: ?ParseFn, prec: Precedence) ParseRule {
        return .{
            .prefix = pre,
            .infix = in,
            .precedence = prec,
        };
    }
};

var parser = Parser{
    .previous = undefined,
    .current = undefined,
};
var compiling_chunk: ?*Chunk = null;
fn currentChunk() *Chunk {
    return compiling_chunk.?;
}

fn err(message: []const u8) void {
    errorAt(&parser.previous, message);
}
fn errorAtCurrent(message: []const u8) void {
    errorAt(&parser.current, message);
}
fn errorAt(token: *Token, message: []const u8) void {
    if (parser.panic_mode) return;
    parser.panic_mode = true;
    var stderr = std.io.getStdErr().writer();
    stderr.print("[line {d}] Error", .{token.line}) catch {};
    if (token.kind == .eof) {
        stderr.print(" at end", .{}) catch {};
    } else if (token.kind == .err) {
        // Nothing.
    } else {
        stderr.print(" at '{s}'", .{token.lexeme}) catch {};
    }
    stderr.print(": {s}\n", .{message}) catch {};
    parser.had_error = true;
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = scanner.scanToken();
        if (parser.current.kind != .err) break;

        errorAtCurrent(parser.current.lexeme);
    }
}

fn consume(kind: TokenKind, message: []const u8) void {
    if (parser.current.kind == kind) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

fn emitByte(byte: u8) void {
    currentChunk().writeByte(byte, parser.previous.line) catch |e| {
        switch (e) {
            error.OutOfMemory => @panic(
                "out of memory while adding constant"),
        }
    };
}
fn emitOp(byte: Op) void {
    emitByte(@intFromEnum(byte));
}
fn emitBytes(byte1: Op, byte2: u8) void {
    emitOp(byte1);
    emitByte(byte2);
}
fn emitOps(byte1: Op, byte2: Op) void {
    emitOp(byte1);
    emitOp(byte2);
}

fn emitReturn() void {
    emitOp(.@"return");
}

fn makeConstant(value: Value) u8 {
    const constant = currentChunk().addConst(value) catch |e| {
        switch (e) {
            error.OutOfMemory => @panic(
                "out of memory while adding constant"),
        }
    };
    if (constant > std.math.maxInt(u8)) {
        err("Too many constants in one chunk.");
        return 0;
    }

    return @intCast(constant);
}

fn emitConstant(value: Value) void {
    emitBytes(Op.constant, makeConstant(value));
}

fn endCompiler() void {
    emitReturn();
    if (common.DEBUG_PRINT_CODE and !parser.had_error) {
        dbg.disassembleChunk(currentChunk(), "code");
    }
}

fn binary() void {
    const operatorKind = parser.previous.kind;
    const rule = getRule(operatorKind);
    parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));
    switch (operatorKind) {
        .bang_equal =>    emitOps(.equal, .not),
        .equal_equal =>   emitOp(.equal),
        .greater =>       emitOp(.greater),
        .greater_equal => emitOps(.less, .not),
        .less =>          emitOp(.less),
        .less_equal =>    emitOps(.greater, .not),
        .plus =>  emitOp(.add),
        .minus => emitOp(.subtract),
        .star =>  emitOp(.multiply),
        .slash => emitOp(.divide),
        else => unreachable
    }
}

fn literal() void {
    switch (parser.previous.kind) {
        .false => emitOp(.false),
        .nil => emitOp(.nil),
        .true => emitOp(.true),
        else => unreachable
    }
}

fn grouping() void {
    expression();
    consume(.right_paren, "Expect ')' after expression.");
}

fn expression() void {
    parsePrecedence(.assignment);
}

fn number() void {
    const value = std.fmt.parseFloat(f64, parser.previous.lexeme)
        // The scanner should return a proper number.
        catch unreachable;
    emitConstant(.{ .number = value });
}

fn string() void {
    emitConstant(Value.objVal(objects.ObjString, copyString(parser.previous.lexeme[1..parser.previous.lexeme.len-1])));
}

fn unary() void {
    const operatorKind = parser.previous.kind;
    // Compile the operand.
    parsePrecedence(.unary);
    // Emit the operator instruction.
    switch (operatorKind) {
        .bang => emitOp(.not),
        .minus => emitOp(.negate),
        else => unreachable
    }
}

const rules: EnumArray(TokenKind, ParseRule) = def: {
    var arr = EnumArray(TokenKind, ParseRule)
        .initFill(ParseRule.init(null, null, .none));
    arr.set(.left_paren,    ParseRule.init(grouping, null,   .none));
    arr.set(.right_paren,   ParseRule.init(null,     null,   .none));
    arr.set(.left_brace,    ParseRule.init(null,     null,   .none));
    arr.set(.right_brace,   ParseRule.init(null,     null,   .none));
    arr.set(.comma,         ParseRule.init(null,     null,   .none));
    arr.set(.dot,           ParseRule.init(null,     null,   .none));
    arr.set(.minus,         ParseRule.init(unary,    binary, .term));
    arr.set(.plus,          ParseRule.init(null,     binary, .term));
    arr.set(.semicolon,     ParseRule.init(null,     null,   .none));
    arr.set(.slash,         ParseRule.init(null,     binary, .factor));
    arr.set(.star,          ParseRule.init(null,     binary, .factor));
    arr.set(.bang,          ParseRule.init(unary,    null,   .none));
    arr.set(.bang_equal,    ParseRule.init(null,     binary, .equality));
    arr.set(.equal,         ParseRule.init(null,     null,   .none));
    arr.set(.equal_equal,   ParseRule.init(null,     binary, .equality));
    arr.set(.greater,       ParseRule.init(null,     binary, .comparison));
    arr.set(.greater_equal, ParseRule.init(null,     binary, .comparison));
    arr.set(.less,          ParseRule.init(null,     binary, .comparison));
    arr.set(.less_equal,    ParseRule.init(null,     binary, .comparison));
    arr.set(.identifier,    ParseRule.init(null,     null,   .none));
    arr.set(.string,        ParseRule.init(string,   null,   .none));
    arr.set(.number,        ParseRule.init(number,   null,   .none));
    arr.set(.@"and",        ParseRule.init(null,     null,   .none));
    arr.set(.class,         ParseRule.init(null,     null,   .none));
    arr.set(.@"else",       ParseRule.init(null,     null,   .none));
    arr.set(.false,         ParseRule.init(literal,  null,   .none));
    arr.set(.@"for",        ParseRule.init(null,     null,   .none));
    arr.set(.fun,           ParseRule.init(null,     null,   .none));
    arr.set(.@"if",         ParseRule.init(null,     null,   .none));
    arr.set(.nil,           ParseRule.init(literal,  null,   .none));
    arr.set(.@"or",         ParseRule.init(null,     null,   .none));
    arr.set(.print,         ParseRule.init(null,     null,   .none));
    arr.set(.@"return",     ParseRule.init(null,     null,   .none));
    arr.set(.super,         ParseRule.init(null,     null,   .none));
    arr.set(.this,          ParseRule.init(null,     null,   .none));
    arr.set(.true,          ParseRule.init(literal,  null,   .none));
    arr.set(.@"var",        ParseRule.init(null,     null,   .none));
    arr.set(.@"while",      ParseRule.init(null,     null,   .none));
    arr.set(.err,           ParseRule.init(null,     null,   .none));
    arr.set(.eof,           ParseRule.init(null,     null,   .none));
    break :def arr;
};

fn parsePrecedence(precedence: Precedence) void {
    advance();
    const prefixRule = getRule(parser.previous.kind).prefix;
    if (prefixRule == null) {
        err("Expect expression.");
        return;
    }
    prefixRule.?();
    while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.kind).precedence)) {
        advance();
        const infixRule = getRule(parser.previous.kind).infix;
        infixRule.?();
    }
}

fn getRule(kind: TokenKind) *const ParseRule {
    return &rules.get(kind);
}
