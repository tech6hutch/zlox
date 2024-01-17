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

    while (!match(.eof)) {
        declaration();
    }

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
    primary,

    fn lessEqual(self: Precedence, other: Precedence) bool {
        return @intFromEnum(self) <= @intFromEnum(other);
    }
};

const ParseFn = *const fn(can_assign: bool) void;

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

fn check(kind: TokenKind) bool {
    return parser.current.kind == kind;
}

fn match(kind: TokenKind) bool {
    if (!check(kind)) return false;
    advance();
    return true;
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

fn binary(_: bool) void {
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

fn literal(_: bool) void {
    switch (parser.previous.kind) {
        .false => emitOp(.false),
        .nil => emitOp(.nil),
        .true => emitOp(.true),
        else => unreachable
    }
}

fn grouping(_: bool) void {
    expression();
    consume(.right_paren, "Expect ')' after expression.");
}

fn expression() void {
    parsePrecedence(.assignment);
}

fn varDeclaration() void {
    const global: u8 = parseVariable("Expect variable name.");

    if (match(.equal)) {
        expression();
    } else {
        emitOp(.nil);
    }
    consume(.semicolon, "Expect ';' after variable declaration.");

    defineVariable(global);
}

fn expressionStatement() void {
    expression();
    consume(.semicolon, "Expect ';' after expression.");
    emitOp(.pop);
}

fn printStatement() void {
    expression();
    consume(.semicolon, "Expect ';' after value.");
    emitOp(.print);
}

fn synchronize() void {
    parser.panic_mode = false;

    while (parser.current.kind != .eof) {
        if (parser.previous.kind == .semicolon) return;
        switch (parser.current.kind) {
            .class, .fun, .@"var",
            .@"for", .@"if", .@"while",
            .print, .@"return" => return,

            // Do nothing.
            else => {}
        }

        advance();
    }
}

fn declaration() void {
    if (match(.@"var")) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panic_mode) synchronize();
}

fn statement() void {
    if (match(.print)) {
        printStatement();
    } else {
        expressionStatement();
    }
}

fn number(_: bool) void {
    const value = std.fmt.parseFloat(f64, parser.previous.lexeme)
        // The scanner should return a proper number.
        catch unreachable;
    emitConstant(.{ .number = value });
}

fn string(_: bool) void {
    emitConstant(Value.objVal(objects.ObjString, copyString(parser.previous.lexeme[1..parser.previous.lexeme.len-1])));
}

fn namedVariable(name: Token, can_assign: bool) void {
    const arg: u8 = identifierConstant(&name);

    if (can_assign and match(.equal)) {
        expression();
        emitBytes(.set_global, arg);
    } else {
        emitBytes(.get_global, arg);
    }
}

fn variable(can_assign: bool) void {
    namedVariable(parser.previous, can_assign);
}

fn unary(_: bool) void {
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
    arr.set(.identifier,    ParseRule.init(variable, null,   .none));
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
    const can_assign = precedence.lessEqual(.assignment);
    prefixRule.?(can_assign);

    while (precedence.lessEqual(getRule(parser.current.kind).precedence)) {
        advance();
        const infixRule = getRule(parser.previous.kind).infix;
        infixRule.?(can_assign);
    }

    if (can_assign and match(.equal)) {
        err("Invalid assignment target.");
    }
}

fn identifierConstant(name: *const Token) u8 {
    return makeConstant(Value.objVal(objects.ObjString, copyString(name.lexeme)));
}

fn parseVariable(error_message: []const u8) u8 {
    consume(.identifier, error_message);
    return identifierConstant(&parser.previous);
}

fn defineVariable(global: u8) void {
    emitBytes(.define_global, global);
}

fn getRule(kind: TokenKind) *const ParseRule {
    return &rules.get(kind);
}
