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
const ObjString = objects.ObjString;
const ObjFunction = objects.ObjFunction;
const copyString = objects.copyString;
const loxmem = @import("./memory.zig");

const MAX_SWITCH_CASES = 256;
const MAX_LOOP_BREAKS = 256;

var scanner: Scanner = undefined;

pub fn compile(source: [*:0]const u8) ?*ObjFunction {
    scanner = Scanner.init(source);
    var compiler: Compiler = undefined;
    initCompiler(&compiler, .script);

    parser.had_error = false;
    parser.panic_mode = false;

    advance();

    while (!match(.eof)) {
        declaration();
    }

    // Don't use `current` anymore, it's undefined now.
    const fun = endCompiler();
    return if (parser.had_error) null else fun;
}

pub fn markCompilerRoots() void {
    var compiler: ?*Compiler = current;
    while (compiler != null) {
        loxmem.markObject(objects.upcast_nullable(compiler.?.function));
        compiler = compiler.?.enclosing;
    }
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

const Local = struct {
    name: Token,
    // Needs to hold up to 256 and down to -1.
    depth: i16,
    /// Whether it's captured by a nested function.
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const LoopInfo = struct {
    scope_depth: u8,
    /// Offset of the start of the loop.
    continue_offset: usize,
    /// Jumps (from break statements) to patch. Relative to start of loop
    /// (`continue_offset`) to save space.
    break_offsets: [MAX_LOOP_BREAKS]u16,
    break_offset_count: std.math.IntFittingRange(0, MAX_LOOP_BREAKS),
};

const FunctionKind = enum {
    function,
    script,
};

const Compiler = struct {
    enclosing: ?*Compiler,
    function: ?*ObjFunction,
    kind: FunctionKind,

    // Exactly 256 because the instruction operand used to encode a local
    // is a single byte.
    locals: [256]Local,
    // Indicating we have the max 256 locals won't fit in a u8 lol.
    local_count: u9,
    upvalues: [256]Upvalue,
    // 256 is also a fine limit on nested scopes, IMHO.
    scope_depth: u8,
    innermost_loop: ?*LoopInfo,
};

var parser = Parser{
    .previous = undefined,
    .current = undefined,
};
var current: *Compiler = undefined;

fn currentChunk() *Chunk {
    return &current.function.?.chunk;
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

fn emitPops(starting_count: usize) void {
    var count = starting_count;
    while (count > 1) {
        emitOp(.popn);
        const diff: usize = @min(count, std.math.maxInt(u8));
        emitByte(@intCast(diff));
        count -= diff;
    }
    if (count == 1) {
        emitOp(.pop);
    }
}

/// Emits an instruction to (unconditionally) jump _up_ to `loop_start`.
fn emitLoop(loop_start: usize) void {
    emitOp(.loop);

    // +2 for the loop instruction's own operands, which we write below.
    const offset: usize = currentChunk().count() - loop_start + 2;
    const offset16 = std.math.cast(u16, offset)
        orelse blk: {
            err("Loop body too large.");
            break :blk 0;
        };

    const offset_bytes: [2]u8 = @bitCast(offset16);
    emitByte(offset_bytes[0]);
    emitByte(offset_bytes[1]);
}

/// Emits a jump instruction where we are now. Make sure to patch it later with
/// the right offset (at the returned location in the chunk).
fn emitJump(instruction: Op) usize {
    emitOp(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk().count() - 2;
}

/// Emits instructions that return nil.
fn emitReturn() void {
    emitOp(.nil);
    emitOp(.@"return");
}

fn emitDebug(str: []const u8) void {
    emitOp(.debug);
    const len: u8 = std.math.cast(u8, str.len) orelse {
        @panic("str must be under 256 chars");
    };
    emitByte(len);
    for (str) |char| emitByte(char);
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

/// Change the jump operand at `offset` to point just past where we are now.
fn patchJump(offset: usize) void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump: usize = currentChunk().count() - offset - 2;

    const jump16: u16 = std.math.cast(u16, jump)
        orelse blk: {
            err("Too much code to jump over.");
            break :blk 0;
        };

    if (std.debug.runtime_safety and (
        currentChunk().code.items[offset] != 0xff or
        currentChunk().code.items[offset + 1] != 0xff
    )) {
        dbg.disassembleChunk(currentChunk(), "code before panic");
        std.debug.panic(
            "You may be trying to patch a jump you already patched. See code above. " ++
            "Ignore if you're compiling a block that's exactly {d} bytes of instructions, I guess.",
            .{std.math.maxInt(u16)});
    }

    const jump_bytes: [2]u8 = @bitCast(jump16);
    currentChunk().code.items[offset] = jump_bytes[0];
    currentChunk().code.items[offset + 1] = jump_bytes[1];
}
fn patchBreaks(loop_info: LoopInfo) void {
    for (loop_info.break_offsets[0..loop_info.break_offset_count]) |break_offset| {
        patchJump(break_offset + loop_info.continue_offset);
    }
}

fn initCompiler(compiler: *Compiler, kind: FunctionKind) void {
    // TODO: isn't it going to be UB if we use `enclosing`? `current` starts out undefined.
    compiler.enclosing = current;
    compiler.function = null;
    compiler.kind = kind;
    compiler.local_count = 0;
    compiler.scope_depth = 0;
    compiler.function = objects.newFunction();
    compiler.innermost_loop = null;
    current = compiler;
    if (kind != .script) {
        current.function.?.name = copyString(parser.previous.lexeme);
    }

    var local: *Local = &current.locals[current.local_count];
    current.local_count += 1;
    local.depth = 0;
    local.is_captured = false;
    local.name.lexeme = "";
}

fn endCompiler() *ObjFunction {
    emitReturn();
    const fun = current.function.?;

    if (common.DEBUG_PRINT_CODE and !parser.had_error) {
        dbg.disassembleChunk(currentChunk(),
            if (fun.name) |name| name.chars else "<script>");
    }

    // We never read from this again, it's okay.
    current = current.enclosing orelse undefined;
    return fun;
}

fn beginScope() void {
    current.scope_depth += 1;
}

fn endScope() void {
    current.scope_depth = std.math.sub(u8, current.scope_depth, 1)
        catch {
            dbg.disassembleChunk(currentChunk(), "code before panic");
            @panic("Tried to end the top-most scope (i.e., an overflow occured in 'current.scope_depth -= 1;').");
        };

    // TODO: ok this really messes with my attempted optimization OP_POPN
    // current.local_count -= popScopesDownTo(current.scope_depth);
    while (current.local_count > 0 and
            current.locals[current.local_count - 1].depth > current.scope_depth) {
        if (current.locals[current.local_count - 1].is_captured) {
            emitOp(.close_upvalue);
        } else {
            emitOp(.pop);
        }
        current.local_count -= 1;
    }
}

/// Pops all locals at `scope_depth` and deeper. Returns the number of locals popped.
fn popScopesDownTo(scope_depth: u8) u9 {
    if (std.debug.runtime_safety and current.scope_depth < scope_depth) {
        std.debug.panic(
            "Tried to pop scopes until depth {d}, but we're already back up to depth {d}",
            .{scope_depth, current.scope_depth});
    }

    var count: u9 = 0;
    var index: u9 = current.local_count;
    while (index > 0) {
        index -= 1;
        if (current.locals[index].depth < scope_depth) break;
        count += 1;
    }
    emitPops(count);
    return count;
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

fn call(_: bool) void {
    const arg_count = argumentList();
    emitBytes(.call, arg_count);
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

fn block() void {
    while (!check(.right_brace) and !check(.eof)) {
        declaration();
    }

    consume(.right_brace, "Expect '}' after block.");
}

fn function(kind: FunctionKind) void {
    var compiler: Compiler = undefined;
    initCompiler(&compiler, kind);
    beginScope(); // ends implicitly

    consume(.left_paren, "Expect '(' after function name.");
    if (!check(.right_paren)) {
        const arity = &current.function.?.arity;
        while (true) {
            arity.* = std.math.add(u8, arity.*, 1) catch {
                errorAtCurrent(std.fmt.comptimePrint(
                    "Can't have more than 255 parameters.",
                    .{}));
                return;
            };
            const constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expect ')' after parameters.");
    consume(.left_brace, "Expect '{' before function body.");
    block();

    const fun = endCompiler();
    emitBytes(.closure, makeConstant(Value.objVal(fun)));

    for (compiler.upvalues[0..fun.upvalue_count]) |upvalue| {
        emitByte(if (upvalue.is_local) 1 else 0);
        emitByte(upvalue.index);
    }
}

fn classDeclaration() void {
    consume(.identifier, "Expect class name.");
    const name_constant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(.class, name_constant);
    defineVariable(name_constant);

    consume(.left_brace, "Expect '{' before class body.");
    consume(.right_brace, "Expect '}' after class body.");
}

fn funDeclaration() void {
    const global = parseVariable("Expect function name.");
    markInitialized(); // allows functions to refer to themselves
    function(.function);
    defineVariable(global);
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

fn breakStatement() void {
    if (current.innermost_loop) |loop_info| {
        _ = popScopesDownTo(loop_info.scope_depth + 1);
        const break_offset = emitJump(.jump) - loop_info.continue_offset;
        loop_info.break_offsets[loop_info.break_offset_count] =
            std.math.cast(u16, break_offset)
            orelse blk: {
                err("Loop body too large.");
                break :blk 0;
            };
        loop_info.break_offset_count += 1;
    } else {
        err("Can't use 'break' outside of a loop.");
    }
    consume(.semicolon, "Expect ';' after 'break'.");
}

fn continueStatement() void {
    if (current.innermost_loop) |loop_info| {
        _ = popScopesDownTo(loop_info.scope_depth + 1);
        emitLoop(loop_info.continue_offset);
    } else {
        err("Can't use 'continue' outside of a loop.");
    }
    consume(.semicolon, "Expect ';' after 'continue'.");
}

fn expressionStatement() void {
    expression();
    consume(.semicolon, "Expect ';' after expression.");
    emitOp(.pop);
}

fn forStatement() void {
    beginScope();
    consume(.left_paren, "Expect '(' after 'for'.");
    if (match(.semicolon)) {
        // No initializer.
    } else if (match(.@"var")) {
        varDeclaration();
    } else {
        expressionStatement();
    }

    var loop_info: LoopInfo = .{
        .scope_depth = current.scope_depth,
        .continue_offset = currentChunk().count(),
        .break_offsets = undefined,
        .break_offset_count = 0,
    };
    const prev_loop_info = current.innermost_loop;
    current.innermost_loop = &loop_info;

    var exit_jump: usize = std.math.maxInt(usize);
    if (!match(.semicolon)) {
        expression();
        consume(.semicolon, "Expect ';' after loop condition.");

        // Jump out of the loop if the condition is false.
        exit_jump = emitJump(.jump_if_false_pop);
    }

    if (!match(.right_paren)) {
        // Jump over the increment since it should run _after_ the body.
        const body_jump = emitJump(.jump);
        const increment_start = currentChunk().count();
        expression();
        emitOp(.pop);
        consume(.right_paren, "Expect ')' after for clauses.");

        emitLoop(loop_info.continue_offset); // jump up to the condition
        loop_info.continue_offset = increment_start;
        patchJump(body_jump);
    }

    statement();
    // Jump up to the condition (or the increment if there is one).
    emitLoop(loop_info.continue_offset);
    current.innermost_loop = prev_loop_info;

    if (exit_jump != std.math.maxInt(usize)) {
        patchJump(exit_jump);
    }
    patchBreaks(loop_info);

    endScope();
}

fn ifStatement() void {
    consume(.left_paren, "Expect '(' after 'if'.");
    expression();
    consume(.right_paren, "Expect ')' after condition.");

    const then_jump = emitJump(.jump_if_false_pop);
    statement();

    const else_jump = emitJump(.jump);

    patchJump(then_jump);

    if (match(.@"else")) statement();
    patchJump(else_jump);
}

fn printStatement() void {
    expression();
    consume(.semicolon, "Expect ';' after value.");
    emitOp(.print);
}

fn returnStatement() void {
    if (current.kind == .script) {
        err("Can't return from top-level code.");
    }

    if (match(.semicolon)) {
        emitReturn();
    } else {
        expression();
        consume(.semicolon, "Expect ';' after return value.");
        emitOp(.@"return");
    }
}

fn switchStatement() void {
    consume(.left_paren, "Expect '(' after 'switch'.");
    expression();
    consume(.right_paren, "Expect ')' after switch value.");
    consume(.left_brace, "Expect '{' after parenthesized switch value.");

    // Each are relative to the start of the switch, to save space since we
    // allocate statically like this.
    var switch_jumps: [MAX_SWITCH_CASES]u16 = undefined;
    var switch_jump_count: u16 = 0;

    const switch_offset: usize = currentChunk().count();

    var case_jump: usize = std.math.maxInt(usize);
    while (match(.case)) {
        if (case_jump != std.math.maxInt(usize)) {
            patchJump(case_jump);
        }

        consume(.left_paren, "Expect '(' after 'case'.");
        expression();
        consume(.right_paren, "Expect ')' after case value.");
        case_jump = emitJump(.case);

        statement();

        switch_jumps[switch_jump_count] = std.math.cast(u16, emitJump(.jump) - switch_offset)
            orelse {
                err("Switch is too big.");
                return;
            };
        switch_jump_count += 1;
    }
    if (case_jump != std.math.maxInt(usize)) {
        patchJump(case_jump);
    }

    // Default case.
    emitOp(.pop); // if we're here, none of the cases popped the switch value
    if (match(.@"else")) statement();

    for (switch_jumps[0..switch_jump_count]) |switch_jump| {
        patchJump(switch_jump + switch_offset);
    }

    consume(.right_brace, "Expect '}' after switch cases.");
}

fn whileStatement() void {
    beginScope(); // for consistency with for loops, even tho we don't declare anything

    var loop_info: LoopInfo = .{
        .scope_depth = current.scope_depth,
        .continue_offset = currentChunk().count(),
        .break_offsets = undefined,
        .break_offset_count = 0,
    };
    const prev_loop_info = current.innermost_loop;
    current.innermost_loop = &loop_info;

    consume(.left_paren, "Expect '(' after 'while'.");
    expression();
    consume(.right_paren, "Expect ')' after condition.");

    const exit_jump = emitJump(.jump_if_false_pop);
    statement();
    emitLoop(loop_info.continue_offset);
    current.innermost_loop = prev_loop_info;

    patchJump(exit_jump);
    patchBreaks(loop_info);

    endScope();
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
    if (match(.class)) {
        classDeclaration();
    } else if (match(.fun)) {
        funDeclaration();
    } else if (match(.@"var")) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panic_mode) synchronize();
}

fn statement() void {
    if (match(.print)) {
        printStatement();
    } else if (match(.@"break")) {
        breakStatement();
    } else if (match(.@"continue")) {
        continueStatement();
    } else if (match(.@"for")) {
        forStatement();
    } else if (match(.@"if")) {
        ifStatement();
    } else if (match(.@"return")) {
        returnStatement();
    } else if (match(.@"switch")) {
        switchStatement();
    } else if (match(.@"while")) {
        whileStatement();
    } else if (match(.left_brace)) {
        beginScope();
        block();
        endScope();
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

fn or_(_: bool) void {
    const else_jump = emitJump(.jump_if_false);
    const end_jump = emitJump(.jump);

    patchJump(else_jump);
    emitOp(.pop);

    parsePrecedence(.@"or");
    patchJump(end_jump);
}

fn string(_: bool) void {
    emitConstant(Value.objVal(copyString(parser.previous.lexeme[1..parser.previous.lexeme.len-1])));
}

fn namedVariable(name: Token, can_assign: bool) void {
    var arg = resolveLocal(current, &name);
    var get_op: Op = .get_local;
    var set_op: Op = .set_local;
    if (arg == -1) {
        arg = resolveUpvalue(current, &name);
        get_op = .get_upvalue;
        set_op = .set_upvalue;
        if (arg == -1) {
            arg = identifierConstant(&name);
            get_op = .get_global;
            set_op = .set_global;
        }
    }

    if (can_assign and match(.equal)) {
        expression();
        emitBytes(set_op, @intCast(arg));
    } else {
        emitBytes(get_op, @intCast(arg));
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
    arr.set(.left_paren,    ParseRule.init(grouping, call,   .call));
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
    arr.set(.@"and",        ParseRule.init(null,     and_,   .@"and"));
    arr.set(.class,         ParseRule.init(null,     null,   .none));
    arr.set(.@"else",       ParseRule.init(null,     null,   .none));
    arr.set(.false,         ParseRule.init(literal,  null,   .none));
    arr.set(.@"for",        ParseRule.init(null,     null,   .none));
    arr.set(.fun,           ParseRule.init(null,     null,   .none));
    arr.set(.@"if",         ParseRule.init(null,     null,   .none));
    arr.set(.nil,           ParseRule.init(literal,  null,   .none));
    arr.set(.@"or",         ParseRule.init(null,     or_,    .@"or"));
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
    return makeConstant(Value.objVal(copyString(name.lexeme)));
}

fn identifiersEqual(a: *const Token, b: *const Token) bool {
    if (a.lexeme.len != b.lexeme.len) return false;
    return std.mem.eql(u8, a.lexeme, b.lexeme);
}

fn resolveLocal(compiler: *Compiler, name: *const Token) i16 {
    var i = compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local: *Local = &compiler.locals[i];
        if (identifiersEqual(name, &local.name)) {
            if (local.depth == -1) {
                err("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }

    return -1;
}

fn addUpvalue(compiler: *Compiler, index: u8, is_local: bool) i16 {
    const upvalue_count = compiler.function.?.upvalue_count;

    for (0..upvalue_count) |i| {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @intCast(i);
        }
    }

    // I think Bob Nystrom's code is wrong here? He checks against the number of
    // values in a u8 (256), but if we're at the max (and about to add one more),
    // wouldn't checking for the max u8 (255) be correct?
    // TODO: test this.
    if (upvalue_count == std.math.maxInt(u8)) {
        err("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[upvalue_count].is_local = is_local;
    compiler.upvalues[upvalue_count].index = index;
    compiler.function.?.upvalue_count += 1;
    return upvalue_count;
}

fn resolveUpvalue(compiler: *Compiler, name: *const Token) i16 {
    const enclosing = compiler.enclosing orelse return -1;

    const local = resolveLocal(enclosing, name);
    if (local != -1) {
        compiler.enclosing.?.locals[@intCast(local)].is_captured = true;
        return addUpvalue(compiler, @intCast(local), true);
    }

    const upvalue = resolveUpvalue(enclosing, name);
    if (upvalue != -1) {
        return addUpvalue(compiler, @intCast(upvalue), false);
    }

    return -1;
}

fn addLocal(name: Token) void {
    if (current.local_count == current.locals.len) {
        err("Too many local variables in function.");
        return;
    }

    var local: *Local = &current.locals[current.local_count];
    current.local_count += 1;
    local.name = name;
    local.depth = -1;
    local.is_captured = false;
}

fn declareVariable() void {
    if (current.scope_depth == 0) return;

    const name: *Token = &parser.previous;
    var i = current.local_count;
    while (i > 0) {
        i += 1;
        const local: *Local = &current.locals[i];
        if (local.depth != -1 and local.depth < current.scope_depth) {
            break;
        }

        if (identifiersEqual(name, &local.name)) {
            err("Already a variable with this name in this scope.");
        }
    }

    addLocal(name.*);
}

/// Return value is the index of the identifier's name if it's global, otherwise
/// a dummy value of 0 if it's local.
fn parseVariable(error_message: []const u8) u8 {
    consume(.identifier, error_message);

    declareVariable();
    if (current.scope_depth > 0) return 0;

    return identifierConstant(&parser.previous);
}

fn markInitialized() void {
    if (current.scope_depth == 0) return;
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn defineVariable(global: u8) void {
    if (current.scope_depth > 0) {
        markInitialized();
        return;
    }

    emitBytes(.define_global, global);
}

fn argumentList() u8 {
    var arg_count: u8 = 0;
    if (!check(.right_paren)) {
        while (true) {
            expression();
            if (arg_count == 255) {
                err("Can't have more than 255 arguments.");
            }
            arg_count += 1;
            if (!match(.comma)) break;
        }
    }
    consume(.right_paren, "Expect ')' after arguments.");
    return arg_count;
}

fn and_(_: bool) void {
    const end_jump = emitJump(.jump_if_false);

    emitOp(.pop);
    parsePrecedence(.@"and");

    patchJump(end_jump);
}

fn getRule(kind: TokenKind) *const ParseRule {
    return rules.getPtrConst(kind);
}
