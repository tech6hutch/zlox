const std = @import("std");
const common = @import("./common.zig");
const values = @import("./values.zig");
const Value = values.Value;
const loxmem = @import("./memory.zig");
const Vm = @import("./Vm.zig");
const Chunk = @import("./Chunk.zig");

pub const Obj = struct {
    kind: ObjKind,
    is_marked: bool,
    next: ?*Obj,
    pub inline fn downcast(self: *Obj, comptime T: type) *T {
        return @fieldParentPtr(T, "obj", self);
    }
};

pub inline fn upcast(obj: anytype) *Obj {
    switch (@typeInfo(@TypeOf(obj))) {
        .Pointer => {},
        .Optional => @compileError("use upcast_nullable"),
        else => @compileError("this function expects a pointer")
    }
    return &@field(obj, "obj");
}

pub inline fn upcast_nullable(obj: anytype) ?*Obj {
    switch (@typeInfo(@TypeOf(obj))) {
        .Optional => {},
        .Pointer => @compileError("use upcast"),
        else => @compileError("this function expects a nullable pointer")
    }
    return if (obj) |obj_nn| &@field(obj_nn, "obj") else null;
}

pub const ObjKind = enum {
    closure,
    function,
    native,
    string,
    upvalue,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    upvalue_count: u8,
    chunk: Chunk,
    name: ?*ObjString,
};

pub const NativeFn = *const fn(args: []const Value) Value;

pub const ObjNative = struct {
    obj: Obj,
    function: NativeFn,
};

pub const ObjString = struct {
    obj: Obj,
    chars: [:0]u8,
    hash: u32,
    pub inline fn len(self: *ObjString) usize {
        return self.chars.len;
    }
};

pub const ObjUpvalue = struct {
    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*ObjUpvalue,
};

pub const ObjClosure = struct {
    obj: Obj,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,
};

pub fn takeString(str: [:0]u8) *ObjString {
    const hash = hashString(str);
    const maybe_interned: ?*ObjString = Vm.vm.strings.findString(str, hash);
    if (maybe_interned) |interned| {
        loxmem.freeArray(str);
        return interned;
    }

    return allocateString(str, hash);
}
pub fn copyString(str: []const u8) *ObjString {
    const hash = hashString(str);
    const maybe_interned: ?*ObjString = Vm.vm.strings.findString(str, hash);
    if (maybe_interned) |interned| return interned;

    const heapChars = loxmem.allocate(u8, str.len + 1);
    @memcpy(heapChars, str.ptr);
    return allocateString(loxmem.nullTerminate(heapChars), hash);
}

pub fn newUpvalue(slot: *Value) *ObjUpvalue {
    var upvalue: *ObjUpvalue = allocateObj(ObjUpvalue, .upvalue);
    upvalue.location = slot;
    upvalue.closed = Value.nilVal();
    upvalue.next = null;
    return upvalue;
}

pub fn printObject(value: Value) void {
    switch (value.objKind()) {
        .closure => printFunction(value.asClosure().function),
        .function => printFunction(value.obj.downcast(ObjFunction)),
        .native => std.debug.print("<native fn>", .{}),
        .string => std.debug.print("{s}", .{value.asZigString()}),
        .upvalue => std.debug.print("upvalue", .{}),
    }
}

fn printFunction(function: *ObjFunction) void {
    if (function.name) |name| {
        std.debug.print("<fn {s}>", .{name.chars});
    } else {
        std.debug.print("<script>", .{});
    }
}

fn allocateString(str: [:0]u8, hash: u32) *ObjString {
    var string: *ObjString = allocateObj(ObjString, .string);
    string.chars = str;
    string.hash = hash;

    Vm.vm.push(Value.objVal(string));
    _ = Vm.vm.strings.set(string, Value.nilVal());
    _ = Vm.vm.pop();

    return string;
}

pub fn newClosure(function: *ObjFunction) *ObjClosure {
    var upvalues = loxmem.allocate(?*ObjUpvalue, function.upvalue_count);
    for (0..function.upvalue_count) |i| {
        upvalues[i] = null;
    }

    var closure: *ObjClosure = allocateObj(ObjClosure, .closure);
    closure.function = function;
    closure.upvalues = upvalues;
    return closure;
}

pub fn newFunction() *ObjFunction {
    var function: *ObjFunction = allocateObj(ObjFunction, .function);
    function.arity = 0;
    function.upvalue_count = 0;
    function.name = null;
    function.chunk = Chunk.init(loxmem.allocator);
    return function;
}

pub fn newNative(function: NativeFn) *ObjNative {
    var native: *ObjNative = allocateObj(ObjNative, .native);
    native.function = function;
    return native;
}

/// Uses FNV-1a
fn hashString(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |char| {
        hash ^= char;
        hash *%= 16777619;
    }
    return hash;
}

fn allocateObj(comptime T: type, comptime objKind: ObjKind) *T {
    return _allocateObject(T, objKind).downcast(T);
}

fn _allocateObject(comptime T: type, objKind: ObjKind) *Obj {
    var object: *Obj = upcast(loxmem.create(T));
    object.kind = objKind;
    object.is_marked = false;

    object.next = Vm.vm.objs;
    Vm.vm.objs = object;

    if (common.DEBUG_LOG_GC) {
        std.debug.print("{*} allocate {d} for {d} ({s})\n",
            .{object, @sizeOf(T), @intFromEnum(objKind), @tagName(objKind)});
    }

    return object;
}
