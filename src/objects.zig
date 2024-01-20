const std = @import("std");
const common = @import("./common.zig");
const values = @import("./values.zig");
const Value = values.Value;
const loxmem = @import("./memory.zig");
const Vm = @import("./Vm.zig");
const Chunk = @import("./Chunk.zig");

pub const Obj = struct {
    kind: ObjKind,
    next: ?*Obj,
    pub inline fn downcast(self: *Obj, comptime T: type) *T {
        return @fieldParentPtr(T, "obj", self);
    }
};

pub inline fn upcast(comptime T: type, obj: *T) *Obj {
    return &@field(obj, "obj");
    // return switch (T) {
    //     inline else => @field(obj, "obj")
    //     // else => @compileError("wrong type (or you need to add it)")
    // };
}

pub const ObjKind = enum {
    function,
    string,
};

pub const ObjFunction = struct {
    obj: Obj,
    arity: u8,
    chunk: Chunk,
    name: ?*ObjString,
};

pub const ObjString = struct {
    obj: Obj,
    chars: [:0]u8,
    hash: u32,
    pub inline fn len(self: *ObjString) usize {
        return self.chars.len;
    }
    pub inline fn cast_into_base(self: *ObjString) *Obj {
        return self.obj;
    }
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
    return allocateString(loxmem.null_terminate(heapChars), hash);
}

pub fn printObject(value: Value) void {
    switch (value.objKind()) {
        .function => printFunction(value.obj.downcast(ObjFunction)),
        .string => std.debug.print("{s}", .{value.asZigString()}),
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
    _ = Vm.vm.strings.set(string, Value.nilVal());
    return string;
}

pub fn newFunction() *ObjFunction {
    var function: *ObjFunction = allocateObj(ObjFunction, .function);
    function.arity = 0;
    function.name = null;
    function.chunk = Chunk.init(loxmem.allocator);
    return function;
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

fn allocateObj(comptime T: type, objKind: ObjKind) *T {
    return _allocateObject(T, objKind).downcast(T);
}

fn _allocateObject(comptime T: type, objKind: ObjKind) *Obj {
    var object: *Obj = upcast(T, loxmem.create(T));
    object.kind = objKind;
    object.next = Vm.vm.objs;
    Vm.vm.objs = object;
    return object;
}
