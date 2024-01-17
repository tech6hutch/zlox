const std = @import("std");
const common = @import("./common.zig");
const values = @import("./values.zig");
const Value = values.Value;
const loxmem = @import("./memory.zig");
const Vm = @import("./Vm.zig");

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
    string,
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
    return allocateString(str, hash);
}
pub fn copyString(str: []const u8) *ObjString {
    const hash = hashString(str);
    const heapChars = loxmem.allocate(u8, str.len + 1);
    @memcpy(heapChars, str.ptr);
    return allocateString(loxmem.null_terminate(heapChars), hash);
}

pub fn printObject(value: Value) void {
    switch (value.objKind()) {
        .string => std.debug.print("{s}", .{value.asZigString()}),
    }
}

fn allocateString(str: [:0]u8, hash: u32) *ObjString {
    var string: *ObjString = allocateObj(ObjString, .string);
    string.chars = str;
    string.hash = hash;
    return string;
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
