const std = @import("std");
const objects = @import("./objects.zig");
const Obj = objects.Obj;
const ObjKind = objects.ObjKind;
const printObject = objects.printObject;

const ValueKind = enum {
    bool,
    nil,
    number,
    obj,
};

pub const Value = union(ValueKind) {
    bool: bool,
    nil: f64,
    number: f64,
    obj: *Obj,

    pub fn boolVal(value: bool) Value {
        return .{ .bool = value };
    }
    pub fn nilVal() Value {
        return .{ .nil = 0 };
    }
    pub fn numberVal(value: f64) Value {
        return .{ .number = value };
    }
    pub fn objVal(comptime T: type, object: *T) Value {
        return .{ .obj = objects.upcast(T, object) };
    }

    pub inline fn isBool(self: Value) bool {
        return self == Value.bool;
    }
    pub inline fn isNil(self: Value) bool {
        return self == Value.nil;
    }
    pub inline fn isNumber(self: Value) bool {
        return self == Value.number;
    }
    pub inline fn isObj(self: Value) bool {
        return self == Value.obj;
    }
    pub inline fn isFunction(self: Value) bool {
        return self.isObjKind(.function);
    }
    pub inline fn isString(self: Value) bool {
        return self.isObjKind(.string);
    }
    pub inline fn isObjKind(self: Value, obj_kind: ObjKind) bool {
        return self.isObj() and self.objKind() == obj_kind;
    }

    pub inline fn asFunction(self: Value) *objects.ObjFunction {
        return self.obj.downcast(objects.ObjFunction);
    }
    pub inline fn asString(self: Value) *objects.ObjString {
        return self.obj.downcast(objects.ObjString);
    }
    pub inline fn asZigString(self: Value) [:0]const u8 {
        return self.asString().chars;
    }

    pub inline fn kind(self: Value) ValueKind {
        return @as(ValueKind, self);
    }
    pub inline fn objKind(self: Value) ObjKind {
        return self.obj.kind;
    }
};

pub const Array = std.ArrayList(Value);

pub fn print(value: Value) void {
    switch (value) {
        .bool => |b| std.debug.print("{s}", .{if (b) "true" else "false"}),
        .nil => std.debug.print("nil", .{}),
        .number => |n| std.debug.print("{d}", .{n}),
        .obj => printObject(value),
    }
}

pub fn equal(a: Value, b: Value) bool {
    if (a.kind() != b.kind()) return false;
    return switch (a) {
        .bool => a.bool == b.bool,
        .nil => true,
        .number => a.number == b.number,
        .obj => a.obj == b.obj,
    };
}
