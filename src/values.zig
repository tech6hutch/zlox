const std = @import("std");
const objects = @import("./objects.zig");
const Obj = objects.Obj;
const ObjKind = objects.ObjKind;
const printObject = objects.printObject;
const StaticType = @import("compiler.zig").StaticType;

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
    pub fn objVal(object: anytype) Value {
        return .{ .obj = if (@TypeOf(object.*) == Obj) object else objects.upcast(object) };
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
    pub inline fn isBoundMethod(self: Value) bool {
        return self.isObjKind(.boundMethod);
    }
    pub inline fn isClass(self: Value) bool {
        return self.isObjKind(.class);
    }
    pub inline fn isClosure(self: Value) bool {
        return self.isObjKind(.closure);
    }
    pub inline fn isFunction(self: Value) bool {
        return self.isObjKind(.function);
    }
    pub inline fn isInstance(self: Value) bool {
        return self.isObjKind(.instance);
    }
    pub inline fn isNative(self: Value) bool {
        return self.isObjKind(.native);
    }
    pub inline fn isString(self: Value) bool {
        return self.isObjKind(.string);
    }
    pub inline fn isObjKind(self: Value, obj_kind: ObjKind) bool {
        return self.isObj() and self.objKind() == obj_kind;
    }

    pub inline fn asBoundMethod(self: Value) *objects.ObjBoundMethod {
        return self.obj.downcast(objects.ObjBoundMethod);
    }
    pub inline fn asClass(self: Value) *objects.ObjClass {
        return self.obj.downcast(objects.ObjClass);
    }
    pub inline fn asClosure(self: Value) *objects.ObjClosure {
        return self.obj.downcast(objects.ObjClosure);
    }
    pub inline fn asFunction(self: Value) *objects.ObjFunction {
        return self.obj.downcast(objects.ObjFunction);
    }
    pub inline fn asInstance(self: Value) *objects.ObjInstance {
        return self.obj.downcast(objects.ObjInstance);
    }
    pub inline fn asNative(self: Value) objects.NativeFn {
        return self.obj.downcast(objects.ObjNative).function;
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
    pub fn runtimeType(self: Value) StaticType {
        if (self.isNil()) return .unknown;
        if (self.isBool()) return .bool;
        if (self.isNumber()) return .float;
        if (self.isString()) return .string;
        if (self.isObj()) return .object;
        unreachable;
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
