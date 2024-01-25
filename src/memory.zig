const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("./Vm.zig");
const values = @import("./values.zig");
const Value = values.Value;
const objects = @import("./objects.zig");
const Obj = objects.Obj;
const common = @import("./common.zig");
const markCompilerRoots = @import("./compiler.zig").markCompilerRoots;

const GC_HEAP_GROW_FACTOR = 2;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const inner_allocator = gpa.allocator();

pub const allocator: Allocator = .{
    .ptr = &gpa,
    .vtable = &.{
        .alloc = _alloc,
        .resize = _resize,
        .free = _free,
    }
};

fn _alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    const ptr = inner_allocator.vtable.alloc(ctx, len, ptr_align, ret_addr);
    reallocating(ptr, 0, len);
    return ptr;
}
fn _resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    if (inner_allocator.vtable.resize(ctx, buf, buf_align, new_len, ret_addr)) {
        reallocating(buf.ptr, buf.len, new_len);
        return true;
    } else {
        return false;
    }
}
fn _free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    reallocating(buf.ptr, buf.len, 0);
    return inner_allocator.vtable.free(ctx, buf, buf_align, ret_addr);
}

fn reallocating(ptr: ?*anyopaque, old_size: usize, new_size: usize) void {
    _ = ptr;
    Vm.vm.bytes_allocated -= old_size; // don't wanna overflow
    Vm.vm.bytes_allocated += new_size;

    if (common.DEBUG_STRESS_GC and new_size > old_size) {
        collectGarbage();
    } else if (Vm.vm.bytes_allocated > Vm.vm.next_gc) {
        collectGarbage();
    }
}

pub fn markObject(maybe_object: ?*Obj) void {
    var object = maybe_object orelse return;
    if (object.is_marked) return;

    if (common.DEBUG_LOG_GC) {
        std.debug.print("{*} mark ", .{object});
        values.print(Value.objVal(object));
        std.debug.print("\n", .{});
    }

    object.is_marked = true;

    Vm.vm.gray_stack.append(object) catch {
        std.process.exit(1);
    };
}

pub fn markValue(value: Value) void {
    if (value.isObj()) markObject(value.obj);
}

fn markArray(array: []Value) void {
    for (array) |value| {
        markValue(value);
    }
}

fn blackenObject(object: *Obj) void {
    if (common.DEBUG_LOG_GC) {
        std.debug.print("{*} blacken ", .{object});
        values.print(Value.objVal(object));
        std.debug.print("\n", .{});
    }

    switch (object.kind) {
        .class => {
            const class = object.downcast(objects.ObjClass);
            markObject(objects.upcast(class.name));
        },
        .closure => {
            const closure = object.downcast(objects.ObjClosure);
            markObject(objects.upcast(closure.function));
            for (closure.upvalues) |upvalue| {
                markObject(objects.upcast_nullable(upvalue));
            }
        },
        .function => {
            const function = object.downcast(objects.ObjFunction);
            markObject(objects.upcast_nullable(function.name));
            markArray(function.chunk.constants.items);
        },
        .instance => {
            const instance = object.downcast(objects.ObjInstance);
            markObject(objects.upcast(instance.class));
            instance.fields.markTable();
        },
        .upvalue => markValue(object.downcast(objects.ObjUpvalue).closed),
        .native,
        .string => {},
    }
}

pub fn allocate(comptime T: type, count: usize) []T {
    return allocator.alloc(T, count) catch |e| switch (e) {
        error.OutOfMemory => std.os.exit(1)
    };
}
pub fn freeArray(ptr: anytype) void {
    allocator.free(ptr);
}

pub fn create(comptime T: type) *T {
    return allocator.create(T) catch |e| switch (e) {
        error.OutOfMemory => std.os.exit(1)
    };
}
pub fn destroy(ptr: anytype) void {
    allocator.destroy(ptr);
}

var collecting_garbage = false;
fn collectGarbage() void {
    if (collecting_garbage) {
        std.debug.print("-- skip gc\n", .{});
        return;
    }
    collecting_garbage = true;

    const before = Vm.vm.bytes_allocated;
    if (common.DEBUG_LOG_GC) {
        std.debug.print("-- gc begin\n", .{});
    }

    markRoots();
    traceReferences();
    Vm.vm.strings.removeWhite();
    sweep();

    Vm.vm.next_gc = Vm.vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    if (common.DEBUG_LOG_GC) {
        std.debug.print("-- gc end\n", .{});
        std.debug.print("   collected {d} bytes (from {d} to {d}) next at {d}\n", .{
            @as(isize, @intCast(before)) - @as(isize, @intCast(Vm.vm.bytes_allocated)),
            before, Vm.vm.bytes_allocated, Vm.vm.next_gc});
    }

    collecting_garbage = false;
}

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

/// Writes a null sentinel to a string and returns it with the proper type.
pub fn nullTerminate(slice: []u8) [:0]u8 {
    slice[slice.len-1] = 0;
    return slice[0..slice.len-1:0];
}

pub fn freeObjects(vm: *Vm) void {
    var object = vm.objs;
    while (object) |o| {
        const next = o.next;
        freeObject(o);
        object = next;
    }

    Vm.vm.gray_stack.deinit();
}

pub fn freeObject(object: *Obj) void {
    if (common.DEBUG_LOG_GC) {
        std.debug.print("{*} free type {d} ({s})\n",
            .{object, @intFromEnum(object.kind), @tagName(object.kind)});
    }

    // We don't need to push/pop values here since this function is only called
    // during GC, and GC won't run when we're already GCing.
    _ = collecting_garbage; // obviously a compile error if we changed anything

    // You must downcast before passing the object to destroy(), since Zig's allocator
    // asserts that the size of the type is the same size that was allocated (and
    // obviously Obj is smaller than anything that "inherits" from it).
    switch (object.kind) {
        .class => {
            const class = object.downcast(objects.ObjClass);
            destroy(class);
        },
        .closure => {
            const closure = object.downcast(objects.ObjClosure);
            freeArray(closure.upvalues);
            destroy(closure);
        },
        .function => {
            const function = object.downcast(objects.ObjFunction);
            function.chunk.deinit();
            destroy(function);
        },
        .instance => {
            const instance = object.downcast(objects.ObjInstance);
            instance.fields.deinit();
            destroy(instance);
        },
        .native => {
            destroy(object.downcast(objects.ObjNative));
        },
        .string => {
            const string = object.downcast(objects.ObjString);
            freeArray(string.chars);
            destroy(string);
        },
        .upvalue => {
            destroy(object.downcast(objects.ObjUpvalue));
        },
    }
}

fn markRoots() void {
    var slot = Vm.vm.stack[0..].ptr;
    while (@intFromPtr(slot) < @intFromPtr(Vm.vm.stack_top)) : (slot += 1) {
        markValue(slot[0]);
    }

    for (Vm.vm.frames[0..Vm.vm.frame_count]) |*frame| {
        markObject(objects.upcast(frame.closure));
    }

    {
        var upvalue = Vm.vm.open_upvalues;
        while (upvalue != null) : (upvalue = upvalue.?.next) {
            markObject(objects.upcast_nullable(upvalue));
        }
    }

    Vm.vm.globals.markTable();
    markCompilerRoots();
}

fn traceReferences() void {
    var gray_stack = &Vm.vm.gray_stack;
    while (gray_stack.popOrNull()) |object| {
        blackenObject(object);
    }
    // TODO: why doesn't he have to do this to prevent collecting negative bytes
    // during GC? Do his dynamic arrays not allocate thru the GC?
    gray_stack.clearAndFree();
}

fn sweep() void {
    var previous: ?*Obj = null;
    var object: ?*Obj = Vm.vm.objs;
    while (object != null) {
        if (object.?.is_marked) {
            object.?.is_marked = false;
            previous = object;
            object = object.?.next;
        } else {
            const unreached: *Obj = object.?;
            object = object.?.next;
            if (previous) |prev| {
                prev.next = object;
            } else {
                Vm.vm.objs = object;
            }

            freeObject(unreached);
        }
    }
}
