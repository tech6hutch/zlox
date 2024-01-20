const std = @import("std");
const Allocator = std.mem.Allocator;
const Vm = @import("./Vm.zig");
const objects = @import("./objects.zig");
const Obj = objects.Obj;

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
    return inner_allocator.vtable.alloc(ctx, len, ptr_align, ret_addr);
}
fn _resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
    return inner_allocator.vtable.resize(ctx, buf, buf_align, new_len, ret_addr);
}
fn _free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    return inner_allocator.vtable.free(ctx, buf, buf_align, ret_addr);
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

// pub const WrappedAllocator = struct {
//     child_allocator: Allocator,
//     _gpa: std.heap.GeneralPurposeAllocator(.{}),

//     const Self = @This();

//     pub fn init() Self {
//         var gpa = std.heap.GeneralPurposeAllocator(.{}){};
//         return .{
//             .child_allocator = gpa.allocator(),
//             ._gpa = gpa,
//         };
//     }

//     pub fn deinit(self: Self) void {
//         self._gpa.deinit();
//     }

//     pub fn allocator(self: *Self) Allocator {
//         return .{
//             .ptr = self,
//             .vtable = &.{
//                 .alloc = alloc,
//                 .resize = resize,
//                 .free = free,
//             },
//         };
//     }

//     pub fn alloc(self: *Self, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
//         return self.child_allocator.rawAlloc(len, ptr_align, ret_addr);
//     }

//     pub fn resize(self: *Self, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
//         return self.child_allocator.rawResize(buf, buf_align, new_len, ret_addr);
//     }

//     pub fn free(self: *Self, buf: []u8, buf_align: u8, ret_addr: usize) void {
//         return self.child_allocator.rawFree(buf, buf_align, ret_addr);
//     }

//     pub fn reallocate(pointer: *void, old_size: usize, new_size: usize) ?*void {
//         _ = old_size;
//         if (new_size == 0) {
//             free(pointer);
//             return null;
//         }

//         const result: *void = resize(pointer, new_size)
//             catch std.os.exit(1);
//         return result;
//     }
// };

pub fn growCapacity(capacity: usize) usize {
    return if (capacity < 8) 8 else capacity * 2;
}

/// Writes a null sentinel to a string and returns it with the proper type.
pub fn null_terminate(slice: []u8) [:0]u8 {
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
}

pub fn freeObject(object: *Obj) void {
    switch (object.kind) {
        .function => {
            const function = object.downcast(objects.ObjFunction);
            function.chunk.deinit();
            destroy(function);
        },
        .string => {
            const string = object.downcast(objects.ObjString);
            freeArray(string.chars);
            destroy(string);
        },
    }
}
