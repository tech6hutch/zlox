// This module is not currently used

const std = @import("std");
const Allocator = std.mem.Allocator;

pub const WrappedAllocator = struct {
    child_allocator: Allocator,
    _gpa: std.heap.GeneralPurposeAllocator(.{}),

    const Self = @This();

    pub fn init() Self {
        var gpa = std.heap.GeneralPurposeAllocator(.{}){};
        return .{
            .child_allocator = gpa.allocator(),
            ._gpa = gpa,
        };
    }

    pub fn deinit(self: Self) void {
        self._gpa.deinit();
    }

    pub fn allocator(self: *Self) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    pub fn alloc(self: *Self, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        return self.child_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    pub fn resize(self: *Self, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
        return self.child_allocator.rawResize(buf, buf_align, new_len, ret_addr);
    }

    pub fn free(self: *Self, buf: []u8, buf_align: u8, ret_addr: usize) void {
        return self.child_allocator.rawFree(buf, buf_align, ret_addr);
    }

    pub fn reallocate(pointer: *void, old_size: usize, new_size: usize) ?*void {
        _ = old_size;
        if (new_size == 0) {
            free(pointer);
            return null;
        }

        const result: *void = resize(pointer, new_size)
            catch std.os.exit(1);
        return result;
    }
};
