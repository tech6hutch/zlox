const std = @import("std");
const common = @import("/common.zig");
const values = @import("./values.zig");
const Value = values.Value;
const objects = @import("./objects.zig");
const Obj = objects.Obj;
const ObjString = objects.ObjString;
const loxmem = @import("./memory.zig");

const TABLE_MAX_LOAD = 0.75;

const Table = @This();

/// The number of entries in use, plus tombstones. `count <= entries.len`
count: usize = 0,
/// All allocated entries, including uninitialized
entries: ?[]Entry = null,

inline fn capacity(self: *Table) usize {
    return if (self.entries) |entries| entries.len else 0;
}

pub fn init() Table {
    return .{};
}
pub fn deinit(self: *Table) void {
    if (self.entries) |entries| {
        loxmem.freeArray(entries);
    }
}

pub fn get(self: *Table, key: *ObjString, value: *Value) bool {
    if (self.count == 0) return false;

    const entry: *Entry = findEntry(self.entries, key);
    if (entry.key == null) return false;

    value.* = entry.value;
    return true;
}

pub fn set(self: *Table, key: *ObjString, value: Value) bool {
    const current_load = @as(f64, @floatFromInt(self.capacity())) * TABLE_MAX_LOAD;
    if (@as(f64, @floatFromInt(self.count + 1)) > current_load) {
        self.adjustCapacity(loxmem.growCapacity(self.capacity()));
    }
    var entry: *Entry = findEntry(self.entries.?, key);
    const is_new_key = entry.key == null;
    if (is_new_key and entry.value.isNil()) self.count += 1;

    entry.key = key;
    entry.value = value;
    return is_new_key;
}

pub fn delete(self: *Table, key: *ObjString) bool {
    if (self.count == 0) return false;

    // Find the entry.
    const entry: *Entry = findEntry(self.entries, key);
    if (entry.key == null) return false;

    // Place a tombstone in the entry.
    entry.key = null;
    entry.value = Value.boolVal(true);
    return true;
}

pub fn addAll(self: *Table, from: *Table) void {
    if (from.entries == null) return;
    for (from.entries.?) |*entry| {
        if (entry.key != null) {
            self.set(entry.key, entry.value);
        }
    }
}

pub fn findString(self: *Table, str: []const u8, hash: u32) ?*ObjString {
    if (self.count == 0) return null;

    var index: usize = hash % self.capacity();
    while (true) {
        const entry: *Entry = &self.entries.?[index];
        if (entry.key) |key| {
            if (key.len() == str.len and
                    key.hash == hash and
                    std.mem.eql(u8, key.chars, str)) {
                // We found it.
                return entry.key;
            }
        } else if (entry.value.isNil()) {
            // Stop if we find an empty non-tombstone entry.
            return null;
        }

        index = (index + 1) % self.capacity();
    }
}

fn adjustCapacity(self: *Table, new_capacity: usize) void {
    const new_entries: []Entry = loxmem.allocate(Entry, new_capacity);
    for (new_entries) |*entry| {
        entry.key = null;
        entry.value = Value.nilVal();
    }

    self.count = 0;
    if (self.entries) |old_entries| {
        for (old_entries) |*old_entry| {
            if (old_entry.key == null) continue;
            const new_entry = findEntry(new_entries, old_entry.key.?);
            new_entry.key = old_entry.key;
            new_entry.value = old_entry.value;
            self.count += 1;
        }
        loxmem.freeArray(old_entries);
    }

    self.entries = new_entries;
}

/// Entries with a null key are sentinel values:
/// - It's empty if its value is nil.
/// - It's a tombstone if its value is true.
pub const Entry = struct {
    key: ?*ObjString,
    value: Value,
};

fn findEntry(entries: []Entry, key: *ObjString) *Entry {
    var index: usize = key.hash % entries.len;
    var tombstone: ?*Entry = null;
    while (true) {
        const entry: *Entry = &entries[index];
        if (entry.key == null) {
            if (entry.value.isNil()) {
                // Empty entry.
                return tombstone orelse entry;
            } else {
                // We found a tombstone.
                if (tombstone == null) tombstone = entry;
            }
        } else if (entry.key == key) {
            // We found the key.
            return entry;
        }

        index = (index + 1) % entries.len;
    }
}
