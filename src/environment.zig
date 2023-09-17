const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

// TODO Not sure if the arena is the best allocation strategy here. If the Environment exists for a long time
//      with many variables being introduced and removed, this will waste a lot of memory.
pub const Environment = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    values: std.StringHashMapUnmanaged(interpreter.Value),

    pub fn deinit(self: *Self) void {
        const parent_alloc = self.arena.child_allocator;
        self.arena.deinit();
        parent_alloc.destroy(self.arena);
    }

    pub fn define(self: *Self, name: []const u8, value: interpreter.Value) !void {
        // We allow shadowing of variables by not checking if the variable exists already.
        try self.values.put(self.arena.allocator(), name, value);
    }

    pub fn get(self: Self, name: token.Token) ?interpreter.Value {
        return self.values.get(name.lexeme);
    }
};
