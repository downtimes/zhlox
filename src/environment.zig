const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

pub const EnvironmentError = error{ VariableNotFound, OutOfMemory };

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
        var allocator = self.arena.allocator();
        const owned_str = try allocator.alloc(u8, name.len);
        std.mem.copyForwards(u8, owned_str, name);
        const owned_value = try allocator.create(interpreter.Value);
        owned_value.* = value;
        // We allow shadowing of variables by not checking if the variable exists already.
        try self.values.put(self.arena.allocator(), owned_str, owned_value.*);
    }

    pub fn get(self: Self, name: token.Token) ?interpreter.Value {
        return self.values.get(name.lexeme);
    }

    pub fn assign(self: *Self, name: token.Token, value: interpreter.Value) EnvironmentError!void {
        if (self.values.contains(name.lexeme)) {
            const owned_value = try self.arena.allocator().create(interpreter.Value);
            owned_value.* = value;
            try self.values.put(self.arena.allocator(), name.lexeme, owned_value.*);
            return;
        }
        return EnvironmentError.VariableNotFound;
    }
};
