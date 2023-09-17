const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

pub const EnvironmentError = error{ VariableNotFound, OutOfMemory };

// TODO Not sure if the arena is the best allocation strategy here. If the Environment exists for a long time
//      with many variables being introduced, this will waste a lot of memory.
pub const Environment = struct {
    const Self = @This();

    arena: *std.heap.ArenaAllocator,
    values: std.StringHashMapUnmanaged(interpreter.Value),

    pub fn init(allocator: std.mem.Allocator) !Self {
        const env = Environment{
            .arena = try allocator.create(std.heap.ArenaAllocator),
            .values = std.StringHashMapUnmanaged(interpreter.Value){},
        };
        env.arena.* = std.heap.ArenaAllocator.init(allocator);
        return env;
    }

    pub fn deinit(self: *Self) void {
        const parent_alloc = self.arena.child_allocator;
        self.arena.deinit();
        parent_alloc.destroy(self.arena);
    }

    pub fn define(self: *Self, name: []const u8, value: interpreter.Value) !void {
        const allocator = self.arena.allocator();

        const owned_str = try allocator.dupe(u8, name);

        const owned_value = try allocator.create(interpreter.Value);
        owned_value.* = try value.deepCopy(allocator);

        // We allow shadowing of variables by not checking if the variable exists already.
        try self.values.put(self.arena.allocator(), owned_str, owned_value.*);
    }

    pub fn get(self: Self, name: token.Token, outside_allocator: std.mem.Allocator) ?interpreter.Value {
        const val = self.values.get(name.lexeme);
        if (val == null) {
            return null;
        }

        return val.?.deepCopy(outside_allocator) catch {
            return null;
        };
    }

    pub fn assign(self: *Self, name: token.Token, value: interpreter.Value) EnvironmentError!void {
        if (self.values.contains(name.lexeme)) {
            const allocator = self.arena.allocator();
            const new_value = try value.deepCopy(allocator);
            var entry = self.values.getEntry(name.lexeme).?;
            entry.value_ptr.* = new_value;
            return;
        }
        return EnvironmentError.VariableNotFound;
    }
};
