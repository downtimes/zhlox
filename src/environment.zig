const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

pub const EnvironmentError = error{ VariableNotFound, OutOfMemory };

pub const Environment = struct {
    const Self = @This();

    parent: ?*Environment,
    allocator: std.mem.Allocator,
    values: std.StringHashMapUnmanaged(interpreter.Value),

    // Parent environment must be valid as long as this environment is used.
    pub fn init_with_parent(allocator: std.mem.Allocator, parent: *Environment) Self {
        const env = Environment{
            .parent = parent,
            .allocator = allocator,
            .values = std.StringHashMapUnmanaged(interpreter.Value){},
        };
        return env;
    }

    pub fn init(allocator: std.mem.Allocator) Self {
        const env = Environment{
            .parent = null,
            .allocator = allocator,
            .values = std.StringHashMapUnmanaged(interpreter.Value){},
        };
        return env;
    }

    pub fn deinit(self: *Self) void {
        var iter = self.values.iterator();
        while (iter.next()) |elem| {
            self.allocator.free(elem.key_ptr.*);
            elem.value_ptr.*.deinit(self.allocator);
        }
        self.values.deinit(self.allocator);
    }

    pub fn define(self: *Self, name: []const u8, value: interpreter.Value) !void {
        const owned_str = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_str);
        var owned_value = try value.clone(self.allocator);
        errdefer owned_value.deinit(self.allocator);

        // We allow shadowing of variables by removing entries instead of blocking addition.
        var old = try self.values.fetchPut(self.allocator, owned_str, owned_value);
        if (old) |*o| {
            self.allocator.free(o.key);
            o.value.deinit(self.allocator);
        }
    }

    pub fn get(self: Self, name: token.Token, outside_allocator: std.mem.Allocator) ?interpreter.Value {
        const val = self.values.get(name.lexeme);
        if (val) |v| {
            return v.clone(outside_allocator) catch {
                return null;
            };
        } else if (self.parent) |parent| {
            return parent.get(name, outside_allocator);
        }

        return null;
    }

    pub fn assign(self: *Self, name: token.Token, value: interpreter.Value) EnvironmentError!void {
        if (self.values.contains(name.lexeme)) {
            const new_value = try value.clone(self.allocator);
            const value_ptr = self.values.getPtr(name.lexeme).?;
            value_ptr.*.deinit(self.allocator);
            value_ptr.* = new_value;
            return;
        } else if (self.parent) |parent| {
            return parent.assign(name, value);
        }

        return EnvironmentError.VariableNotFound;
    }
};
