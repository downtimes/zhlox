const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

// TODO reference count the environments and only delete them if the last reference goes out of scope.
//      replace function environment with one such reference.
pub const EnvironmentError = error{ VariableNotFound, OutOfMemory };

pub const Environment = struct {
    const Self = @This();

    parent: ?*Environment,
    allocator: std.mem.Allocator,
    values: std.StringHashMapUnmanaged(interpreter.Value),

    // Parent environment must be valid as long as this environment is used.
    pub fn init_with_parent(allocator: std.mem.Allocator, parent: *Environment) Self {
        var env = init(allocator);
        env.parent = parent;
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

    pub fn equals(self: Self, other: Self) bool {
        if (self.values.count() != other.values.count()) {
            return false;
        }

        var iter = self.values.iterator();
        while (iter.next()) |elem| {
            const o = other.values.getPtr(elem.key_ptr.*) orelse return false;
            if (!elem.value_ptr.*.equal(o.*)) {
                return false;
            }
        }
        return true;
    }

    pub fn clone(self: Self, allocator: std.mem.Allocator) !Self {
        var new_env = init(allocator);
        errdefer new_env.deinit();

        new_env.parent = self.parent;
        new_env.values = try self.values.clone(allocator);
        return new_env;
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
            self.allocator.free(owned_str);
            o.value.deinit(self.allocator);
        }
    }

    pub fn get(self: Self, name: token.Token) ?interpreter.Value {
        const val = self.values.get(name.lexeme);
        if (val) |v| {
            return v;
        } else if (self.parent) |parent| {
            return parent.get(name);
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
