const std = @import("std");
const interpreter = @import("interpreter.zig");
const token = @import("token.zig");

// TODO reference count the environments and only delete them if the last reference goes out of scope.
//      replace function environment with one such reference.
pub const EnvironmentError = error{ VariableNotFound, OutOfMemory };

// Convention is that all environments are allocated in the global allocator
// and can therefore live how ever long they should live. Therefore, instead of cloning
// them you can simply point to any environment at any time.
// During runtime the environments build a tree structure with the global environment of
// the interpreter being the root of the tree. When the root deinit is called, all other
// environments are cleaned up as well.
pub const Environment = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    parent: ?*Environment,
    children: std.ArrayList(*Environment),
    values: std.StringHashMap(interpreter.Value),

    // Parent environment must be valid as long as this environment is used.
    pub fn create_with_parent(allocator: std.mem.Allocator, parent: *Environment) !*Self {
        var env = try create(allocator);
        env.parent = parent;
        try parent.children.append(env);
        return env;
    }

    pub fn create(allocator: std.mem.Allocator) !*Self {
        const env_place = try allocator.create(Self);
        env_place.* = Environment{
            .allocator = allocator,
            .parent = null,
            .children = std.ArrayList(*Environment).init(allocator),
            .values = std.StringHashMap(interpreter.Value).init(allocator),
        };
        return env_place;
    }

    pub fn deinit(self: *Self) void {
        // Deinitialize all children.
        for (self.children.items) |c| {
            c.deinit();
        }
        self.children.deinit();

        // Make sure to remove ourself from our parent node.
        if (self.parent) |p| {
            const index = for (0.., p.children.items) |index, elem| {
                if (elem == self) break index;
            } else null;
            if (index) |i| {
                _ = p.children.swapRemove(i);
            }
        }

        // Remove our own data.
        var iter = self.values.iterator();
        while (iter.next()) |elem| {
            self.allocator.free(elem.key_ptr.*);
            elem.value_ptr.*.deinit(self.allocator);
        }
        self.values.deinit();

        self.allocator.destroy(self);
    }

    pub fn define(self: *Self, name: []const u8, value: interpreter.Value) !void {
        const owned_str = try self.allocator.dupe(u8, name);
        errdefer self.allocator.free(owned_str);
        var owned_value = try value.clone(self.allocator);
        errdefer owned_value.deinit(self.allocator);

        // We allow shadowing of variables by removing entries instead of blocking addition.
        var old = try self.values.fetchPut(owned_str, owned_value);
        if (old) |*o| {
            // The hashmap does use the old key so we have to free the new key.
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
