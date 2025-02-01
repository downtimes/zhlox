const std = @import("std");
const interpreter = @import("interpreter.zig");

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
    closure_refs: u32,

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
            .closure_refs = 0,
        };
        return env_place;
    }

    pub fn clean_unused(self: *Self) void {
        // Don't clean up the global environment, environments that still have children
        // or environments that are referenced by closures.
        if (self.parent == null or self.children.items.len != 0 or self.closure_refs != 0) {
            return;
        }

        self.remove_from_parent();

        var it = self.values.iterator();
        while (it.next()) |kv| {
            self.allocator.free(kv.key_ptr.*);
            kv.value_ptr.*.deinit(self.allocator);
        }
        self.values.deinit();

        // We checked that we are not the root. So it is certain that we have
        // a parent environment.
        var parent = self.parent.?;
        self.allocator.destroy(self);

        parent.clean_unused();
    }

    pub fn deinit(self: *Self) void {
        // Deinitialize all children.
        for (self.children.items) |c| {
            c.deinit();
        }
        self.children.deinit();

        self.remove_from_parent();

        // Remove our own data.
        var it = self.values.iterator();
        while (it.next()) |kv| {
            self.allocator.free(kv.key_ptr.*);
            kv.value_ptr.*.deinit(self.allocator);
        }
        self.values.deinit();

        self.allocator.destroy(self);
    }

    fn remove_from_parent(self: *Self) void {
        if (self.parent) |p| {
            const index = for (0.., p.children.items) |index, elem| {
                if (elem == self) break index;
            } else null;
            if (index) |i| {
                _ = p.children.swapRemove(i);
            }
        }
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

    // steps with 0 value is a valid argument. It gives back self.
    fn ancestor(self: *Self, steps: u16) *Environment {
        var env = self;
        var count = steps;
        while (count > 0) {
            // Ensure we have a parent.
            std.debug.assert(env.parent != null);
            env = env.parent.?;
            count -= 1;
        }
        // Ensure no variables of the global scope are resolved using this method.
        std.debug.assert(env.parent != null);
        return env;
    }

    pub fn getInParent(self: *Self, steps: u16, name: []const u8) ?interpreter.Value {
        return self.ancestor(steps).get(name);
    }

    pub fn get(self: *Self, name: []const u8) ?interpreter.Value {
        return self.values.get(name);
    }

    pub fn assignInParent(self: *Self, steps: u16, name: []const u8, value: interpreter.Value) EnvironmentError!void {
        return self.ancestor(steps).assign(name, value);
    }

    pub fn assign(self: *Self, name: []const u8, value: interpreter.Value) EnvironmentError!void {
        if (self.values.contains(name)) {
            const new_value = try value.clone(self.allocator);
            const value_ptr = self.values.getPtr(name).?;
            value_ptr.*.deinit(self.allocator);
            value_ptr.* = new_value;
            return;
        }

        return EnvironmentError.VariableNotFound;
    }
};
