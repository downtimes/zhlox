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
    values: std.StringHashMap(*interpreter.Value),
    refs: u32,

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
            .values = std.StringHashMap(*interpreter.Value).init(allocator),
            .refs = 0,
        };
        return env_place;
    }

    pub fn clean_unused(self: *Self, parents: bool) void {
        // Don't clean up the global environment, environments that still have children
        // or environments that are referenced by closures.
        if (self.parent == null or self.children.items.len != 0 or self.refs != 0) {
            return;
        }

        var it = self.values.valueIterator();
        while (it.next()) |v| {
            v.*.deinit();
        }
        self.values.deinit();
        self.children.deinit();

        // We checked that we are not the root. So it is certain that we have
        // a parent environment.
        var parent = self.parent.?;
        parent.remove_child(self);
        self.allocator.destroy(self);

        if (parents) {
            parent.clean_unused(true);
        }
    }

    pub fn deinit(self: *Self) void {
        // Deinitialize all children.
        for (self.children.items) |c| {
            c.deinit();
        }
        self.children.deinit();

        if (self.parent) |p| {
            p.remove_child(self);
        }

        // Remove our own data.
        var it = self.values.valueIterator();
        while (it.next()) |v| {
            v.*.deinit();
        }
        self.values.deinit();

        self.allocator.destroy(self);
    }

    fn remove_child(self: *Self, env: *Environment) void {
        for (0.., self.children.items) |index, c| {
            if (c == env) {
                _ = self.children.swapRemove(index);
                return;
            }
        }
    }

    pub fn define(self: *Self, name: []const u8, value: *interpreter.Value) !void {
        // We allow shadowing of variables by removing entries instead of blocking addition.
        const resp = try self.values.getOrPut(name);
        if (resp.found_existing) {
            resp.value_ptr.*.deinit();
        }
        value.ref += 1;
        resp.value_ptr.* = value;
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

    pub fn getInParent(self: *Self, steps: u16, name: []const u8) ?*interpreter.Value {
        return self.ancestor(steps).get(name);
    }

    pub fn get(self: *Self, name: []const u8) ?*interpreter.Value {
        return self.values.get(name);
    }

    pub fn assignInParent(self: *Self, steps: u16, name: []const u8, value: *interpreter.Value) EnvironmentError!void {
        return self.ancestor(steps).assign(name, value);
    }

    pub fn assign(self: *Self, name: []const u8, value: *interpreter.Value) EnvironmentError!void {
        if (self.values.getPtr(name)) |ptr| {
            ptr.*.deinit();
            value.ref += 1;
            ptr.* = value;
            return;
        }

        return EnvironmentError.VariableNotFound;
    }
};
