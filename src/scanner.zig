const std = @import("std");
const token = @import("token.zig");

pub const Scanner = struct {
    input: []const u8,
    allocator: std.mem.Allocator,
    const Self = @This();

    pub fn scanTokens(self: Self) std.ArrayList(token.Token) {
        return std.ArrayList(token.Token).init(self.allocator);
    }
};
