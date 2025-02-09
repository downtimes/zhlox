const std = @import("std");
const token = @import("token.zig");
const parser = @import("parser.zig");

pub const Binary = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const Unary = struct {
    operator: token.Token,
    right: *Expr,
};

pub const Function = struct {
    name: token.Token,
    params: []token.Token,
    body: []Stmt,
};

pub const Call = struct {
    callee: *Expr,
    line_number: u32,
    arguments: []Expr,
};

pub const Literal = union(enum) {
    number: f64,
    string: []const u8, // The string is not owned, it points into the input_scratch
    bool_: bool,
    nil: void,
};

pub const Assignment = struct {
    variable: Variable,
    value: *Expr,
};

pub const VariableDeclaration = struct {
    name: token.Token,
    initializer: ?Expr,
};

pub const Conditional = struct {
    condition: Expr,
    then: *Stmt,
    els: ?*Stmt,
};

pub const Logical = struct {
    left: *Expr,
    operator: token.Token,
    right: *Expr,
};

pub const WhileStmt = struct {
    condition: Expr,
    body: []Stmt,
};

pub const Return = struct {
    line_number: u32,
    value: ?Expr,
};

pub const Class = struct {
    name: token.Token,
    methods: []Function,
};

pub const Stmt = union(enum) {
    expr: Expr,
    cond: Conditional,
    print: Expr,
    while_: WhileStmt,
    var_decl: VariableDeclaration,
    function: Function,
    class: Class,
    ret: Return,
    block: []Stmt,
};

pub const Variable = struct {
    name: token.Token,
    resolve_steps: ?u16,
};

pub const Get = struct {
    name: token.Token,
    object: *Expr,
};

pub const Set = struct {
    name: token.Token,
    object: *Expr,
    value: *Expr,
};

pub const Expr = union(enum) {
    binary: Binary,
    grouping: *Expr,
    literal: Literal,
    logical: Logical,
    unary: Unary,
    call: Call,
    get: Get,
    set: Set,
    variable: Variable,
    assign: Assignment,
};
