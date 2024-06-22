const c = @cImport({
    @cInclude("signal.h");
    @cInclude("readline/readline.h");
    @cInclude("mpc/mpc.h");
});
const std = @import("std");

const Value = union(enum) {
    num: i64,
    sym: []const u8,
    lvals: std.ArrayList(*Lval),
};

const Lval = struct {
    allocator: std.mem.Allocator,
    value: Value,

    fn init(allocator: std.mem.Allocator, value: Value) !*Lval {
        const lval = try allocator.create(Lval);
        lval.* = .{ .allocator = allocator, .value = value };

        return lval;
    }

    fn deinit(self: *Lval) void {
        switch (self.value) {
            .sym => |sym| {
                self.allocator.free(sym);
            },
            .lvals => |lvals| {
                for (lvals.items) |item| {
                    item.deinit();
                }
                lvals.deinit();
            },
            .num => {},
        }
        self.allocator.destroy(self);
    }
};

test "does the Lval deinit even work" {
    const allocator = std.testing.allocator;

    const lval1 = try Lval.init(allocator, .{ .num = 10 });
    const str = try allocator.dupe(u8, "test string!");
    const lval2 = try Lval.init(allocator, .{ .sym = str });

    var arr = std.ArrayList(*Lval).init(allocator);
    try arr.append(lval1);
    try arr.append(lval2);
    const lval = try Lval.init(allocator, .{ .lvals = arr });
    lval.deinit();
}

fn sigintHandler(_: c_int) callconv(.C) void {
    _ = c.write_history(".history");
    std.process.exit(0);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    _ = c.signal(c.SIGINT, sigintHandler);

    const stdout = std.io.getStdOut().writer();

    const Number = c.mpc_new("number");
    const Symbol = c.mpc_new("symbol");
    const Expr = c.mpc_new("expr");
    const Sexpr = c.mpc_new("sexpr");
    const Lispy = c.mpc_new("lispy");

    _ = c.mpca_lang(c.MPCA_LANG_DEFAULT,
        \\ number: /-?[0-9]+/ ;
        \\ symbol: '+' | '-' | '*' | '/' ;
        \\ sexpr: '(' <expr>* ')' ;
        \\ expr: <number> | <symbol> | <sexpr> ;
        \\ lispy: /^/ <expr>* /$/ ;
    , Number, Symbol, Sexpr, Expr, Lispy);

    try stdout.print("Lispy Version 0.0.1\n", .{});
    try stdout.print("Press ctrl+c to Exit\n", .{});

    _ = c.read_history(".history");

    while (true) {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const aa = arena.allocator();

        const input = c.readline("lisp> ");
        defer std.c.free(input);

        if (input == null) {
            break;
        }
        _ = c.add_history(input);

        var r: c.mpc_result_t = undefined;
        if (c.mpc_parse("<stdin>", input, Lispy, &r) != 0) {
            const ast: [*c]c.mpc_ast_t = @alignCast(@ptrCast(r.output));

            const nnodes = numberOfNodes(ast);
            try stdout.print("number of nodes: {d}\n", .{nnodes});

            const thing = try lvalRead(aa, ast);
            // defer thing.deinit();

            try lvalPrint(stdout, thing);
            try stdout.print("\n", .{});

            if (lvalEval(thing)) |result| {
                try stdout.print("result: ", .{});
                try lvalPrint(stdout, result);
                try stdout.print("\n", .{});
            } else |err| {
                try stdout.print("error: {any}\n", .{err});
            }

            c.mpc_ast_print(ast);
            c.mpc_ast_delete(ast);
        } else {
            c.mpc_err_print(r.@"error");
            c.mpc_err_delete(r.@"error");
        }
    }

    c.mpc_cleanup(5, Number, Symbol, Sexpr, Expr, Lispy);
    _ = c.write_history(".history");
}

fn lvalEvalSexpr(v: *Lval) !*Lval {
    for (v.value.lvals.items, 0..) |item, i| {
        v.value.lvals.items[i] = try lvalEval(item);
    }

    if (v.value.lvals.items.len == 0) {
        return v;
    }

    if (v.value.lvals.items.len == 1) {
        return v.value.lvals.pop();
    }

    const f = v.value.lvals.orderedRemove(0);
    defer f.deinit();

    switch (f.value) {
        .sym => |sym| {
            return try builtinOp(v, sym);
        },
        else => {
            return LispError.SexpressionNotSymbol;
        },
    }
}

test "does the lvalEvalSexpr even work" {
    const allocator = std.testing.allocator;

    const str = try allocator.dupe(u8, "+");
    const lvalsym = try Lval.init(allocator, .{ .sym = str });
    const lval1 = try Lval.init(allocator, .{ .num = 10 });
    const lval2 = try Lval.init(allocator, .{ .num = 5 });

    var arr = std.ArrayList(*Lval).init(allocator);
    try arr.append(lvalsym);
    try arr.append(lval1);
    try arr.append(lval2);
    const lval = try Lval.init(allocator, .{ .lvals = arr });
    // defer lval.deinit(); // bad

    var arr2 = std.ArrayList(*Lval).init(allocator);
    try arr2.append(lval);
    const llval = try Lval.init(allocator, .{ .lvals = arr2 });
    defer llval.deinit();

    const result = try lvalEvalSexpr(llval);
    defer result.deinit();

    try std.testing.expectEqual(result.value.num, 15);
}

test "bad lvalEvalSexpr (/ ())" {
    const allocator = std.testing.allocator;

    const str = try allocator.dupe(u8, "/");
    const lvalsym = try Lval.init(allocator, .{ .sym = str });

    const emptyArr = std.ArrayList(*Lval).init(allocator);
    const elval = try Lval.init(allocator, .{ .lvals = emptyArr });

    var arr = std.ArrayList(*Lval).init(allocator);
    try arr.append(lvalsym);
    try arr.append(elval);
    const lval = try Lval.init(allocator, .{ .lvals = arr });

    var arr2 = std.ArrayList(*Lval).init(allocator);
    try arr2.append(lval);
    const llval = try Lval.init(allocator, .{ .lvals = arr2 });

    var arr3 = std.ArrayList(*Lval).init(allocator);
    try arr3.append(llval);
    const lllval = try Lval.init(allocator, .{ .lvals = arr3 });
    defer lllval.deinit();

    try std.testing.expectError(LispError.NumberOnly, lvalEvalSexpr(lllval));
}

fn lvalEval(lval: *Lval) anyerror!*Lval {
    switch (lval.value) {
        .lvals => return lvalEvalSexpr(lval),
        else => return lval,
    }
}

fn builtinOp(a: *Lval, op: []const u8) !*Lval {
    for (a.value.lvals.items) |u| {
        switch (u.value) {
            .num => continue,
            else => {
                return LispError.NumberOnly;
            },
        }
    }

    const x = a.value.lvals.orderedRemove(0);
    errdefer x.deinit();

    if (std.mem.eql(u8, op, "-") and a.value.lvals.items.len == 0) {
        x.value.num = -x.value.num;
    }

    while (a.value.lvals.items.len > 0) {
        const y = a.value.lvals.orderedRemove(0);
        defer y.deinit();

        if (std.mem.eql(u8, op, "+")) {
            x.value.num += y.value.num;
        }
        if (std.mem.eql(u8, op, "-")) {
            x.value.num -= y.value.num;
        }
        if (std.mem.eql(u8, op, "*")) {
            x.value.num *= y.value.num;
        }
        if (std.mem.eql(u8, op, "/")) {
            if (y.value.num == 0) {
                return LispError.DivideByZero;
            }
            x.value.num = @divFloor(x.value.num, y.value.num);
        }
    }

    return x;
}

test "builtinOp +" {
    const allocator = std.testing.allocator;

    var arr = std.ArrayList(*Lval).init(allocator);
    const num1 = try Lval.init(allocator, .{ .num = 10 });
    const num2 = try Lval.init(allocator, .{ .num = 5 });
    try arr.append(num1);
    try arr.append(num2);
    const lval = try Lval.init(allocator, .{ .lvals = arr });
    defer lval.deinit();

    const result = try builtinOp(lval, "+");
    defer result.deinit();
    try std.testing.expectEqual(result.value.num, 15);
}

test "builtinOp / 0" {
    const allocator = std.testing.allocator;
    var arr = std.ArrayList(*Lval).init(allocator);
    const num1 = try Lval.init(allocator, .{ .num = 10 });
    const num2 = try Lval.init(allocator, .{ .num = 0 });
    try arr.append(num1);
    try arr.append(num2);
    const lval = try Lval.init(allocator, .{ .lvals = arr });
    defer lval.deinit();

    try std.testing.expectError(LispError.DivideByZero, builtinOp(lval, "/"));
}

test "builtinOp / ()" {
    const allocator = std.testing.allocator;

    const emptyArr = std.ArrayList(*Lval).init(allocator);
    const elval = try Lval.init(allocator, .{ .lvals = emptyArr });

    var arr = std.ArrayList(*Lval).init(allocator);
    try arr.append(elval);
    const lval = try Lval.init(allocator, .{ .lvals = arr });
    defer lval.deinit();

    try std.testing.expectError(LispError.NumberOnly, builtinOp(lval, "/"));
}

fn lvalPrint(out: std.fs.File.Writer, lvar: *Lval) anyerror!void {
    switch (lvar.value) {
        .num => |num| try out.print("{d}", .{num}),
        .sym => |sym| try out.print("{s}", .{sym}),
        .lvals => try lvalExprPrint(out, lvar, '(', ')'),
    }
}

fn lvalExprPrint(out: std.fs.File.Writer, lvar: *Lval, open: u8, close: u8) anyerror!void {
    try out.print("{c}", .{open});

    for (lvar.value.lvals.items, 0..) |lval, i| {
        try lvalPrint(out, lval);
        if (i + 1 != lvar.value.lvals.items.len) {
            try out.print(" ", .{});
        }
    }

    try out.print("{c}", .{close});
}

fn lvalReadNum(allocator: std.mem.Allocator, t: [*c]c.mpc_ast_t) !*Lval {
    const num = try std.fmt.parseInt(i64, std.mem.span(t.*.contents), 10);

    const lval = Lval.init(allocator, .{ .num = num });

    return lval;
}

test "lvalReadNum ok" {
    const allocator = std.testing.allocator;

    const content = [_:0]u8{ '1', '0' };

    const ast: c.mpc_ast_t = .{ .contents = @constCast(@ptrCast(&content)) };

    const thing = try lvalReadNum(allocator, @constCast(@ptrCast(&ast)));
    defer thing.deinit();

    try std.testing.expectEqual(10, thing.value.num);
}

test "lvalReadNum bad" {
    const allocator = std.testing.allocator;

    const content = [_:0]u8{ 'c', 'a', 't' };

    const ast: c.mpc_ast_t = .{ .contents = @constCast(@ptrCast(&content)) };

    const result = lvalReadNum(allocator, @constCast(@ptrCast(&ast)));
    try std.testing.expectError(error.InvalidCharacter, result);
}

fn lvalSexpr(allocator: std.mem.Allocator) !*Lval {
    const lvals = std.ArrayList(*Lval).init(allocator);
    const lval = try Lval.init(allocator, .{ .lvals = lvals });
    return lval;
}

fn lvalRead(allocator: std.mem.Allocator, t: [*c]c.mpc_ast_t) !*Lval {
    const tag = std.mem.span(t.*.tag);
    if (std.mem.containsAtLeast(u8, tag, 1, "number")) {
        return lvalReadNum(allocator, t);
    }
    if (std.mem.containsAtLeast(u8, tag, 1, "symbol")) {
        const sym = try allocator.dupe(u8, std.mem.span(t.*.contents));
        errdefer allocator.free(sym);

        const lval = try Lval.init(allocator, .{ .sym = sym });

        return lval;
    }

    var x: *Lval = undefined;

    if (std.mem.eql(u8, tag, ">")) {
        x = try lvalSexpr(allocator);
    }
    if (std.mem.containsAtLeast(u8, tag, 1, "sexpr")) {
        x = try lvalSexpr(allocator);
    }

    for (0..@intCast(t.*.children_num)) |i| {
        const contents = std.mem.span(t.*.children[i].*.contents);
        const tagc = std.mem.span(t.*.children[i].*.tag);

        if (std.mem.eql(u8, contents, "(")) continue;
        if (std.mem.eql(u8, contents, ")")) continue;
        if (std.mem.eql(u8, tagc, "regex")) continue;

        const v = try lvalRead(allocator, t.*.children[i]);
        try x.value.lvals.append(v);
    }

    return x;
}

fn numberOfNodes(t: [*c]c.mpc_ast_t) i64 {
    if (t.*.children_num == 0) {
        return 1;
    }

    if (t.*.children_num > 0) {
        var total: i64 = 1;

        for (0..@intCast(t.*.children_num)) |i| {
            total += numberOfNodes(t.*.children[i]);
        }

        return total;
    }
    return 0;
}

fn eval(t: [*c]c.mpc_ast_t) !i64 {
    if (std.mem.containsAtLeast(u8, std.mem.span(t.*.tag), 1, "number")) {
        return try std.fmt.parseInt(i64, std.mem.span(t.*.contents), 10);
    }

    const op: []const u8 = std.mem.span(t.*.children[1].*.contents);

    var x = try eval(t.*.children[2]);

    var i: usize = 3;

    while (std.mem.containsAtLeast(u8, std.mem.span(t.*.children[i].*.tag), 1, "expr")) {
        x = try evalOp(x, op, try eval(t.*.children[i]));
        i += 1;
    }

    return x;
}

fn evalOp(x: i64, op: []const u8, y: i64) LispError!i64 {
    if (std.mem.eql(u8, op, "+")) return x + y;
    if (std.mem.eql(u8, op, "-")) return x - y;
    if (std.mem.eql(u8, op, "*")) return x * y;
    if (std.mem.eql(u8, op, "/")) {
        if (y == 0) return LispError.DivideByZero;
        return @divFloor(x, y);
    }
    return LispError.BadOp;
}

const LispError = error{
    SexpressionNotSymbol,
    DivideByZero,
    BadOp,
    NumberOnly,
};
