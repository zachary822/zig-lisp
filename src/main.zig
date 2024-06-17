const c = @cImport({
    @cInclude("readline/readline.h");
    @cInclude("mpc/mpc.h");
});
const std = @import("std");

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    const Number = c.mpc_new("number");
    const Operator = c.mpc_new("operator");
    const Expr = c.mpc_new("expr");
    const Lispy = c.mpc_new("lispy");

    _ = c.mpca_lang(c.MPCA_LANG_DEFAULT,
        \\ number: /-?[0-9]+/ ;
        \\ operator: '+' | '-' | '*' | '/' ;
        \\ expr: <number> | '(' <operator> <expr>+ ')' ;
        \\ lispy: /^/ <operator> <expr>+ /$/ ;
    , Number, Operator, Expr, Lispy);

    try stdout.print("Lispy Version 0.0.1\n", .{});
    try stdout.print("Press ctrl+c to Exit\n", .{});

    while (true) {
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

            const res: i64 = try eval(ast);
            try stdout.print("result: {d}\n", .{res});

            c.mpc_ast_print(ast);
            c.mpc_ast_delete(ast);
        } else {
            c.mpc_err_print(r.@"error");
            c.mpc_err_delete(r.@"error");
        }
    }

    c.mpc_cleanup(4, Number, Operator, Expr, Lispy);
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
        x = evalOp(x, op, try eval(t.*.children[i]));
        i += 1;
    }

    return x;
}

fn evalOp(x: i64, op: []const u8, y: i64) i64 {
    if (std.mem.eql(u8, op, "+")) return x + y;
    if (std.mem.eql(u8, op, "-")) return x - y;
    if (std.mem.eql(u8, op, "*")) return x * y;
    if (std.mem.eql(u8, op, "/")) return @divFloor(x, y);
    return 0;
}
