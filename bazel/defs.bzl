def _impl(ctx):
    expr = ctx.actions.declare_file("Expr.java")
    stmt = ctx.actions.declare_file("Stmt.java")
    ctx.actions.run(
        outputs = [expr, stmt],
        executable = ctx.executable.generator,
        arguments = [expr.dirname],
    )
    return DefaultInfo(files = depset([expr, stmt]))

generated_ast_srcs = rule(
    implementation = _impl,
    attrs = {
        "generator": attr.label(
            executable = True,
            cfg = "host",
        )
    }
)
