def _impl(ctx):
    expr = ctx.actions.declare_file("Expr.java")
    ctx.actions.run(
        outputs = [expr],
        executable = ctx.executable.generator,
        arguments = [expr.dirname],
    )
    return DefaultInfo(files = depset([expr]))

generated_ast_srcs = rule(
    implementation = _impl,
    attrs = {
        "generator": attr.label(
            executable = True,
            cfg = "host",
        )
    }
)
