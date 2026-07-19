# lox

## Build with bazel

Discover all the possible targets
```bash
bazel query ...
bazel query //jlox/...
```

Run the lox tree walking interpreter
```bash
bazel run //jlox/com/craftinginterpreters/lox:lox
```

Whilst the above will work just fine, the experience is much better when using `rlwrap` to provide readline functionality
```bash
rlwrap bazel run //jlox/com/craftinginterpreters/lox:lox
```

Run a demo program
```bash
bazel run //jlox/com/craftinginterpreters/lox:lox $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

Generate `Expr.java` and `Stmt.java`, this is useful for development so that the LSP correctly picks up the variables.
```bash
bazel run //jlox/com/craftinginterpreters/tool:generate_ast $(git rev-parse --show-toplevel)/jlox/com/craftinginterpreters/lox
```

