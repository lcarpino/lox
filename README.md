# lox

## Build with bazel

See all the possible targets
```bash
bazel query ...
bazel query //jlox/...
```

Run the lox tree walking interpreter
```bash
bazel run //jlox/com/craftinginterpreters/lox:lox
```

Generate `Expr.java` and `Stmt.java`, this is useful for development so that 
the LSP correctly picks up the variables.
```bash
bazel run //jlox/com/craftinginterpreters/tool:generate_ast $(git rev-parse --show-toplevel)/jlox/com/craftinginterpreters/lox
```

