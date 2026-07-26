# lox

## Build jlox with bazel

Discover all the possible targets
```bash
bazel query ...
bazel query //jlox/...
```

Build the interpreter as a standalone jar
```bash
bazel run //jlox/com/craftinginterpreters/lox:lox_deploy.jar
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

## Build cljlox with lein

Build the interpreter as a standalone jar
```bash
lein uberjar
```

Run a demo program
```bash
lein run $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

## Running the official Lox test suite

Make sure the dart is installed by following the [official instructions](https://dart.dev/get-dart#install). The most up-to-date version is not compatible with the official test suite
```bash
sudo apt-get update && sudo apt-get install dart=2.19.6-1 && sudo apt-mark hold dart
```
