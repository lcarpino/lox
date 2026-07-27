# Lox

This is a mono-repo for my implementations of the Lox programming language, following the wonderful Crafting
Interpreters book by Bob Nystrom. So far it includes two versions of the tree-walking interpreter, a faithful
implementation of the official Java version in the book and a more functionally pure implementation in Clojure.

## jlox

My implementation of `jlox` makes use of `bazel` as the build system, which must be installed before `jlox` can be
built. `bazel` makes toolchains that involve code generation easy to define and orchestrate, as a result the generated
files `Expr.java` and `Stmt.java` are not committed to the repo and instead are transitive dependencies in the build
chain of the full `jlox` interpreter.

### Building `jlox` with `bazel`

Discover all the possible build targets.

```bash
bazel query ...
bazel query //jlox/...
```

Build the interpreter as a standalone jar that can be used for running the test suite or deployed elsewhere.

```bash
bazel run //jlox/com/craftinginterpreters/lox:lox_deploy.jar
```

Run the lox tree walking interpreter directly, this will default to interactive mode. **Note**: `bazel` runs in a
sandbox, any paths to programs _must_ be passed as _absolute paths_ or the interpreter will crash with a no such file
exception. On Linux generally it is sufficient to just wrap the path with `$(realpath [RELATIVE-PATH])` and use this as
the input.

```bash
bazel run //jlox/com/craftinginterpreters/lox:lox
```

Whilst the above will work just fine, the experience is much better when using `rlwrap` to provide readline
functionality

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

## cljlox

### Building `cljlox` with `lein`

Build the interpreter as a standalone jar

```bash
lein uberjar
```

Run a demo program

```bash
lein run $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

### Building `cljlox` as a native application using `GraalVM`

Using GraalVM is arguably a little bit overkill for this project. But, given that, for relatively short running Lox
 programs the wall-clock time is dominated by JVM startup and optimising `clojure.core` bytecode, compiling the
 interpreter actually makes quite a lot of sense. Although to be completely candid, there is something about seeing the
 entire Lox test suite run and pass in only a few seconds that is very satisfying.

```bash
lein uberjar \
&& mkdir -p $(git rev-parse --show-toplevel)/target/graalvm \
&& native-image \
  -jar target/uberjar/cljlox-0.1.0-SNAPSHOT-standalone.jar \
  --no-fallback \
  --initialize-at-build-time \
  -H:Name=cljlox \
  -o target/graalvm/cljlox
```

## Running the official Lox test suite

Make sure the dart is installed by following the [official instructions](https://dart.dev/get-dart#install) to set up
the appropriate apt repository. The most up-to-date version of the dart sdk is not compatible with the official test
suite

```bash
sudo apt-get update && sudo apt-get install dart=2.19.6-1 && sudo apt-mark hold dart
```

```bash
dart tool/bin/test.dart jlox --interpreter ../lox/scripts/cljlox.sh
```
