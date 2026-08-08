# Lox

This is a mono-repo for my implementations of the Lox programming language, following the wonderful [Crafting
Interpreters](https://craftinginterpreters.com/) book by [Bob Nystrom](https://stuffwithstuff.com/). So far it includes
two versions of the tree-walking interpreter, a faithful implementation of the official Java version in the book and a
more functionally pure implementation in Clojure.

## jlox

My implementation of `jlox` makes use of `bazel` as the build system, which must be installed before `jlox` can be
built. `bazel` makes toolchains that involve code generation easy to define and orchestrate, as a result the generated
files `Expr.java` and `Stmt.java` are not committed to the repo and instead are transitive dependencies in the build
chain of the full `jlox` interpreter. This is essentially the exact implementation from the book.

### Building `jlox` with `bazel`

Discover all the possible build targets.

```bash
bazelisk query ...
bazelisk query //jlox/...
```

Build the interpreter as a standalone jar that can be used for running the test suite or deployed elsewhere.

```bash
bazelisk run //jlox/com/craftinginterpreters/lox:lox_deploy.jar
```

Run the lox tree walking interpreter directly, this will default to interactive mode. **Note**: `bazel` runs in a
sandbox, any paths to programs _must_ be passed as _absolute paths_ or the interpreter will crash with a no such file
exception. On Linux generally it is sufficient to just wrap the path with `$(realpath [RELATIVE-PATH])` and use this as
the input.

```bash
bazelisk run //jlox/com/craftinginterpreters/lox:lox
```

Whilst the above will work just fine, the experience is much better when using `rlwrap` to provide readline
functionality

```bash
rlwrap bazelisk run //jlox/com/craftinginterpreters/lox:lox
```
Run a demo program

```bash
bazelisk run //jlox/com/craftinginterpreters/lox:lox $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

Generate `Expr.java` and `Stmt.java`, this is useful for development so that the LSP correctly picks up the variables.
```bash
bazelisk run //jlox/com/craftinginterpreters/tool:generate_ast $(git rev-parse --show-toplevel)/jlox/com/craftinginterpreters/lox
```

## cljlox

`cljlox` is a complete implementation of `Lox` language that passes all of the test suite, but written in a completely
different paradigm to `jlox`. `cljlox` completely eschews mutation, adopting a data-flow style of programming where all
of the functions are completely pure and we make sure of plain data structures rather than objects. Note that, because
`Lox` itself is a language that makes heavy use of mutation we have to make one concession to impurity in our memory
store so that it is possible to mutate values pointed to by references. We use `deps.edn` and [clojure
tools](https://github.com/clojure/brew-install) to enable the usual clojure development experience whist providing bazel
targets as well.

### Prerequisites

To build and run `cljlox`, you must have Clojure tools installed. On Linux, you can install it using:

```bash
curl -fsSL https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh | sudo bash
```

Major changes compared to `jlox`:

- Removed the use of exceptions for parser synchronisation and return statements, instead all of this is handled
  explicitly as part of the current state and threaded through all of the call stack.
- Replaced the visitor pattern with multimethods.
- All state is held in as plain data using Clojure nested maps. We define explicit schemas using Malli to make sure that
  data remains in a valid state as it is passed through our interpreter.
- Separated the `jlox` resolver into a separate analyser and resolver. This makes the interpreter more explicit at the
  cost of having to walk the tree an extra time on each pass.
- We are using a manual memory model where we have immutable addresses that point to mutable memory locations. For
  simplicity this mutable memory is a Clojure vector which is appended to each time new variables are created.

One of the caveats with the current approach is that there is no garbage collection, so our virtual heap will continue
to grow without limit. I may address this at some point, but for this project, where the goal was to learn about
compilers and interpreters, I do not see this as a particularly serious limitation.

### Building `cljlox` with `deps.edn`

Build the interpreter as a standalone jar file which can be used with a standard Java runtime environment.

```bash
clj -T:build uber
```

Run a demo program using the generated uberjar

```bash
java -jar target/jvm/cljlox.jar
```

Or run directly using clojure tools

```bash
clj -M:run $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

### Building `cljlox` with `bazel`

We can build `cljlox` in the same way as `jlox` using `bazel`.

```bash
bazelisk build //cljlox:cljlox
```

```bash
bazelisk run //cljlox:cljlox $(git rev-parse --show-toplevel)/demo/project-euler/problem-0001.lox
```

### Building `cljlox` as a native application using `bazel` and `GraalVM`

Using GraalVM is arguably a little bit overkill for this project. But, given that, for relatively short running Lox
 programs the wall-clock time is dominated by JVM startup and optimising `clojure.core` bytecode, compiling the
 interpreter actually makes quite a lot of sense. Although to be completely candid, there is something about seeing the
 entire Lox test suite run and pass in only a few seconds that is very satisfying.

```bash
bazelisk build //cljlox:cljlox_native
```

## cljslox

`cljslox` almost comes for free with our implementation of `cljlox`, this is a JavaScript version of the interpreter
which is neat because we can easily turn the entire interpreter into a web app.

### Prerequisites

## Running the official Lox test suite

Make sure the dart is installed by following the [official instructions](https://dart.dev/get-dart#install) to set up
the appropriate apt repository. The most up-to-date version of the dart sdk is not compatible with the official test
suite

```bash
sudo apt-get update && sudo apt-get install dart=2.19.6-1 && sudo apt-mark hold dart
```

```bash
dart pub get -C craftinginterpreters/tool/
```

```bash
(cd craftinginterpreters && dart tool/bin/test.dart jlox --interpreter ../scripts/cljlox.sh)
```

Unfortunately the version of dart required to run the official test suite is not supported by
[`rules_dart`](https://github.com/aran/rules_dart) which provides the `dart` module to `bazel`. To circumvent this and
to enable the complete test suite to be orchestrated from `bazel` we have written our own test harness in `clojure`. It
should produce the same results as the official test harness, and to ensure it does both test suites are run as part of
CI.

To run the complete test suite through `bazel`
```bash
bazelisk test //...
```
