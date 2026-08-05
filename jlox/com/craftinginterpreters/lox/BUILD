load("@rules_java//java:defs.bzl", "java_binary", "java_library")

java_library(
    name = "token",
    srcs = [
        "Token.java",
        "TokenType.java",
    ],
)

java_binary(
    name = "lox",
    srcs = [
        "Environment.java",
        "Interpreter.java",
        "Lox.java",
        "LoxCallable.java",
        "LoxClass.java",
        "LoxFunction.java",
        "LoxInstance.java",
        "Parser.java",
        "Resolver.java",
        "Return.java",
        "RuntimeError.java",
        "Scanner.java",
        "TokenType.java",
        "Token.java",
        "//jlox/com/craftinginterpreters/tool:generate_ast_srcs",
    ],
    main_class = "com.craftinginterpreters.lox.Lox",
    visibility = ["//visibility:public"],
)

java_binary(
    name = "ast",
    srcs = [
        "AstPrinter.java",
        "//jlox/com/craftinginterpreters/tool:generate_ast_srcs",
    ],
    deps = [
        ":token",
    ],
    main_class = "com.craftinginterpreters.lox.AstPrinter",
    visibility = ["//visibility:public"],
)

java_test(
    name = "jlox_test",
    main_class = "tool.test_harness",
    use_testrunner = False,
    runtime_deps = ["//tool:test-harness-lib"],
    data = [
        ":lox",
        "//:test_data",
    ],
    args = [
        "--interpreter", "$(location :lox)",
        "--dir", "craftinginterpreters/test",
        "--suite", "jlox",
    ],
)
