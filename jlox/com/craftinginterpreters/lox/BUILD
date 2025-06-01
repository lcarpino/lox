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
        "Expr.java",
        "Lox.java",
        "TokenType.java",
        "Token.java",
        "Scanner.java",
    ],
    main_class = "com.craftinginterpreters.lox.Lox",
    visibility = ["//visibility:public"],
)

java_binary(
    name = "ast",
    srcs = [
        "AstPrinter.java",
        "Expr.java",
    ],
    deps = [
        ":token",
    ],
    main_class = "com.craftinginterpreters.lox.AstPrinter",
    visibility = ["//visibility:public"],
)