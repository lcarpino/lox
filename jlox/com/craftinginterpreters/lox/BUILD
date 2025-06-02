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
        "Lox.java",
        "TokenType.java",
        "Token.java",
        "Scanner.java",
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