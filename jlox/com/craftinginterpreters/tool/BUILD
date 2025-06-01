load("@rules_java//java:defs.bzl", "java_binary")

# TODO: make Expr.java generation part of the build process

java_binary(
    name = "generate_ast",
    srcs = [
        "GenerateAst.java",
    ],
    main_class = "com.craftinginterpreters.tool.GenerateAst",
    visibility = ["//visibility:public"],
)