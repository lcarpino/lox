load("@rules_java//java:defs.bzl", "java_binary")
load("//bazel:defs.bzl", "generated_ast_srcs")

# TODO: make Expr.java generation part of the build process

java_binary(
    name = "generate_ast",
    srcs = [
        "GenerateAst.java",
    ],
    main_class = "com.craftinginterpreters.tool.GenerateAst",
    visibility = ["//visibility:public"],
)

generated_ast_srcs(
    name = "generate_ast_srcs",
    generator = ":generate_ast",
    visibility = ["//visibility:public"],
)