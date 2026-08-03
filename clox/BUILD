load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")

cc_library(
    name = "libclox",
    srcs = ["src/chunk.c", "src/debug.c", "src/memory.c"],
    hdrs = ["include/clox/chunk.h", "include/clox/common.h", "include/clox/debug.h", "include/clox/memory.h"],
    includes = ["include"],
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "clox",
    srcs = ["src/main.c"],
    deps = [":libclox"],
)
