load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library")

cc_library(
    name = "libclox",
    srcs = [],
    hdrs = ["include/clox/common.h"],
    includes = ["include"],
    visibility = ["//visibility:public"],
)

cc_binary(
    name = "clox",
    srcs = ["src/main.c"],
    deps = [":libclox"],
)
