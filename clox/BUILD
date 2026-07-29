# clox/BUILD

# The core interpreter engine
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
