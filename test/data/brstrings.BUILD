package(default_visibility = ["//visibility:public"])

load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")

cc_library(
    name = r"hello-lib",
    srcs = [b"hello-lib.cc"],
    hdrs = [br"hello-lib.h"],
)

cc_binary(
    name = """hello-world""",
    srcs = ["hello-world.cc"],
    deps = [":hello-lib"],
)

cc_test(
    name = '''hello-success_test''',
    srcs = [b'''hello-world.cc'''],
    deps = [r''':hello-lib'''],
    br   = [br''':hello-lib'''],
)

cc_test(
    name = "hello-fail_test",
    srcs = ["hello-fail.cc"],
    deps = [":hello-lib"],
)

filegroup(
    name = "srcs",
    srcs = glob(["**"]),
)
