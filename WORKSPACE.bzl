load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")  # buildifier: disable=load
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")  # buildifier: disable=load

all_content = """filegroup(name = "all", srcs = glob(["**"]), visibility = ["//visibility:public"])"""
lua_src = """filegroup(name = "src", srcs = glob(["src/**"]), visibility = ["//visibility:public"])"""

def cc_fetch_repos():

    maybe(
        git_repository,
        name = "rules_cc",
        remote = "https://github.com/bazelbuild/rules_cc",
        commit = "b1c40e1de81913a3c40e5948f78719c28152486d",
        shallow_since = "1605101351 -0800"
        # branch = "master"
    )

    maybe(
        http_archive,
        name = "rules_foreign_cc",
        url = "https://github.com/bazelbuild/rules_foreign_cc/archive/0.2.0.tar.gz",
        sha256 = "d54742ffbdc6924f222d2179f0e10e911c5c659c4ae74158e9fe827aad862ac6",
        strip_prefix = "rules_foreign_cc-0.2.0",
    )

    ######
    maybe(
        git_repository,
        name = "makeheaders",
        remote = "https://github.com/obazl/makeheaders",
        # branch = "main"
        commit = "bb528f3edac6c00953010e28d51e4a52da7555aa",
        shallow_since = "1618495335 -0500"
        # http_archive,
        # name = "makeheaders",
        # urls = [
        #     "https://github.com/obazl/makeheaders/archive/57bae6cc7e88783b060acf711bc21f99d8380ca5.tar.gz"
        # ],
        # strip_prefix = "makeheaders-57bae6cc7e88783b060acf711bc21f99d8380ca5",
        # sha256 = "83ef79d69c02348efd1f52a85fa10e9bd57333e89a23c2fe66a9b298c055d164"
    )

    ######
    maybe(
        http_archive,
        name = "uthash",
        build_file_content = """
filegroup(name = "include", srcs = glob(["include/*.h"]), visibility = ["//visibility:public"])
    """,
        urls = [
            "https://github.com/troydhanson/uthash/archive/refs/tags/v2.3.0.tar.gz"
        ],
        strip_prefix = "uthash-2.3.0",
        sha256 = "e10382ab75518bad8319eb922ad04f907cb20cccb451a3aa980c9d005e661acc"
    )

    ######
    maybe(
        http_archive,
        name = "sfsexp",
        build_file_content = all_content,
        urls = [
            "https://github.com/mjsottile/sfsexp/archive/ad589f9e6e0eca20345320e9c82a3aecc0a5c8aa.tar.gz"
        ],
        strip_prefix = "sfsexp-ad589f9e6e0eca20345320e9c82a3aecc0a5c8aa",
        sha256 = "5a5e70f9d4dffc53a943879a04eedcd458986ddd24eb413c572a4e04fb3682a2"
    )

    ######
    maybe(
        http_archive,
        name = "libinih",
        # build_file_content = "exports_files(['ini.c', 'ini.h'])",
    build_file_content = """
filegroup(name = "srcs", srcs = ["ini.c", "ini.h"], visibility = ["//visibility:public"])
filegroup(name = "hdrs", srcs = ["ini.h"], visibility = ["//visibility:public"])""",
        urls = [
            "https://github.com/benhoyt/inih/archive/cb55f57d87ae840bd0f65dbe6bd22fa021a873a7.tar.gz"
        ],
        strip_prefix = "inih-cb55f57d87ae840bd0f65dbe6bd22fa021a873a7",
        sha256 = "26d05999033eef9e3abca2d4dbf3dc2e4a24335df51231b6faa093be06bb19d7"
    )

    maybe(
        http_archive,
        name = "libre2c",
        urls = [
            "https://github.com/skvadrik/re2c/archive/refs/tags/2.1.1.zip"
        ],
        strip_prefix = "re2c-2.1.1",
        sha256 = "080931d214943ea021fa9360a4694e824674e5c0f2e880153e8cb41982453aa6",
        build_file_content = all_content,
        workspace_file_content = "workspace( name = \"opam-re2c\" )"
    )

    maybe(
        http_archive,
        name = "lua",
        build_file_content = "exports_files(glob([\"**\"]))",
        # build_file_content = lua_src,
        # build_file = "@//external/lua:BUILD.bazel",
        urls = [
            "https://www.lua.org/ftp/lua-5.4.3.tar.gz",
        ],
        strip_prefix = "lua-5.4.3",
        sha256 = "f8612276169e3bfcbcfb8f226195bfc6e466fe13042f1076cbde92b7ec96bbfb"
    )

    maybe(
        http_archive,
        name = "fswatch",
        urls = [
            "https://github.com/emcrisostomo/fswatch/archive/refs/tags/1.15.0.tar.gz",
            # "https://github.com/emcrisostomo/fswatch/archive/30f8f1b134209f5ff39bf580313ff20c4dcb06e5.tar.gz"
        ],
        strip_prefix = "fswatch-1.15.0",
        sha256 = "4a4db635cdaecd63fa7c8813f9cce3f385d0081b626835b11a3da3b66412d75d",
        build_file_content = all_content,
        workspace_file_content = "workspace( name = \"fswatch\" )"
    )

    # http://www.throwtheswitch.org/unity
    maybe(
        http_archive,
        name = "unity",
        urls = [
            "https://github.com/ThrowTheSwitch/Unity/archive/refs/tags/v2.5.2.zip",
        ],
        strip_prefix = "v2.5.2",
        build_file_content = all_content,
        workspace_file_content = "workspace( name = \"unity\" )"
    )

####################
def fetch_stardoc():

    maybe(
        git_repository,
        name = "io_bazel_stardoc",
        remote = "https://github.com/bazelbuild/stardoc.git",
        commit = "4378e9b6bb2831de7143580594782f538f461180",
        shallow_since = "1570829166 -0400"
    )

