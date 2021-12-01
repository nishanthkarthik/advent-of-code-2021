load("@rules_haskell//haskell:defs.bzl", "haskell_toolchain_library")

package(default_visibility = ["//visibility:public"])

[haskell_toolchain_library(
    name = pkg,
    package = pkg,
) for pkg in ["base", "process", "directory"]]
