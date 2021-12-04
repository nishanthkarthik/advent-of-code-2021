load("@rules_haskell//haskell:defs.bzl", "haskell_toolchain_library")

package(default_visibility = ["//visibility:public"])

PACKAGES = ["base", "process", "directory", "containers", "text"]

[haskell_toolchain_library(
    name = pkg,
    package = pkg,
) for pkg in PACKAGES]
