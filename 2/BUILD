load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "main",
    srcs = ["Main.hs"],
    deps = ["//library:base"],
    data = ["input.txt"],
)
