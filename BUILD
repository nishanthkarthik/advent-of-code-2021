load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

[haskell_binary(
    name = "day{}".format(i),
    srcs = ["{}/Main.hs".format(i)],
    deps = ["//library:base", "//library:containers", "//library:text"],
    data = ["{}/input.txt".format(i)],
) for i in range(1, 1 + 4)]
