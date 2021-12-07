load("@rules_haskell//haskell:defs.bzl", "haskell_binary", "haskell_toolchain_library")

STACKAGE_PACKAGES = ["base", "text", "containers", "megaparsec"]

[haskell_binary(
    name = "day{}".format(i),
    srcs = ["{}/Main.hs".format(i)],
    deps = ["@stackage//:{}".format(pkg) for pkg in STACKAGE_PACKAGES],
    data = ["{}/input.txt".format(i)],
) for i in range(1, 1 + 7)]
