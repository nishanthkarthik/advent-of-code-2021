load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "rules_haskell",
    commit = "eec33928e7ee2337a8d01ab964cb556353345696",
    shallow_since = "1637317693 +0000",
    remote = "https://github.com/tweag/rules_haskell.git",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(
    version = "8.10.3",
)
