load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")

git_repository(
    name = "rules_haskell",
    commit = "a7241fa64c7cd36462a1f6ac4c660d1247d5e07b",
    shallow_since = "1638457329 +0000",
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
    version = "8.10.7",
)

# Stackage

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

stack_snapshot(
    name = "stackage",
    packages = [
        "array",
        "base",
        "containers",
        "megaparsec",
        "random",
        "text",
        "heap",
        "parallel",
    ],
    snapshot = "lts-18.18",
    stack_snapshot_json = "//tooling:stackage_snapshot.json",
)
