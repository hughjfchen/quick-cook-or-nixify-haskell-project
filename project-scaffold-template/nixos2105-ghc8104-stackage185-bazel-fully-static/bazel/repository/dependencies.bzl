load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

def workspace_dependencies():

    http_archive(
    	name = "bazel_skylib",
    	urls = [
        	"https://github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
        	"https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.0.3/bazel-skylib-1.0.3.tar.gz",
    	],
    	sha256 = "1c531376ac7e5a180e0237938a2536de0c54d93f5c278634818e0efc952dd56c",
    )

    http_archive(
    	name = "io_tweag_rules_nixpkgs",
    	sha256 = "7aee35c95251c1751e765f7da09c3bb096d41e6d6dca3c72544781a5573be4aa",
    	strip_prefix = "rules_nixpkgs-0.8.0",
    	urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.8.0.tar.gz"],
    )


    http_archive(
        name = "rules_sh",
        sha256 = "83a065ba6469135a35786eb741e17d50f360ca92ab2897857475ab17c0d29931",
        strip_prefix = "rules_sh-0.2.0",
        urls = ["https://github.com/tweag/rules_sh/archive/v0.2.0.tar.gz"],
    )

    http_archive(
    	name = "rules_python",
    	url = "https://github.com/bazelbuild/rules_python/releases/download/0.3.0/rules_python-0.3.0.tar.gz",
    	sha256 = "934c9ceb552e84577b0faf1e5a2f0450314985b4d8712b2b70717dc679fdc01b",
    )

    http_archive(
        name = "rules_haskell",
        sha256 = "34195d24ad929cc1f06fc3bc0406573dd22df7bbbb59adb41ebc37d17d4115e8",
        strip_prefix = "rules_haskell-08139016f1276d1d03dd7fa6c732aac994ae67d0",
        urls = ["https://github.com/tweag/rules_haskell/archive/08139016f1276d1d03dd7fa6c732aac994ae67d0.tar.gz"],
    )

    http_archive(
    	name = "io_bazel_rules_go",
    	sha256 = "69de5c704a05ff37862f7e0f5534d4f479418afc21806c887db544a316f3cb6b",
    	urls = [
        	"https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
        	"https://github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
    	],
    )

    http_archive(
    	name = "io_bazel_rules_docker",
    	sha256 = "59d5b42ac315e7eadffa944e86e90c2990110a1c8075f1cd145f487e999d22b3",
    	strip_prefix = "rules_docker-0.17.0",
    	urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.17.0/rules_docker-v0.17.0.tar.gz"],
    )

