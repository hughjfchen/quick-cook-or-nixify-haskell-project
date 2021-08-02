load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_sh_posix_configure")
load("@rules_sh//sh:posix.bzl", "sh_posix_configure")

def setup_posix():
    # POSIX tooling is taken from the tooling pin.
    nixpkgs_sh_posix_configure(repository = "@nixpkgs")

    sh_posix_configure()
