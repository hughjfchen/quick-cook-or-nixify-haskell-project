# This file pins a version of Nixpkgs which is used only for "tooling", which is
# the term we give to programs which we'd like available in our `PATH` through
# `direnv`. This list should generally not include anything used to _actually
# build code_ other than e.g. `bazel` itself -- such toolchains/library sets
# etc. should probably be pinned in their own language-specific Nix derivations.
# See `nix/haskell/default.nix` for an example of this.

let
  # The version of Nixpkgs we will pin for our tooling.
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-{{nixos_release}}";

    # Update the "name" attribute if/when you change this
    url = https://github.com/NixOS/nixpkgs/archive/11c662074e2ae3dddd7e157918b6981de4ce7857.tar.gz;

    # You can obtain an appropriate hash using
    # `nix-prefetch-url --unpack <url>`.
    sha256 = "19ivbhilikci2c86b7i21ccwmjly6bf6r91iqxla2ibkja6hg79b";
  };

  # If you need a different version or a custom build of a tool, or perhaps a
  # tool that isn't in Nixpkgs, an overlay can be a good way to bring it into
  # the package set. This repository doesn't include any examples of such tools
  # but you might find the pattern useful.
  overlay = self: super: {
  };

in
  args@{ overlays ? [], ... }:
    import baseNixpkgs (args // {
      overlays = [overlay] ++ overlays;
    })
