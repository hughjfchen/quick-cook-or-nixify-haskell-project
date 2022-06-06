{ pkgs, lib, config, ... }:
lib.mapAttrs' (attr: _: {
  name = lib.removeSuffix ".nix" attr;
  value = import (./. + "/${attr}") { inherit pkgs lib config; };
}) (lib.filterAttrs (fname: type: type == "regular" && fname != "default.nix")
  (builtins.readDir ./.))
