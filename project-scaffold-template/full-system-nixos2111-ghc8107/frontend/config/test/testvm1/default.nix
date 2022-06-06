{ pkgs, lib, config, ... }:
let
  upConfig = import ../default.nix { inherit pkgs lib config; };
  mergeConfig = lib.recursiveUpdate config upConfig;
in lib.recursiveUpdate mergeConfig (lib.mapAttrs' (attr: _: {
  name = lib.removeSuffix ".nix" attr;
  value = import (./. + "/${attr}") {
    inherit pkgs lib;
    config = mergeConfig;
  };
}) (lib.filterAttrs (fname: type: type == "regular" && fname != "default.nix")
  (builtins.readDir ./.)))
