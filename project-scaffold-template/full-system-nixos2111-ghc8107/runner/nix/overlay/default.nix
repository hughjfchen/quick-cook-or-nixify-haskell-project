self: prev:
with prev.lib;
mapAttrs' (attr: _: {
  name = removeSuffix ".nix" attr;
  value = import (./. + "/${attr}") self prev;
}) (filterAttrs (attr: _: attr != "default.nix") (builtins.readDir ./.))
