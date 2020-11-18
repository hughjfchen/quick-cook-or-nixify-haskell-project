{ defaultPlatformProject ? import ./default.nix {} ,
toBuild ? import ./nix/cross-build/systems.nix defaultPlatformProject.pkgs 
} :
# 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
defaultPlatformProject.pkgs.lib.mapAttrs (_: pkgs: rec {
  # nativePkgs.lib.recurseIntoAttrs, just a bit more explicilty.
  recurseForDerivations = true;

  MY_PROJECT_NAME = import ./default.nix { nativePkgs = pkgs; };

  inherit pkgs;

}) toBuild

