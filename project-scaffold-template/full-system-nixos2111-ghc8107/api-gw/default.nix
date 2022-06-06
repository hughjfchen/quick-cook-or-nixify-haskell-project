let
  sources = import ./nix/sources.nix;
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import sources."haskell.nix" { };
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  #nixpkgsSrc = if haskellNix.pkgs.stdenv.hostPlatform.isDarwin then sources.nixpkgs-darwin else haskellNix.sources.nixpkgs-2111;
  # no need to check platform now
  nixpkgsSrc = haskellNix.sources.nixpkgs-2111;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in { nativePkgs ? import nixpkgsSrc (nixpkgsArgs // {
  overlays = nixpkgsArgs.overlays ++ [ (import ./nix/overlay) ];
}), haskellCompiler ? "ghc8107", customModules ? [ ] }:
let
  pkgs = nativePkgs;
  # 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
in rec {
  # inherit the pkgs package set so that others importing this function can use it
  inherit pkgs;

  # nativePkgs.lib.recurseIntoAttrs, just a bit more explicilty.
  recurseForDerivations = true;

  # java-analyzer-uploader = (pkgs.haskell-nix.project {
  #     src = pkgs.haskell-nix.haskellLib.cleanGit {
  #       name = "java-analyzer-uploader";
  #       src = ./.;
  #     };
  #     index-state = pkgs.haskell-nix.internalHackageIndexState;
  #     compiler-nix-name = haskellCompiler;
  #     modules = customModules;
  # });

  # java-analyzer-uploader-exe = java-analyzer-uploader.java-analyzer-uploader.components.exes.java-analyzer-uploader;
  # java-analyzer-uploader-test = java-analyzer-uploader.java-analyzer-uploader.components.tests.java-analyzer-uploader-test;

}
