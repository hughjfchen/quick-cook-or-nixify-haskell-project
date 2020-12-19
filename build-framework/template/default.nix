let
  sources = import ./nix/sources.nix;
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import sources."haskell.nix" {};
  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = if haskellNix.pkgs.lib.strings.hasSuffix builtins.currentSystem "darwin" then sources.nixpkgs-darwin else haskellNix.sources.MY_NIXPKGS;
  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ nativePkgs ? import nixpkgsSrc (nixpkgsArgs // { overlays = nixpkgsArgs.overlays ++ [(import ./nix/overlay)]; })
, haskellCompiler ? "MY_GHC_VER"
#, cardano-node-json
#, cardano-node-info ? __fromJSON (__readFile cardano-node-json)
#, cardano-node-src ? nativePkgs.fetchgit (removeAttrs cardano-node-info [ "date" ])
}:
let pkgs = nativePkgs;
in
# 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
rec {
  # inherit the pkgs package set so that others importing this function can use it
  inherit pkgs;

  # nativePkgs.lib.recurseIntoAttrs, just a bit more explicilty.
  recurseForDerivations = true;

  MY_PROJECT_NAME = (pkgs.haskell-nix.project {
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "MY_PROJECT_NAME";
        src = ./.;
      };
      index-state: "MY_INDEX_STATE";
      compiler-nix-name = haskellCompiler;
      # 'ghc' option is deprecated
      #ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
      modules = [
        { packages.MY_PROJECT_NAME.dontStrip = false; }
      ];
    });

  MY_PROJECT_NAME-exe = MY_PROJECT_NAME.MY_PROJECT_NAME.components.exes.MY_PROJECT_NAME;

  #hello = (pkgs.haskell-nix.hackage-package {
      #name = "hello";
      #version = "1.0.0.2";
      #ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    #}).components.exes.hello;

  #cabal-install = (pkgs.haskell-nix.hackage-package {
      #name = "cabal-install";
      # can't build 3.0 or 3.2, we seem to pass in the lib Cabal from our GHC :-/
      #version = "3.2.0.0";
      #ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};

      #modules = [
        # haddock can't find haddock m(
        #{ doHaddock = false; }
        # lukko breaks hsc2hs
        #{ packages.lukko.patches = [ ./cabal-install-patches/19.patch ]; }
        # Remove Cabal from nonReinstallablePkgs to be able to pick Cabal-3.2.
        #{ nonReinstallablePkgs = [
          #"rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          #"deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          ## ghcjs custom packages
          #"ghcjs-prim" "ghcjs-th"
          #"ghc-boot"
          #"ghc" "Win32" "array" "binary" "bytestring" "containers"
          #"directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          #"hpc"
          #"mtl" "parsec" "process" "text" "time" "transformers"
          #"unix" "xhtml"
          # "stm" "terminfo"
        #]; }
      #];
    #}).components.exes.cabal;

  #__cardano-node = (pkgs.haskell-nix.cabalProject {
      # pkgs.haskell-nix.haskellLib.cleanGit { name = "cardano-node"; src = ... } <- this doesn't work with fetchgit results
      #src = cardano-node-src;
      # ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
      #modules = [
        # Allow reinstallation of Win32
        #{ nonReinstallablePkgs =
          #[ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            #"deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            # ghcjs custom packages
            #"ghcjs-prim" "ghcjs-th"
            #"ghc-boot"
            #"ghc" "array" "binary" "bytestring" "containers"
            #"filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            # "ghci" "haskeline"
            #"hpc"
            #"mtl" "parsec" "text" "transformers"
            #"xhtml"
            # "stm" "terminfo"
          #];
        #}
        # haddocks are useless (lol);
        # and broken for cross compilers!
        #{ doHaddock = false; }
        #{ compiler.nix-name = haskellCompiler; }
        #{ packages.cardano-config.flags.systemd = false;
          #packages.cardano-node.flags.systemd = false; }
        #{ packages.terminal-size.patches = [ ./cardano-node-patches/terminal-size-0.3.2.1.patch ];
          #packages.unix-bytestring.patches = [ ./cardano-node-patches/unix-bytestring-0.3.7.3.patch ];
          #packages.typerep-map.patches = [ ./cardano-node-patches/typerep-map-PR82.patch ];
          #packages.streaming-bytestring.patches = [ ./cardano-node-patches/streaming-bytestring-0.1.6.patch ];
          #packages.byron-spec-ledger.patches = [ ./cardano-node-patches/byron-ledger-spec-no-goblins.patch ];
          #packages.byron-spec-ledger.flags.goblins = false;
          # this one will disable gitRev; which fails (due to a linker bug) for armv7
          # packages.cardano-config.patches = [ ./cardano-node-patches/1036.patch ];

          # Disable cabal-doctest tests by turning off custom setups
          #packages.comonad.package.buildType = nativePkgs.lib.mkForce "Simple";
          #packages.distributive.package.buildType = nativePkgs.lib.mkForce "Simple";
          #packages.lens.package.buildType = nativePkgs.lib.mkForce "Simple";
          #packages.nonempty-vector.package.buildType = nativePkgs.lib.mkForce "Simple";
          #packages.semigroupoids.package.buildType = nativePkgs.lib.mkForce "Simple";

          # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
          #packages.Win32.components.library.build-tools = nativePkgs.lib.mkForce [];
          #packages.terminal-size.components.library.build-tools = nativePkgs.lib.mkForce [];
          #packages.network.components.library.build-tools = nativePkgs.lib.mkForce [];
        #}
      #];
    #});

    #inherit (__cardano-node.cardano-node.components.exes) cardano-node;
    #inherit (__cardano-node.cardano-cli.components.exes)  cardano-cli;

    #tarball = nativePkgs.stdenv.mkDerivation {
      #name = "${pkgs.stdenv.targetPlatform.config}-tarball";
      #buildInputs = with nativePkgs; [ patchelf zip ];

      #phases = [ "buildPhase" "installPhase" ];

      #buildPhase = ''
        #mkdir -p cardano-node
        #cp ${cardano-cli}/bin/*cardano-cli* cardano-node/
        #cp ${cardano-node.override { enableDebugRTS = true; }}/bin/*cardano-node* cardano-node/
      #'' + pkgs.lib.optionalString (pkgs.stdenv.targetPlatform.isLinux && !pkgs.stdenv.targetPlatform.isMusl) ''
        #for bin in cardano-node/*; do
          #patchelf --set-interpreter /lib/ld-linux-armhf.so.3 $bin
        #done
      #'' + pkgs.lib.optionalString (pkgs.stdenv.targetPlatform.isWindows) ''
        #cp ${pkgs.libffi}/bin/*.dll cardano-node/
      #'';
      #installPhase = ''
        #mkdir -p $out/
        #zip -r -9 $out/${pkgs.stdenv.hostPlatform.config}-cardano-node-${cardano-node-info.rev or "unknown"}.zip cardano-node
      #'';
    #};

}

