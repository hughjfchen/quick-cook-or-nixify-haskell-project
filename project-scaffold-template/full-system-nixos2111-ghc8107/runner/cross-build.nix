{ defaultPlatformProject ? import ./default.nix {} ,
toBuild ? import ./nix/cross-build/systems.nix defaultPlatformProject.pkgs 
} :
# map through the system list
defaultPlatformProject.pkgs.lib.mapAttrs (_: pkgs: rec {
  # nativePkgs.lib.recurseIntoAttrs, just a bit more explicilty.
  recurseForDerivations = true;

  java-analyzer-runner = import ./default.nix { nativePkgs = pkgs;
                                     customModules = [ { packages.java-analyzer-runner.dontStrip = false; } ] ++ (if pkgs.stdenv.hostPlatform.isMusl then 
                                                        [
                                                          # following customization is to build fully static binary for project using postgresql-libpq
                                                          { packages.postgresql-libpq.flags.use-pkg-config = true;  }
                                                          # The order of -lssl and -lcrypto is important here
                                                          { packages.java-analyzer-runner.configureFlags = 
                                                            [
                                                              "--ghc-option=-optl=-lssl"
                                                              "--ghc-option=-optl=-lcrypto"
                                                              "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
                                                              "--ghc-option=-optl=-lpgcommon"
                                                              "--ghc-option=-optl=-lpgport"
                                                              "--ghc-option=-optl=-L${pkgs.postgresql.out}/lib"
                                                            ];
                                                          }
                                                        ] 
                                                        else 
                                                        []);
                                   };

  inherit pkgs;

}) toBuild

