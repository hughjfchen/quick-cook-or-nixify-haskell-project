{ defaultPlatformProject ? import ./default.nix {} }:

  defaultPlatformProject.MY_PROJECT_NAME.shellFor {
    # Include only the *local* packages of your project.
    packages = ps: with ps; [
      MY_PROJECT_NAME
      #pkgb
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = false;

    # You might want some extra tools in the shell (optional).

    # Some common tools can be added with the `tools` argument
    #tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; };
    tools = { cabal = "MY_CABAL_VER"; hlint = "MY_HLINT_VER"; };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = with defaultPlatformProject.pkgs;
      #[ ghcid lorri niv ];
      [ ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
}
