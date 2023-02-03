{ nativePkgs ? import ./default.nix { }, # the native package set
pkgs ? import ./cross-build.nix { }
, # the package set for corss build, we're especially interested in the fully static binary
site, # the site for release, the binary would deploy to it finally
phase, # the phase for release, must be "local", "test" and "production"
}:
let
  nPkgs = nativePkgs.pkgs;
  sPkgs = pkgs.x86-musl64; # for the fully static build
  lib = nPkgs.lib; # lib functions from the native package set
  pkgName = "{{name}}-frontend";
  innerTarballName = lib.concatStringsSep "." [
    (lib.concatStringsSep "-" [ pkgName site phase ])
    "tar"
    "gz"
  ];

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  deploy-packer = import (builtins.fetchGit {
    url = "https://github.com/hughjfchen/deploy-packer";
  }) {
    inherit lib;
    pkgs = nPkgs;
  };

  # the deployment env
  my-frontend-env = (import
    (builtins.fetchGit { url = "https://github.com/hughjfchen/deploy-env"; }) {
      pkgs = nPkgs;
      modules = [
        ../env/site/${site}/phase/${phase}/db.nix
        ../env/site/${site}/phase/${phase}/db-gw.nix
        ../env/site/${site}/phase/${phase}/api-gw.nix
        ../env/site/${site}/phase/${phase}/messaging.nix
      ];
    }).env;

  # app and dependent config
  my-frontend-config = (import (builtins.fetchGit {
    url = "https://github.com/hughjfchen/deploy-config";
  }) {
    pkgs = nPkgs;
    modules = [
      ../config/site/${site}/phase/${phase}/db.nix
      ../config/site/${site}/phase/${phase}/db-gw.nix
      ../config/site/${site}/phase/${phase}/api-gw.nix
      ../config/site/${site}/phase/${phase}/messaging.nix
    ];
    env = my-db-env;
  }).config;

  # the frontend, comment out for now.
  {{name}}-frontend-distributable =
    (import ./default.nix { }).{{name}}-frontend.overrideAttrs
    (oldAttrs: {
      buildPhase = ''
        # following not working, do not know why
        # rm -fr .env.production.local .env.local .env.production
        # echo "REACT_APP_BASE_URL=http://${my-frontend-config.api-gw.serverName}:${
          toString my-frontend-config.api-gw.listenPort
        }" > .env.production
        sed -i 's/{process.env.REACT_APP_BASE_URL}/http:\/\/${my-frontend-config.api-gw.serverName}:${
          toString my-frontend-config.api-gw.listenPort
        }/g' src/dataprovider.js
        sed -i 's/{process.env.REACT_APP_BASE_URL}/http:\/\/${my-frontend-config.api-gw.serverName}:${
          toString my-frontend-config.api-gw.listenPort
        }/g' src/auth.js
      '' + oldAttrs.buildPhase;
    });

in {{name}}-frontend-distributable
