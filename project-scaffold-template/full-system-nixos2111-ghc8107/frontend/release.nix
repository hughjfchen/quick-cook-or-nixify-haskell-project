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
  {{name}}-frontend-env = (import
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
  {{name}}-frontend-config = (import (builtins.fetchGit {
    url = "https://github.com/hughjfchen/deploy-config";
  }) {
    pkgs = nPkgs;
    modules = [
      ../config/site/${site}/phase/${phase}/db.nix
      ../config/site/${site}/phase/${phase}/db-gw.nix
      ../config/site/${site}/phase/${phase}/api-gw.nix
      ../config/site/${site}/phase/${phase}/messaging.nix
    ];
    env = {{name}}-frontend-env;
  }).config;

  my-frontend-build = (import ./default.nix { }).{{name}}-frontend;
  # my services dependencies
  # following define the service
  my-frontend-distributable =
    nPkgs.runCommand "my-frontend-distributable" { } ''
      mkdir -p $out
      cp -R ${my-frontend-build}/* $out/
      for THEFILE in $(grep -R '${my-frontend-config.frontend.currentServer}' $out|awk -F':' '{print $1}') do
        sed -i 's/${my-frontend-config.frontend.currentServer}/${my-frontend-config.frontend.backendServer}/g' $THEFILE
      done
    '';

in rec {
  inherit nativePkgs pkgs {{name}}-frontend-config;

  mk-my-{{name}}-frontend-reference =
    nPkgs.writeReferencesToFile my-frontend-distributable;
  mk-my-rabbitmq-deploy-sh = deploy-packer.mk-deploy-sh {
    env = my-messaging-env.messaging;
    payloadPath = setup-and-unsetup-or-bin-sh;
    inherit innerTarballName;
    execName = "rabbitmq";
  };
  mk-my-rabbitmq-cleanup-sh = deploy-packer.mk-cleanup-sh {
    env = my-messaging-env.messaging;
    payloadPath = setup-and-unsetup-or-bin-sh;
    inherit innerTarballName;
    execName = "rabbitmq";
  };
  mk-my-release-packer = deploy-packer.mk-release-packer {
    referencePath = mk-my-rabbitmq-reference;
    component = pkgName;
    inherit site phase innerTarballName;
    deployScript = mk-my-rabbitmq-deploy-sh;
    cleanupScript = mk-my-rabbitmq-cleanup-sh;
  };
}
