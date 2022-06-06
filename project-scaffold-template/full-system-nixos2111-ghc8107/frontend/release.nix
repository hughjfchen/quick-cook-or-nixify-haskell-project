{ nativePkgs ? import ./default.nix { }, # the native package set
pkgs ? import ./cross-build.nix { }
, # the package set for corss build, we're especially interested in the fully static binary
releasePhase, # the phase for release, must be "local", "test" and "production"
releaseHost, # the hostname for release,the binary would deploy to it finally
genSystemdUnit ? true
, # whether should generate a systemd unit and a setup script for the binary
userName ? ""
, # the user name on the target machine. If empty, use the user on the build machine for program directory, root for running program
dockerOnTarget ?
  false # whether docker/docker-compose is needed on the target machine
}:
let
  nPkgs = nativePkgs.pkgs;
  sPkgs = pkgs.x86-musl64; # for the fully static build
  lib = nPkgs.lib; # lib functions from the native package set

  common-config = { inherit releasePhase releaseHost genSystemdUnit userName dockerOnTarget; };
  my-frontend-config = import ./config/${releasePhase}/${releaseHost}/default.nix { pkgs = nPkgs; lib = lib; config = common-config; };
  my-frontend-build = (import ./default.nix { }).java-analyzer-frontend;
  # my services dependencies
  # following define the service
  my-frontend-distributable = nPkgs.runCommand "my-frontend-distributable" {} ''
      mkdir -p $out
      cp -R ${my-frontend-build}/* $out/
      for THEFILE in $(grep -R '${my-frontend-config.frontend.currentServer}' $out|awk -F':' '{print $1}') do
        sed -i 's/${my-frontend-config.frontend.currentServer}/${my-frontend-config.frontend.backendServer}/g' $THEFILE
      done
    '';
in {
  inherit nativePkgs pkgs my-frontend-distributable;
}
