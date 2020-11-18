{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.MY_PROJECT_NAME;
let
  executable = MY_PROJECT_NAME.components.exes.MY_PROJECT_NAME;
  binOnly = pkgs.runCommand "MY_PROJECT_NAME-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/MY_PROJECT_NAME $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/MY_PROJECT_NAME
  '';
in pkgs.dockerTools.buildImage {
  name = "MY_PROJECT_NAME";
  contents = [ binOnly pkgs.cacert pkgs.iana-etc ];
  config.Entrypoint = "MY_PROJECT_NAME";
}) crossBuildProject
