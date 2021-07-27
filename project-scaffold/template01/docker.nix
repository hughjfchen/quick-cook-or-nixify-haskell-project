{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.{{name}};
let
  executable = {{name}}.components.exes.{{name}};
  binOnly = pkgs.runCommand "{{name}}-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/{{name}} $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/{{name}}
  '';
in pkgs.dockerTools.buildImage {
  name = "{{name}}";
  contents = [ binOnly pkgs.cacert pkgs.iana-etc ];
  config.Entrypoint = "{{name}}";
}) crossBuildProject
