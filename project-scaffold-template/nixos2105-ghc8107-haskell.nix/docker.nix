{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.{{name}};
let
  executable = {{name}}.{{name}}.components.exes.{{name}};
  binOnly = prj.pkgs.runCommand "{{name}}-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/{{name}} $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/{{name}}
  '';
in { 
  {{name}}-image = prj.pkgs.dockerTools.buildImage {
  name = "{{name}}";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "{{name}}";
  config.Cmd = "--help";
  };
}) crossBuildProject
