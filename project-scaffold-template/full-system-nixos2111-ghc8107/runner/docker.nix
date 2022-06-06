{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.java-analyzer-runner;
let
  executable = java-analyzer-runner.java-analyzer-runner.components.exes.java-analyzer-runner;
  binOnly = prj.pkgs.runCommand "java-analyzer-runner-bin" { } ''
    mkdir -p $out/bin
    cp ${executable}/bin/java-analyzer-runner $out/bin
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/java-analyzer-runner
  '';
in { 
  java-analyzer-runner-image = prj.pkgs.dockerTools.buildImage {
  name = "java-analyzer-runner";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "java-analyzer-runner";
  config.Cmd = "--help";
  };
}) crossBuildProject
