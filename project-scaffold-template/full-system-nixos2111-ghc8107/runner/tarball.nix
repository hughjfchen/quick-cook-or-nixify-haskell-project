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

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "java-analyzer-runner-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/java-analyzer-runner-tarball.zip ${binOnly}
    '';
  };
in {
 java-analyzer-runner-tarball = tarball;
}
) crossBuildProject
