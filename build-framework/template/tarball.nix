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

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "MY_PROJECT_NAME-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/MY_PROJECT_NAME-tarball.zip ${binOnly}
    '';
  };
in
 tarball;
) crossBuildProject
