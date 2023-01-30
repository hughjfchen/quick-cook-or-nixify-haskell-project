{ nativePkgs ? (import ./default.nix {}).pkgs,
crossBuildProject ? import ./cross-build.nix {} }:
nativePkgs.lib.mapAttrs (_: prj:
with prj.{{name}};
let
  executable = {{name}}.{{name}}.components.exes.{{name}};
  binOnly = prj.pkgs.runCommand "{{name}}-bin" { } ''
    mkdir -p $out/bin
    cp -R ${executable}/bin/* $out/bin/
    ${nativePkgs.nukeReferences}/bin/nuke-refs $out/bin/{{name}}
  '';

  tarball = nativePkgs.stdenv.mkDerivation {
    name = "{{name}}-tarball";
    buildInputs = with nativePkgs; [ zip ];

    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/
      zip -r -9 $out/{{name}}-tarball.zip ${binOnly}
    '';
  };
in {
 {{name}}-tarball = tarball;
}
) crossBuildProject
