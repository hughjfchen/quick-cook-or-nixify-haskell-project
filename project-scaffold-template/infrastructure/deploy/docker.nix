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
    printf "%s %s\n" "$out/bin/{{name}}-exe" '"''$@" +RTS -N''${K8S_CPU_LIMIT-3} -M''${K8S_MEMORY_LIMIT-1024M} -A''${GHC_GEN_0_SIZE-16M} -RTS' > $out/bin/{{name}}_entry.sh
    chmod +x $out/bin/{{name}}_entry.sh
  '';
in { 
  {{name}}-image = prj.pkgs.dockerTools.buildImage {
  name = "{{name}}";
  tag = executable.version;
  contents = [ binOnly prj.pkgs.cacert prj.pkgs.iana-etc ];
  config.Entrypoint = "{{name}}_entry.sh";
  config.Cmd = "";
  };
}) crossBuildProject
