{
  nativePkgs ? import ./default.nix {}, # the native package set
  pkgs ? import ./cross-build.nix {}, # the package set for corss build, we're especially interested in the fully static binary
  releasePhase, # the phase for release, must be "local", "test" and "production"
  releaseHost, # the hostname for release,the binary would deploy to it finally
  genSystemdUnit ? true, # whether should generate a systemd unit and a setup script for the binary
  userName ? "", # the user name on the target machine. If empty, use the user on the build machine for program directory, root for running program
  dockerOnTarget ? false # whether docker/docker-compose is needed on the target machine
}:
let
  nPkgs = nativePkgs.pkgs;
  sPkgs = pkgs.x86-musl64; # for the fully static build
  lib = nPkgs.lib; # lib functions from the native package set

  # extra runtime dependencies
  # eclipse package and plugins
  my-eclipse-mat = nPkgs.eclipse-mat.overrideAttrs (oldAttrs: { buildCommand = nPkgs.lib.concatStringsSep "\n" [
                                            oldAttrs.buildCommand
                                            "mv $out/mat/MemoryAnalyzer $out/mat/eclipse"
                                            "mv $out/mat/MemoryAnalyzer.ini $out/mat/eclipse.ini"
                                            "mv $out/mat $out/eclipse"
                                            "sed \'s/\\/mat\\/MemoryAnalyzer/\\/eclipse\\/eclipse/g\' $out/bin/eclipse-mat > $out/bin/eclipse"
                                            "chmod +x $out/bin/eclipse"
                                            ];});
  mat-with-dtfj = nPkgs.eclipses.eclipseWithPlugins {
    eclipse = my-eclipse-mat;
    jvmArgs = [ "-Xmx4096m" ];
    plugins = [
      (nPkgs.eclipses.plugins.buildEclipsePlugin {
        name = "ibm-dtfj";
        srcFeature = builtins.fetchurl {
          url = "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/features/com.ibm.dtfj.feature_1.12.29003.202006111057.jar";
          sha256 = "1jfmb96qn422wrqkkbmd8n0lgdpx0c2g2lbhas00j8020w29yiw8";
        };
        srcPlugins = [
          (builtins.fetchurl {
            url = "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/plugins/com.ibm.dtfj.api_1.12.29003.202006111057.jar";
            sha256 = "0qcmhdh2skbjqmfi42sq4i7zfr2arkvna0qb3k4ci1d36c21d4y1";
          })
          (builtins.fetchurl {
            url = "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/plugins/com.ibm.dtfj.j9_1.12.29003.202006111057.jar";
            sha256 = "1v2vs3xwngsqsvy2vhajqm3i42dx8j99yacir1sp7xbicx5cdiy6";
          })
        ];
      })
    ];
  };
  mat-with-dtfj-sh = nPkgs.writeShellApplication {
    name = "mat-with-dtfj-sh";
    runtimeInputs = [ nPkgs.xvfb-run mat-with-dtfj ];
    text = ''
      xvfb-run -a eclipse -consolelog -application org.eclipse.mat.api.parse "$@"
    '';
  };
  # java jar packages
  my-jca = nPkgs.stdenv.mkDerivation {
    name = "my-jca";
    version = "4611";
    src = builtins.fetchurl {
      url = "https://public.dhe.ibm.com/software/websphere/appserv/support/tools/jca/jca4611.jar";
      sha256 = "16wrbxl229qr4bnmdpdi1swmgfgy8irq35gmbcicgaq3grga781q";
    };
    dontBuild = true;
    dontUnpack = true;
    unpackPhase = "";
    installPhase = ''
      mkdir -p $out/share/java
      cp $src $out/share/java/
    '';
  };
  my-jca-sh = nPkgs.writeShellApplication {
    name = "my-jca-sh";
    runtimeInputs = [ nPkgs.xvfb-run nPkgs.jdk11 ];
    text = ''
      xvfb-run -a java -jar ${my-jca.src} "$@"
    '';
  };

  # the config
  {{name}}-config = nPkgs.writeTextFile {
    name = "{{name}}-config";
    # generate the key = value format config, refer to the lib.generators for other formats
    text = (lib.generators.toKeyValue {}) (import ./config/${releasePhase}/${releaseHost} { pkgs = nPkgs; });
  };
  {{name}}-bin-sh-paths = [
    # list the runtime dependencies, especially those cannot be determined by nix automatically
    nPkgs.wget
    nPkgs.curl
    mat-with-dtfj-sh
    my-jca-sh
    sPkgs.{{name}}.{{name}}-exe
  ];
  {{name}}-bin-sh = nPkgs.writeShellApplication {
    name = "{{name}}-bin-sh";
    runtimeInputs = {{name}}-bin-sh-paths;
    # wrap the executable, suppose it accept a --config commandl ine option to load the config
    text = ''
      if [ -f "$HOME"/.config/{{name}}/{{name}}.properties ]; then
        THE_CONFIG_FILE="$HOME"/.config/{{name}}/{{name}}.properties
      else
        THE_CONFIG_FILE={%raw%}${{%endraw%}{{name}}{%raw%}}{%endraw%}
      fi
      {{name}} --config.file="$THE_CONFIG_FILE" "$@"
    '';
  };
  # following define the service
  {{name}}-service = { lib, pkgs, config, ... }:
      let
        cfg = config.services.{{name}};
      in
        {
          options = {
            services.{{name}} = {
              enable = lib.mkOption {
                default = true;
                type = lib.types.bool;
                description = "enable to generate a config to start the service";
              };
              # add extra options here, if any
            };
          };
          config = lib.mkIf cfg.enable {
            systemd.services.{{name}} = {
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];
              description = "{{name}} service";
              serviceConfig = {
                Type = "simple";
                User = "{{name}}";
                ExecStart = ''{%raw%}${{%endraw%}{{name}}-bin-sh{%raw%}}{%endraw%}/bin/{{name}}-bin-sh --command=Start'';
                ExecStop = ''{%raw%}${{%endraw%}{{name}}-bin-sh{%raw%}}{%endraw%}/bin/{{name}}-bin-sh --command=Stop'';
                Restart = "on-failure";
              };
            };
          };
        };
  mk-{{name}}-service-unit = nPkgs.writeText "{{name}}.service" (nPkgs.nixos ( { lib, pkgs, config, ... }:
      {
        imports = [ {{name}}-service ];
      })
  ).config.systemd.units."{{name}}.service".text;
in
{ inherit nativePkgs pkgs;
  mk-{{name}}-service-systemd-setup-or-bin-sh = if genSystemdUnit then
    (nPkgs.setupSystemdUnits {
      namespace = "{{name}}";
      units = {
        "{{name}}.service" = mk-{{name}}-service-unit;
      };
    }) else {{name}}-bin-sh;
}
