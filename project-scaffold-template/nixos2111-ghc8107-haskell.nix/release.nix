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

  # common services
  my-db-init-script = nPkgs.stdenv.mkDerivation {
    src = ./.;
    name = "my-db-init-script";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      MY_DB_ANON_ROLE=$(grep "DB_ANON_ROLE" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_USER=$(grep "DB_USER" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_PASS=$(grep "DB_PASS" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_NAME=$(grep "DB_NAME" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_JWT_SECRET=$(grep "JWT_SECRET" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      cp -R $src/db/src/libs $out/
      cp -R $src/db/src/data $out/
      cp -R $src/db/src/api $out/
      cp -R $src/db/src/sample_data $out/
      mkdir -p $out/authorization
      cp $src/db/src/authorization/roles.sql $out/authorization
      sed "2i grant usage on schema data to $MY_DB_USER;" $src/db/src/authorization/privileges.sql > $out/authorization/privileges.sql
      sed -e "1i drop database if exists $MY_DB_NAME;" -e "1i create database $MY_DB_NAME;" -e "1i \\\\\c $MY_DB_NAME" -e "s/\$DB_ANON_ROLE/$MY_DB_ANON_ROLE/g" -e "s/\$DB_USER/$MY_DB_USER/g" -e "s/\$DB_PASS/$MY_DB_PASS/g" -e "s/\$JWT_SECRET/$MY_JWT_SECRET/g" $src/db/src/init.sql > $out/init.sql
    '';
  };
  mk-my-postgresql-service-unit = (nPkgs.nixos ({ lib, pkgs, config, ... }: {
    config.services.postgresql = {
      enable = true;
      package = nPkgs.postgresql_9_6;
      port = 5432;
      dataDir = "/var/${userName}/data";
      initdbArgs = [ ];
      initialScript = my-db-init-script + /init.sql;
      ensureDatabases = [ ];
      ensureUsers = [ ];
      enableTCPIP = true;
      authentication = nPkgs.lib.mkOverride 10 ''
        local all all trust
        host all all all md5
      '';
      settings = { };
      #superUser = "postgres"; # read-only

    };
  })).config.systemd.units."postgresql.service".unit;

  my-postgrest-config = nPkgs.stdenv.mkDerivation {
    src = ./.;
    name = "my-postgrest-config";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      MY_DB_USER=$(grep "DB_USER" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_PASS=$(grep "DB_PASS" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_HOST=$(grep "DB_HOST" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_PORT=$(grep "DB_PORT" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_NAME=$(grep "DB_NAME" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_SCHEMA=$(grep "DB_SCHEMA" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_ANON_ROLE=$(grep "DB_ANON_ROLE" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_DB_POOL=$(grep "DB_POOL" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_JWT_SECRET=$(grep "JWT_SECRET" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_MAX_ROWS=$(grep "MAX_ROWS" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_PRE_REQUEST=$(grep "PRE_REQUEST" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      MY_SERVER_PROXY_URI=$(grep "SERVER_PROXY_URI" $src/.env | tr -d " " | awk -F"=" '{print $NF}')
      echo "db-uri=\"postgres://$MY_DB_USER:$MY_DB_PASS@$MY_DB_HOST:$MY_DB_PORT/$MY_DB_NAME\"" > $out/postgrest.conf
      [ -n "$MY_DB_ANON_ROLE" ] && echo "db-anon-role=\"$MY_DB_ANON_ROLE\"" >> $out/postgrest.conf
      [ -n "$MY_DB_SCHEMA" ] && echo "db-schema=\"$MY_DB_SCHEMA\"" >> $out/postgrest.conf
      [ -n "$MY_MAX_ROWS" ] && echo "db-max-rows=$MY_MAX_ROWS" >> $out/postgrest.conf
      [ -n "$MY_DB_POOL" ] && echo "db-pool=$MY_DB_POOL" >> $out/postgrest.conf
      [ -n "$MY_PRE_REQUEST" ] && echo "db-pre-request=\"$MY_PRE_REQUEST\"" >> $out/postgrest.conf
      [ -n "$MY_JWT_SECRET" ] && echo "jwt-secret=\"$MY_JWT_SECRET\"" >> $out/postgrest.conf
    '';
  };

  # my services dependencies
  my-postgrest-bin-sh = nPkgs.writeShellApplication {
    name = "my-postgrest-bin-sh";
    runtimeInputs = [ nPkgs.haskellPackages.postgrest ];
    text = ''
      [ ! -f /var/${userName}/config/postgrest.conf ] && cp ${my-postgrest-config}/postgrest.conf /var/${userName}/config/postgrest.conf
      postgrest /var/${userName}/config/postgrest.conf "$@"
    '';
  };

  # following define the service
  my-postgrest-service = { lib, pkgs, config, ... }:
    let cfg = config.services.my-postgrest;
    in {
      options = {
        services.my-postgrest = {
          enable = lib.mkOption {
            default = true;
            type = lib.types.bool;
            description = "enable to generate a config to start the service";
          };
          # add extra options here, if any
        };
      };
      config = lib.mkIf cfg.enable {
        systemd.services.my-postgrest = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "my postgrest service";
          serviceConfig = {
            Type = "simple";
            User = "${userName}";
            ExecStart = "${my-postgrest-bin-sh}/bin/my-postgrest-bin-sh";
            Restart = "on-failure";
          };
        };
      };
    };
  mk-my-postgrest-service-unit = nPkgs.writeText "my-postgrest.service"
    (nPkgs.nixos ({ lib, pkgs, config, ... }: {
      imports = [ my-postgrest-service ];
    })).config.systemd.units."my-postgrest.service".text;

  my-openresty-config = nPkgs.stdenv.mkDerivation {
    src = ./.;
    name = "my-openresty-config";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      cp -R $src/openresty/lua $out/
      mkdir -p $out/nginx/conf
      cp -R $src/openresty/nginx/* $out/nginx/conf
      cp -R $src/openresty/html $out/nginx
      sed "/OPENRESTY_DOC_ROOT/d; 1i OPENRESTY_DOC_ROOT=/var/${userName}/openresty/nginx/html" $src/.env > $out/env
      mkdir -p $out/lualib
      ln -s $out/lua $out/lualib/user_code
      ln -s $out/lualib $out/nginx/lualib
    '';
  };

  # my services dependencies
  my-openresty-bin-sh = nPkgs.writeShellApplication {
    name = "my-openresty-bin-sh";
    runtimeInputs = [ nPkgs.openresty ];
    text = ''
      [ ! -d /var/log/nginx ] && mkdir -p /var/log/nginx && chown -R ${userName}:${userName} /var/log/nginx
      [ ! -d /var/cache/nginx/client_body ] && mkdir -p /var/cache/nginx/client_body && chown -R ${userName}:${userName} /var/cache/nginx
      [ ! -d /var/${userName}/openresty ] && mkdir -p /var/${userName}/openresty && cp -R ${my-openresty-config}/* /var/${userName}/openresty && chown -R ${userName}:${userName} /var/${userName}/openresty
      [ ! -d /var/${userName}/openresty/nginx/logs ] && mkdir -p /var/${userName}/openresty/nginx/logs && chown -R ${userName}:${userName} /var/${userName}/openresty/nginx/logs
      [ ! -d /var/${userName}/openresty/nginx/html/dumpfiles ] && mkdir -p /var/${userName}/openresty/nginx/html/dumpfiles && chown -R nobody:nogroup /var/${userName}/openresty/nginx/html/dumpfiles
      sed '/^\s*#.*$/d; /^\s*$/d; s/^/export /g' /var/${userName}/openresty/env > /var/${userName}/openresty/env.export
      # shellcheck source=/dev/null
      . /var/${userName}/openresty/env.export
      openresty -p "/var/${userName}/openresty/nginx" -c "/var/${userName}/openresty/nginx/conf/nginx.conf" "$@"
    '';
  };

  # following define the service
  my-openresty-service = { lib, pkgs, config, ... }:
    let cfg = config.services.my-openresty;
    in {
      options = {
        services.my-openresty = {
          enable = lib.mkOption {
            default = true;
            type = lib.types.bool;
            description = "enable to generate a config to start the service";
          };
          # add extra options here, if any
        };
      };
      config = lib.mkIf cfg.enable {
        systemd.services.my-openresty = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "my openresty service";
          serviceConfig = {
            Type = "forking";
            ExecStartPre = ''
              ${my-openresty-bin-sh}/bin/my-openresty-bin-sh -t -q -g "daemon on; master_process on;"
            '';
            ExecStart = ''
              ${my-openresty-bin-sh}/bin/my-openresty-bin-sh -g "daemon on; master_process on;"
            '';
            ExecReload = ''
              ${my-openresty-bin-sh}/bin/my-openresty-bin-sh -g "daemon on; master_process on;" -s reload
            '';
            ExecStop = "${my-openresty-bin-sh}/bin/my-openresty-bin-sh -s stop";
            Restart = "on-failure";
          };
        };
      };
    };
  mk-my-openresty-service-unit = nPkgs.writeText "my-openresty.service"
    (nPkgs.nixos ({ lib, pkgs, config, ... }: {
      imports = [ my-openresty-service ];
    })).config.systemd.units."my-openresty.service".text;

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

  mk-my-postgresql-service-systemd-setup-or-bin-sh = if genSystemdUnit then
    (nPkgs.setupSystemdUnits {
      namespace = "my-postgresql";
      units = {
        "my-postgresql.service" = mk-my-postgresql-service-unit
          + /postgresql.service;
      };
    })
  else
    { };

  mk-my-postgrest-service-systemd-setup-or-bin-sh = if genSystemdUnit then
    (nPkgs.setupSystemdUnits {
      namespace = "my-postgrest";
      units = { "my-postgrest.service" = mk-my-postgrest-service-unit; };
    })
  else
    my-postgrest-bin-sh;

  mk-my-openresty-service-systemd-setup-or-bin-sh = if genSystemdUnit then
    (nPkgs.setupSystemdUnits {
      namespace = "my-openresty";
      units = { "my-openresty.service" = mk-my-openresty-service-unit; };
    })
  else
    my-openresty-bin-sh;

  mk-{{name}}-service-systemd-setup-or-bin-sh = if genSystemdUnit then
    (nPkgs.setupSystemdUnits {
      namespace = "{{name}}";
      units = {
        "{{name}}.service" = mk-{{name}}-service-unit;
      };
    }) else {{name}}-bin-sh;
}
