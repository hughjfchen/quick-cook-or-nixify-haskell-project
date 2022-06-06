{ nativePkgs ? import ./default.nix { }, # the native package set
pkgs ? import ./cross-build.nix { }
, # the package set for corss build, we're especially interested in the fully static binary
site , # the site for release, the binary would deploy to it finally
phase, # the phase for release, must be "local", "test" and "production"
}:
let
  nPkgs = nativePkgs.pkgs;
  sPkgs = pkgs.x86-musl64; # for the fully static build
  lib = nPkgs.lib; # lib functions from the native package set
  pkgName = "my-openresty";
  innerTarballName = lib.concatStringsSep "." [ (lib.concatStringsSep "-" [ pkgName site phase ]) "tar" "gz" ];

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  release-utils = import ./release-utils.nix { inherit lib; pkgs = nPkgs; };

  # the deployment env
  my-openresty-env = (import ../env/site/${site}/phase/${phase}/env.nix { pkgs = nPkgs; }).env;

  # dependent config
  my-openresty-config = (import ../config/site/${site}/phase/${phase}/config.nix { pkgs = nPkgs; env = my-openresty-env; }).config;

  # the frontend, comment out for now.
  #my-frontend-distributable = (import ../frontend/release.nix { pkgs = nPkgs; inherit releasePhase releaseHost genSystemdUnit userName dockerOnTarget; }).my-frontend-distributable;

  # my services dependencies
  # following define the service
  my-openresty-src = nPkgs.stdenv.mkDerivation {
    src = ./.;
    name = lib.concatStringsSep "-" [ pkgName "src" ];
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      mkdir -p $out/lua
      mkdir -p $out/nginx
      mkdir -p $out/nginx/web
      mkdir -p $out/lualib
      cp -R $src/openresty/lua/* $out/lua/

      # for some nginx config diretives, there can not be an evironement variable set_by_lua_block,
      # so we must replace them before they can be loaded by nginx
      # NOTICE: there must not be sub directories under the dir the replaced files located
      mkdir -p $out/nginx/conf
      for confFile in $src/openresty/nginx/conf/*
      do
        sed "s/\$OPENRESTY_SERVER_NAME/${my-openresty-config.api-gw.serverName}/g; s/\$OPENRESTY_LISTEN_PORT/${toString my-openresty-config.api-gw.listenPort}/g; s/\$OPENRESTY_RESOLVER/${my-openresty-config.api-gw.resolver}/g; s/\$OPENRESTY_RUN_DIR/${lib.strings.escape ["/"] my-openresty-env.api-gw.runDir}/g; s/\$OPENRESTY_UPLOAD_MAX_SIZE/${toString my-openresty-config.api-gw.uploadMaxSize}/g" $confFile > $out/nginx/conf/$(basename $confFile)
      done

      ln -s $out/lua $out/lualib/user_code
      ln -s $out/lualib $out/nginx/lualib

      # prepare the export environment variables
      {
        echo 'export DB_HOST=${my-openresty-config.db.host}'
        echo 'export DB_PORT=${toString my-openresty-config.db.port}'
        echo 'export DB_USER=${my-openresty-config.db.apiSchemaUser}'
        echo 'export DB_PASS=${my-openresty-config.db.apiSchemaPassword}'
        echo 'export DB_NAME=${my-openresty-config.db.database}'
        echo 'export DB_SCHEMA=${my-openresty-config.db.apiSchema}'
        echo 'export JWT_SECRET=${my-openresty-config.db.jwtSecret}'
        echo 'export POSTGREST_HOST=${my-openresty-config.db-gw.server-host}'
        echo 'export POSTGREST_PORT=${toString my-openresty-config.db-gw.server-port}'
        echo 'export OPENRESTY_DOC_ROOT=${my-openresty-config.api-gw.docRoot}'
      }  > $out/env.export

    '';
  };

  # my services dependencies
  my-openresty-bin-sh = nPkgs.writeShellApplication {
    name = lib.concatStringsSep "-" [ pkgName "bin" "sh" ];
    runtimeInputs = [ nPkgs.openresty ];
    text = ''
      [ ! -d ${my-openresty-env.api-gw.runDir}/log ] && mkdir -p ${my-openresty-env.api-gw.runDir}/log && chown -R ${my-openresty-env.api-gw.processUser}:${my-openresty-env.api-gw.processUser} ${my-openresty-env.api-gw.runDir}/log
      [ ! -d ${my-openresty-env.api-gw.runDir}/client_body ] && mkdir -p ${my-openresty-env.api-gw.runDir}/client_body && chown -R ${my-openresty-env.api-gw.processUser}:${my-openresty-env.api-gw.processUser} ${my-openresty-env.api-gw.runDir}/client_body
      [ ! -d ${my-openresty-config.api-gw.docRoot}/dumpfiles ] && mkdir -p ${my-openresty-config.api-gw.docRoot}/dumpfiles && chown -R nobody:nogroup ${my-openresty-config.api-gw.docRoot}/dumpfiles
      [ ! -d ${my-openresty-config.api-gw.docRoot}/parsereports ] && mkdir -p ${my-openresty-config.api-gw.docRoot}/parsereports && chown -R nobody:nogroup ${my-openresty-config.api-gw.docRoot}/parsereports
      # shellcheck source=/dev/null
      . ${my-openresty-src}/env.export
      openresty -p "${my-openresty-src}/nginx" -c "${my-openresty-src}/nginx/conf/nginx.conf" "$@"
    '';
  };

  # following define the service
  my-openresty-service = { lib, pkgs, config, ... }:
    {
      options = lib.attrsets.setAttrByPath [ "services" pkgName ] {
          enable = lib.mkOption {
            default = true;
            type = lib.types.bool;
            description = "enable to generate a config to start the service";
          };
          # add extra options here, if any
        };
      config = lib.mkIf (lib.attrsets.getAttrFromPath [ pkgName "enable" ] config.services)
        (lib.attrsets.setAttrByPath [ "systemd" "services" pkgName ] {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "${pkgName} service";
          serviceConfig = {
            Type = "forking";
            ExecStartPre = ''
              ${my-openresty-bin-sh}/bin/${my-openresty-bin-sh.name} -t -q -g "daemon on; master_process on;"
            '';
            ExecStart = ''
              ${my-openresty-bin-sh}/bin/${my-openresty-bin-sh.name} -g "daemon on; master_process on;"
            '';
            ExecReload = ''
              ${my-openresty-bin-sh}/bin/${my-openresty-bin-sh.name} -g "daemon on; master_process on;" -s reload
            '';
            ExecStop = "${my-openresty-bin-sh}/bin/${my-openresty-bin-sh.name} -s stop";
            Restart = "on-failure";
          };
        });
    };

  serviceNameKey = lib.concatStringsSep "." [ pkgName "service" ];
  serviceNameUnit = lib.attrsets.setAttrByPath [ serviceNameKey ] mk-my-openresty-service-unit;

  mk-my-openresty-service-unit = nPkgs.writeText serviceNameKey
    (lib.attrsets.getAttrFromPath [ "config" "systemd" "units" serviceNameKey "text" ] (nPkgs.nixos ({ lib, pkgs, config, ... }: {
      imports = [ my-openresty-service ];
    })));

in rec {
  inherit nativePkgs pkgs my-openresty-config;

  mk-my-openresty-service-systemd-setup-or-bin-sh = if my-openresty-env.api-gw.isSystemdService then
    (nPkgs.setupSystemdUnits {
      namespace = pkgName;
      units = serviceNameUnit;
    })
  else
    my-openresty-bin-sh;

  mk-my-openresty-reference = nPkgs.writeReferencesToFile mk-my-openresty-service-systemd-setup-or-bin-sh;
  mk-my-openresty-deploy-sh = release-utils.mk-deploy-sh {
    env = my-openresty-env.api-gw;
    payloadPath =  mk-my-openresty-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "openresty";
  };
  mk-my-openresty-cleanup-sh = release-utils.mk-cleanup-sh {
    env = my-openresty-env.api-gw;
    payloadPath =  mk-my-openresty-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "openresty";
  };
  mk-my-release-packer = release-utils.mk-release-packer {
    referencePath = mk-my-openresty-reference;
    component = pkgName;
    inherit site phase innerTarballName;
    deployScript = mk-my-openresty-deploy-sh;
    cleanupScript = mk-my-openresty-cleanup-sh;
  };
}
