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
  pkgName = "my-postgresql";
  innerTarballName = lib.concatStringsSep "." [ (lib.concatStringsSep "-" [ pkgName site phase ]) "tar" "gz" ];

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  release-utils = import ./release-utils.nix { inherit lib; pkgs = nPkgs; };

  # the deployment env
  my-db-env-orig = (import ../env/site/${site}/phase/${phase}/env.nix { pkgs = nPkgs; }).env;
  # NOTICE: the postgresql process user must be postgres
  my-db-env = lib.attrsets.recursiveUpdate my-db-env-orig { db.processUser = "postgres";
                                                            db.runDir = "/var/postgres/run";
                                                            db.dataDir = "/var/postgres/data";
                                                          };
  # the config
  my-db-config = (import ../config/site/${site}/phase/${phase}/config.nix { pkgs = nPkgs; env = my-db-env; }).config;
  
  # my services dependencies
  # following define the service
  my-db-init-script = nPkgs.stdenv.mkDerivation {
    src = ./.;
    name = "my-db-init-script";
    dontBuild = true;
    dontUnpack = true;
    installPhase = ''
      mkdir -p $out
      cp -R $src/sql/libs $out/
      cp -R $src/sql/data $out/
      cp -R $src/sql/authorization $out/
      cp -R $src/sql/sample_data $out/

      # replace the environment for the entry point file of the sql initialization
      sed "s/\$DB_ANON_ROLE/${my-db-config.db.anonRole}/g; s/\$DB_API_SCHEMA/${my-db-config.db.apiSchema}/g; s/\$DB_DATA_SCHEMA/${my-db-config.db.dataSchema}/g; s/\$DB_DATA_USER/${my-db-config.db.dataSchemaUser}/g; s/\$DB_DATA_PASS/${my-db-config.db.dataSchemaPassword}/g; s/\$DB_API_USER/${my-db-config.db.apiSchemaUser}/g; s/\$DB_API_PASS/${my-db-config.db.apiSchemaPassword}/g; s/\$DB_NAME/${my-db-config.db.database}/g; s/\$JWT_SECRET/${my-db-config.db.jwtSecret}/g; s/\$JWT_LIFETIME/${toString my-db-config.db.jwtLifeTime}/g" $src/sql/init.sql > $out/init.sql

      # for sql select statement, there can not be an evironement variable, so we must replace them before they can be loaded by psql
      # NOTICE: there must not be subdirectories under the directory the replaced files located
      mkdir -p $out/api
      for sqlFile in $src/sql/api/*
      do
        sed "s/\$DB_DATA_SCHEMA/${my-db-config.db.dataSchema}/g" $sqlFile > $out/api/$(basename $sqlFile)
      done
    '';
  };
  mk-my-postgresql-service-unit = (nPkgs.nixos ({ lib, pkgs, config, ... }: {
    config.services.postgresql = {
      enable = true;
      package = nPkgs.postgresql_9_6;
      port = 5432;
      dataDir = "${my-db-env.db.dataDir}";
      initdbArgs = [ "--encoding=UTF8" ];
      initialScript = my-db-init-script + /init.sql;
      ensureDatabases = [ ];
      ensureUsers = [ ];
      enableTCPIP = true;
      authentication = nPkgs.lib.mkOverride 10 ''
        local all all trust
        host all all all md5
      '';
      settings = { timezone = "Asia/Shanghai"; };
      #superUser = "postgres"; # read-only

    };
  })).config.systemd.units."postgresql.service".unit;

  serviceNameKey = lib.concatStringsSep "." [ pkgName "service" ];
  serviceNameUnit = lib.attrsets.setAttrByPath [ serviceNameKey ] (mk-my-postgresql-service-unit + /postgresql.service);
in rec {
  inherit nativePkgs pkgs my-db-config;

  mk-my-postgresql-service-systemd-setup-or-bin-sh = if my-db-env.db.isSystemdService then
    (nPkgs.setupSystemdUnits {
      namespace = pkgName;
      units = serviceNameUnit;
    })
  else
    { };
  mk-my-postgresql-reference = nPkgs.writeReferencesToFile mk-my-postgresql-service-systemd-setup-or-bin-sh;
  mk-my-postgresql-deploy-sh = release-utils.mk-deploy-sh {
    env = my-db-env.db;
    payloadPath =  mk-my-postgresql-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "postgres";
  };
  mk-my-postgresql-cleanup-sh = release-utils.mk-cleanup-sh {
    env = my-db-env.db;
    payloadPath =  mk-my-postgresql-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "postgres";
  };
  mk-my-release-packer = release-utils.mk-release-packer {
    referencePath = mk-my-postgresql-reference;
    component = pkgName;
    inherit site phase innerTarballName;
    deployScript = mk-my-postgresql-deploy-sh;
    cleanupScript = mk-my-postgresql-cleanup-sh;
  };
}
