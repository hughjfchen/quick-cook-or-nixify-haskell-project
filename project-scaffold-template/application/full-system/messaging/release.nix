{ nativePkgs ? import ./default.nix { }, # the native package set
pkgs ? import ./cross-build.nix { }
, # the package set for corss build, we're especially interested in the fully static binary
site, # the site for release, the binary would deploy to it finally
phase, # the phase for release, must be "local", "test" and "production"
}:
let
  nPkgs = nativePkgs.pkgs;
  sPkgs = pkgs.x86-musl64; # for the fully static build
  lib = nPkgs.lib; # lib functions from the native package set
  pkgName = "my-rabbitmq";
  innerTarballName = lib.concatStringsSep "." [
    (lib.concatStringsSep "-" [ pkgName site phase ])
    "tar"
    "gz"
  ];

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  deploy-packer = import (builtins.fetchGit {
    url = "https://github.com/hughjfchen/deploy-packer";
  }) {
    inherit lib;
    pkgs = nPkgs;
  };

  # the deployment env
  my-messaging-env-orig = (import
    (builtins.fetchGit { url = "https://github.com/hughjfchen/deploy-env"; }) {
      pkgs = nPkgs;
      modules = [
        ../env/site/${site}/phase/${phase}/db.nix
        ../env/site/${site}/phase/${phase}/db-gw.nix
        ../env/site/${site}/phase/${phase}/api-gw.nix
        ../env/site/${site}/phase/${phase}/messaging.nix
      ];
    }).env;

  # app and dependent config
  my-messaging-config = (import (builtins.fetchGit {
    url = "https://github.com/hughjfchen/deploy-config";
  }) {
    pkgs = nPkgs;
    modules = [
      ../config/site/${site}/phase/${phase}/db.nix
      ../config/site/${site}/phase/${phase}/db-gw.nix
      ../config/site/${site}/phase/${phase}/api-gw.nix
      ../config/site/${site}/phase/${phase}/messaging.nix
    ];
    env = my-messaging-env;
  }).config;

  # NOTICE: the rabbitmq process user must be rabbitmq
  my-messaging-env = lib.attrsets.recursiveUpdate my-messaging-env-orig {
    messaging.processUser = "rabbitmq";
    messaging.runDir = "/var/rabbitmq/run";
    messaging.dataDir = "/var/rabbitmq/data";
  };

  # my services dependencies
  # following define the service
  mk-my-rabbitmq-service-unit = (nPkgs.nixos ({ lib, pkgs, config, ... }: {
    config.services.rabbitmq = {
      enable = true;
      package = nPkgs.rabbitmq-server;
      listenAddress = "${my-messaging-env.messaging.ipAddress}";
      port = 5672;
      dataDir = "${my-messaging-env.messaging.dataDir}";
      cookie = "";
      configItems = {
        "num_acceptors.tcp" = "10";
        "channel_max" = "2047";
        "max_message_size" = "134217728";
        "log.dir" = "${my-messaging-env.messaging.runDir}";
        "log.file.level" = "info";
        "default_user" = "${my-messaging-env.messaging.processUser}";
        "default_pass" = "Passw0rd";
        "default_user_tags.administrator" = "true";
        "default_permissions.configure" = ".*";
        "default_permissions.read" = ".*";
        "default_permissions.write" = ".*";
      };
      config = "";
      plugins = [ ];
      pluginDirs = [ ];
      managementPlugin = {
        enable = true;
        port = 15672;
      };

    };
    # Even we set the log directory in the config file, we still need to set the environment variable
    # Or rabbitmq will complain cannot open the /var/logs/rabbitmqxx.log file
    # So we quickly fix it by adding an environment variable
    config.systemd.services.rabbitmq.environment = lib.mkAfter {
      RABBITMQ_LOG_BASE = "${my-messaging-env.messaging.runDir}";
    };
  })).config.systemd.units."rabbitmq.service".unit;

  serviceNameKey = lib.concatStringsSep "." [ pkgName "service" ];
  serviceNameUnit = lib.attrsets.setAttrByPath [ serviceNameKey ]
    (mk-my-rabbitmq-service-unit + /rabbitmq.service);
in rec {
  inherit nativePkgs pkgs my-messaging-config;

  mk-my-rabbitmq-service-systemd-setup-or-bin-sh =
    if my-messaging-env.messaging.isSystemdService then
      (nPkgs.setupSystemdUnits {
        namespace = pkgName;
        units = serviceNameUnit;
      })
    else
      { };

  mk-my-rabbitmq-service-systemd-unsetup-or-bin-sh =
    if my-messaging-env.messaging.isSystemdService then
      (deploy-packer.unsetup-systemd-service {
        namespace = pkgName;
        units = serviceNameUnit;
      })
    else
      { };
  # following derivation just to make sure the setup and unsetup will
  # be packed into the distribute tarball.
  setup-and-unsetup-or-bin-sh = nPkgs.symlinkJoin {
    name = "my-rabbitmq-setup-and-unsetup";
    paths = [
      mk-my-rabbitmq-service-systemd-setup-or-bin-sh
      mk-my-rabbitmq-service-systemd-unsetup-or-bin-sh
    ];
  };

  mk-my-rabbitmq-reference =
    nPkgs.writeReferencesToFile setup-and-unsetup-or-bin-sh;
  mk-my-rabbitmq-deploy-sh = deploy-packer.mk-deploy-sh {
    env = my-messaging-env.messaging;
    payloadPath = setup-and-unsetup-or-bin-sh;
    inherit innerTarballName;
    execName = "rabbitmq";
  };
  mk-my-rabbitmq-cleanup-sh = deploy-packer.mk-cleanup-sh {
    env = my-messaging-env.messaging;
    payloadPath = setup-and-unsetup-or-bin-sh;
    inherit innerTarballName;
    execName = "rabbitmq";
  };
  mk-my-release-packer = deploy-packer.mk-release-packer {
    referencePath = mk-my-rabbitmq-reference;
    component = pkgName;
    inherit site phase innerTarballName;
    deployScript = mk-my-rabbitmq-deploy-sh;
    cleanupScript = mk-my-rabbitmq-cleanup-sh;
  };
}
