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
  pkgName = "my-postgrest";
  innerTarballName = lib.concatStringsSep "." [ (lib.concatStringsSep "-" [ pkgName site phase ]) "tar" "gz" ];

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  release-utils = import ./release-utils.nix { inherit lib; pkgs = nPkgs; };

  # the deployment env
  my-postgrest-env = (import ../env/site/${site}/phase/${phase}/env.nix { pkgs = nPkgs; }).env;

  # dependent config
  my-postgrest-config = (import ../config/site/${site}/phase/${phase}/config.nix { pkgs = nPkgs; env = my-postgrest-env; }).config;

  # my services dependencies
  # following define the service
  my-postgrest-config-kv = nPkgs.writeTextFile {
    name = lib.concatStringsSep "-" [ pkgName "config" ];
    # generate the key = value format config, refer to the lib.generators for other formats
    text = (lib.generators.toKeyValue {}) my-postgrest-config.db-gw;
  };

  # my services dependencies
  my-postgrest-bin-sh = nPkgs.writeShellApplication {
    name = lib.concatStringsSep "-" [ pkgName "bin" "sh" ];
    runtimeInputs = [ nPkgs.haskellPackages.postgrest ];
    text = ''
      postgrest ${my-postgrest-config-kv} "$@"
    '';
  };

  # following define the service
  my-postgrest-service = { lib, pkgs, config, ... }:
    let cfg = config.services.my-postgrest;
    in {
      options = lib.attrsets.setAttrByPath [ "services" pkgName ]
        {
          enable = lib.mkOption {
            default = true;
            type = lib.types.bool;
            description = "enable to generate a config to start the service";
          };
          # add extra options here, if any
        };
      config = lib.mkIf cfg.enable (lib.attrsets.setAttrByPath [ "systemd" "services" pkgName ]
        {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          description = "my postgrest service";
          serviceConfig = {
            Type = "simple";
            User = "${my-postgrest-env.db-gw.processUser}";
            ExecStart = "${my-postgrest-bin-sh}/bin/${my-postgrest-bin-sh.name}";
            Restart = "on-failure";
          };
        });
    };

  serviceNameKey = lib.concatStringsSep "." [ pkgName "service" ];
  serviceNameUnit = lib.attrsets.setAttrByPath [ serviceNameKey ] mk-my-postgrest-service-unit;

  mk-my-postgrest-service-unit = nPkgs.writeText serviceNameKey
    (lib.attrsets.getAttrFromPath [ "config" "systemd" "units" serviceNameKey "text" ] (nPkgs.nixos ({ lib, pkgs, config, ... }: {
      imports = [ my-postgrest-service ];
    })));

in rec {
  inherit nativePkgs pkgs my-postgrest-config-kv;

  mk-my-postgrest-service-systemd-setup-or-bin-sh = if my-postgrest-env.db-gw.isSystemdService then
    (nPkgs.setupSystemdUnits {
      namespace = pkgName;
      units = serviceNameUnit;
    })
  else
    my-postgrest-bin-sh;

  mk-my-postgrest-reference = nPkgs.writeReferencesToFile mk-my-postgrest-service-systemd-setup-or-bin-sh;
  mk-my-postgrest-deploy-sh = release-utils.mk-deploy-sh {
    env = my-postgrest-env.db-gw;
    payloadPath =  mk-my-postgrest-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "postgrest";
  };
  mk-my-postgrest-cleanup-sh = release-utils.mk-cleanup-sh {
    env = my-postgrest-env.db-gw;
    payloadPath =  mk-my-postgrest-service-systemd-setup-or-bin-sh;
    inherit innerTarballName;
    execName = "postgrest";
  };
  mk-my-release-packer = release-utils.mk-release-packer {
    referencePath = mk-my-postgrest-reference;
    component = pkgName;
    inherit site phase innerTarballName;
    deployScript = mk-my-postgrest-deploy-sh;
    cleanupScript = mk-my-postgrest-cleanup-sh;
  };

}
