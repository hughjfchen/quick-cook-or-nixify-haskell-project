{ config, lib, pkgs, ... }:

{
  imports = [ ];

  options = {
    hostName = lib.mkOption {
      type = lib.types.str;
      default = null;
      example = "myhost";
      description = ''
        The host name of the deploy target host.
      '';
    };
    dnsName = lib.mkOption {
      type = lib.types.str;
      default = null;
      example = "myhost.subdomain.com";
      description = ''
        The DNS name of the deploy target host.
      '';
    };
    ipAddress = lib.mkOption {
      type = lib.types.str;
      default = null;
      example = "10.1.23.222";
      description = ''
        The IP address of the deploy target host.
      '';
    };
    processUser = lib.mkOption {
      type = lib.types.str;
      default = null;
      example = "myuser";
      description = ''
        The user name under which the service or program will run on the target host.
      '';
    };
    isSystemdService = lib.mkOption {
      type = lib.types.bool;
      default = true;
      example = true;
      description = ''
        If the service should be a systemd service on the target host?
      '';
    };
    runDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/myuser/run";
      example = "/var/myuser/run";
      description = ''
        The directory the runtime intermedia files should be put under on the target host.
      '';
    };
    dataDir = lib.mkOption {
      type = lib.types.str;
      default = "/var/myuser/data";
      example = "/var/myuser/data";
      description = ''
        The directory the data files should be put under on the target host.
      '';
    };
  };
}
