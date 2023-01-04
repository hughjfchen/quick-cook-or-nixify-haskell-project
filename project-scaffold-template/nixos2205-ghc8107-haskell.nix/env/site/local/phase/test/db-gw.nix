{ config, lib, pkgs, ... }: {
  imports = [ ];

  config = {
    db-gw = rec {
      hostName = "localhost";
      dnsName = "localhost";
      ipAddress = "127.0.0.1";
      processUser = "jadbgwuser";
      isSystemdService = true;
      runDir = "/var/${processUser}/run";
      dataDir = "/var/${processUser}/data";
    };
  };
}
