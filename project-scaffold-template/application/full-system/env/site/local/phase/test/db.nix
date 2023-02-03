{ config, lib, pkgs, ... }: {
  imports = [ ];

  config = {
    db = rec {
      hostName = "localhost";
      dnsName = "";
      ipAddress = "127.0.0.1";
      processUser = "jadbuser";
      isSystemdService = true;
      runDir = "/var/${processUser}/run";
      dataDir = "/var/${processUser}/data";
    };
  };
}
