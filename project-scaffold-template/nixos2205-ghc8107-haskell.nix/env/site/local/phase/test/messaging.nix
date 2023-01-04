{ config, lib, pkgs, ... }: {
  imports = [ ];

  config = {
    messaging = rec {
      hostName = "localhost";
      dnsName = "localhost";
      ipAddress = "127.0.0.1";
      processUser = "jamsguser";
      isSystemdService = true;
      runDir = "/var/${processUser}/run";
      dataDir = "/var/${processUser}/data";
    };
  };
}
