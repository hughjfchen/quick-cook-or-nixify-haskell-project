{ config, lib, pkgs, ... }: {
  imports = [ ];

  config = {
    runner = rec {
      hostName = "localhost";
      dnsName = "localhost";
      ipAddress = "127.0.0.1";
      processUser = "jarunneruser";
      isSystemdService = false;
      runDir = "/var/${processUser}/run";
      dataDir = "/var/${processUser}/data";
    };
  };
}
