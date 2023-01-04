{ config, lib, pkgs, env, ... }:

{
  imports = [ ./db-gw.nix ];

  config = lib.mkIf config.api-gw.enable {
    api-gw = {
      docRoot = "${env.api-gw.runDir}/web";
      uploadHome = "${env.api-gw.runDir}/upload";
      logDir = "${env.api-gw.runDir}/log";
      cacheDir = "${env.api-gw.runDir}/cache";
      serverName = "${env.api-gw.dnsName}";
      listenPort = 80;
      uploadMaxSize = "4096M";
      resolver = "local=on";
      postgrest-host = "${config.db-gw.server-host}";
      postgrest-port = config.db-gw.server-port;
    };
  };
}
