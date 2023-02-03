{ config, lib, pkgs, env, ... }:

{
  imports = [ ./db-gw.nix ];

  config = lib.mkIf config.api-gw.enable {
    api-gw = {
      docRoot = "${env.api-gw.runDir}/web";
      serverName = "${env.api-gw.dnsName}";
      listenPort = 80;
      uploadMaxSize = "4096M";
      resolver = "local=on";
      postgrest-host = "${config.db-gw.server-host}";
      postgrest-port = config.db-gw.server-port;
    };
  };
}
