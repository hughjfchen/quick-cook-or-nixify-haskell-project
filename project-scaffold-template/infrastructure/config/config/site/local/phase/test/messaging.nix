{ config, lib, pkgs, env, ... }:

{
  imports = [ ];

  config = lib.mkIf config.messaging.enable {
    messaging = {
      host = "${env.messaging.ipAddress}";
      port = 3838;
      user = "mymsguser";
      password = "mymsgpassword";
      topic = "mytopic";
      queue = "myqueue";
    };
  };
}
