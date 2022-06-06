{ config, lib, pkgs, env, ... }:

{
  imports = [ ];

  options = {
    messaging = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          To enable the config for messaging, i.e., RabbitMQ.
        '';
      };
      host = lib.mkOption {
        type = lib.types.str;
        default = "${env.messaging.ipAddress}";
        description = ''
          The messaging engine host.
        '';
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 3838;
        description = ''
          The listening port for messaging engine.
        '';
      };
      user = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "myuser";
        description = ''
          The user name to connect to the messaging engine.
        '';
      };
      password = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "ThisIsSecret";
        description = ''
          The pasword using using to connect to the messaging engine.
        '';
      };
      topic = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "mytopic";
        description = ''
          The topic name.
        '';
      };
      queue = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "myqueue";
        description = ''
          The queue name.
        '';
      };
    };
  };
}
