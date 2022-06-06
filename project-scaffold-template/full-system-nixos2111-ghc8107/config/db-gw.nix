{ config, lib, pkgs, env, ... }:

{
  imports = [ ./db.nix ];

  options = {
    db-gw = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          To enable the config for database gateway, i.e., postgrest.
        '';
      };
      db-uri = lib.mkOption {
        type = lib.types.str;
        default =
          "postgres://${config.db.apiSchemaUser}:${config.db.apiSchemaPassword}@${config.db.host}:${
            toString config.db.port
          }/${config.db.database}";
        description = ''
          The URI to access to the database.
        '';
      };
      db-schema = lib.mkOption {
        type = lib.types.str;
        default = "{config.db.apiSchema}";
        description = ''
          The schema name for the exposed API access.
          This schema mostly be used by postgrest.
        '';
      };
      db-anon-role = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.anonRole}";
        example = "anonymous";
        description = ''
          The anonymous role name when accessing to the database.
        '';
      };
      jwt-secret = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.jwtSecret}";
        example = "reallyreallyreallyreallyreallyreallysecret";
        description = ''
          The JWT secret to initialze the JWT token generation
          and verification.
        '';
      };
      server-host = lib.mkOption {
        type = lib.types.str;
        default = "${env.db-gw.ipAddress}";
        example = "localhost";
        description = ''
          The server name for the postgrest binding.
          It could also be an IP address.
        '';
      };
      server-port = lib.mkOption {
        type = lib.types.int;
        default = 3000;
        example = 3000;
        description = ''
          The server listening port for the postgrest binding.
          Default to 3000.
        '';
      };
    };
  };
}
