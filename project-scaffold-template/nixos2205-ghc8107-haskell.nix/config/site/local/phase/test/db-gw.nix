{ config, lib, pkgs, env, ... }:

{
  imports = [ ./db.nix ];

  config = lib.mkIf config.db-gw.enable {
    db-gw = {
      db-uri = ''
        "postgres://${config.db.apiSchemaUser}:${config.db.apiSchemaPassword}@${config.db.host}:${
          toString config.db.port
        }/${config.db.database}"'';
      db-schema = ''"${config.db.apiSchema}"'';
      db-anon-role = ''"${config.db.anonRole}"'';
      jwt-secret = ''"${config.db.jwtSecret}"'';
      server-host = ''"${env.db-gw.ipAddress}"'';
      server-port = 3000;
    };
  };
}
