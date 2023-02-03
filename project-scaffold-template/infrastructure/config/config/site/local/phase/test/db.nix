{ config, lib, pkgs, env, ... }:

{
  imports = [ ];

  config = lib.mkIf config.db.enable {
    db = {
      host = "${env.db.ipAddress}";
      port = 5432;
      dataSchemaUser = "mydatauser";
      dataSchemaPassword = "mydatapassword";
      apiSchemaUser = "myapiuser";
      apiSchemaPassword = "myapipassword";
      database = "mydb";
      anonRole = "anonymous";
      dataSchema = "data";
      apiSchema = "api";
      jwtSecret = "reallyreallyreallyreallyreallyreallysecret";
      jwtLifeTime = 3600;
    };
  };
}
