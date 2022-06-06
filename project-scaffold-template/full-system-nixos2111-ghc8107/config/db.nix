{ config, lib, pkgs, env, ... }:

{
  imports = [ ];

  options = {
    db = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          To enable the config for database.
        '';
      };
      host = lib.mkOption {
        type = lib.types.str;
        default = "${env.db.ipAddress}";
        description = ''
          The database host.
        '';
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 5432;
        description = ''
          The listening port for database.
        '';
      };
      database = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "mydb";
        description = ''
          The database name.
        '';
      };
      dataSchema = lib.mkOption {
        type = lib.types.str;
        default = "data";
        description = ''
          The schema name for all real data put under.
          This backend services will use this schema
          to access the database.
        '';
      };
      dataSchemaUser = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "myuser";
        description = ''
          The normal user name to connect to the database.
          Notice this is not the super user of the database.
          The super user of the database always is postgres.
        '';
      };
      dataSchemaPassword = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "ThisIsSecret";
        description = ''
          The normal pasword is used to connect to the database.
        '';
      };
      apiSchema = lib.mkOption {
        type = lib.types.str;
        default = "api";
        description = ''
          The schema name for the exposed API access.
          This schema mostly be used by postgrest.
        '';
      };
      apiSchemaUser = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "myuser";
        description = ''
          The gateway user name to connect to the database.
          Notice this is not the super user of the database.
          The super user of the database always is postgres.
        '';
      };
      apiSchemaPassword = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "ThisIsSecret";
        description = ''
          The gateway user's pasword is used to connect to the database.
        '';
      };
      anonRole = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "anonymous";
        description = ''
          The anonymous role name when accessing to the database.
        '';
      };
      jwtSecret = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "reallyreallyreallyreallyverysafe";
        description = ''
          The JWT secret to initialze the JWT token generation
          and verification.
        '';
      };
      jwtLifeTime = lib.mkOption {
        type = lib.types.int;
        default = 3600;
        example = 3600;
        description = ''
          The JWT token lifetime before expired.
        '';
      };
    };
  };
}
