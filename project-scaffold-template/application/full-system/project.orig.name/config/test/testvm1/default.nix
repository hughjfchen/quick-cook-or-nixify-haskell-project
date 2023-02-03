{ pkgs }:
let
  inherit pkgs;
in
  {
    "postgresql.db_uri" = "postgres://pguser:pgpass@db-service:5432/my-test-db";
    "postgresql.db_user" = "pguser";
    "postgresql.db_pass" = "pgpass";
    "postgresql.db_pool" = 10;
    "postgrest.postgrest_anon_role" = "anonymouse";
    "postgrest.jwt_secret" = "veryveryveryveryveryverysecret";
  }
