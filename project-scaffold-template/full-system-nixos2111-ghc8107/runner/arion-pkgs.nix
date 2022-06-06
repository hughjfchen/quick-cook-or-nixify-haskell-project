let
  mypkgs = import ./default.nix { };
in
mypkgs.pkgs // rec { java-analyzer-runner = mypkgs;
                     pg-group = mypkgs.pkgs.writeText "pg-group" ''
                       postgres:x:9999:
                     '';
                     pg-passwd = mypkgs.pkgs.writeText "pg-passwd" ''
                       postgres:x:9999:9999:PostgreSQL:/var/data/pgdata:/bin/false
                     '';
                     pg-docker-entrypoint-script-paths = [ pg-docker-entrypoint-script
                                                           mypkgs.pkgs.bash
                                                           mypkgs.pkgs.coreutils
                                                           mypkgs.pkgs.shadow
                                                           mypkgs.pkgs.findutils
                                                           mypkgs.pkgs.util-linux
                                                           mypkgs.pkgs.gosu
                                                           mypkgs.pkgs.nss_wrapper
                                                           mypkgs.pkgs.postgresql
                                                         ];
                     pg-docker-entrypoint-script = mypkgs.pkgs.writeScriptBin "pg-docker-entrypoint.sh" (builtins.readFile ./pg-docker-entrypoint.sh);
                     pg-docker-entrypoint-script-wrapp = mypkgs.pkgs.symlinkJoin {
                       name = "pg-docker-entrypoint-script-wrapp";
                       paths = pg-docker-entrypoint-script-paths;
                       buildInputs = [ mypkgs.pkgs.makeWrapper ];
                       postBuild = "wrapProgram $out/bin/pg-docker-entrypoint.sh --prefix PATH : $out/bin";
                     };
                     geckodriver-wrapp = mypkgs.pkgs.symlinkJoin {
                       name = "geckodriver-wrapp";
                       paths = [ mypkgs.pkgs.locale mypkgs.pkgs.geckodriver mypkgs.pkgs.firefox ];
                       buildInputs = [ mypkgs.pkgs.makeWrapper ];
                       postBuild = "wrapProgram $out/bin/geckodriver --prefix PATH : $out/bin";
                     };
                     chromedriver-wrapp = mypkgs.pkgs.symlinkJoin {
                       name = "chromedriver-wrapp";
                       paths = [ mypkgs.pkgs.chromium ];
                       buildInputs = [ mypkgs.pkgs.makeWrapper ];
                       postBuild = "wrapProgram $out/bin/chromedriver --prefix PATH : $out/bin";
                     };
                     postgrest-config-local = mypkgs.pkgs.lib.generators.toKeyValue {} {
                      db-uri = "\"postgres://pguser:pgpass@db-service:5432/my-test-upload-db\"";
                      db-schema = "\"app\"";
                      db-anon-role = "\"anonymous\"";
                      db-pool = 10;
                      jwt-secret = "\"reallyreallyreallyreallyverysafe\"";
                     };
                     postgrest-config-local-file = mypkgs.pkgs.writeText "postgrest-config-local" postgrest-config-local;
                   }
