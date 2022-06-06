{ pkgs, ... }: {
  config.project.name = "java-analyzer-uploader";
  config.services = {

    db-service = {
      image.enableRecommendedContents = true;
      service.useHostStore = true;
      service.volumes = [
        "/tmp/mytmpdb:/var/data:rw"
        "/tmp/mytmpdb/run:/run:rw"
        "/tmp/db-init.d:/docker-entrypoint-initdb.d:ro"
        "${pkgs.pg-group}:/etc/group:ro"
        "${pkgs.pg-passwd}:/etc/passwd:ro"
      ];
      service.entrypoint =
        "${pkgs.pg-docker-entrypoint-script-wrapp}/bin/pg-docker-entrypoint.sh";
      service.command = [ "postgres" "-i" ];
      service.ports = [
        "5432:5432" # host:container
      ];
      service.environment.PGDATA = "/var/data/mypgdb";
      service.environment.POSTGRES_DATADIR = "/var/data";
      service.environment.POSTGRES_USER = "pguser";
      service.environment.POSTGRES_PASSWORD = "pgpass";
      service.environment.POSTGRES_DB = "test_upload_perf_db";
      service.stop_signal = "SIGINT";
    };

    postgrest-service = {
      image.enableRecommendedContents = true;
      service.useHostStore = true;
      service.command = [
        "${pkgs.haskellPackages.postgrest.outPath}/bin/postgrest"
        "${pkgs.postgrest-config-local-file}"
      ];
      #[ "${pkgs.haskellPackages.postgrest.outPath}/bin/postgrest" ];
      service.ports = [
        "3000:3000" # host:container
      ];
      service.environment.PGRST_DB_URI =
        "postgres://pguser:pgpass@db-service:5432/test_upload_perf_db";
      service.environment.PGRST_DB_SCHEMA = "test_schema";
      service.environment.PGRST_DB_ANON_ROLE = "anonymous";
      service.environment.PGRST_DB_POOL = "10";
      service.environment.PGRST_JWT_SECRET = "reallyreallyreallyreallyverysafe";
      #service.environment.PGRST_MAX_ROWS = "";
      #service.environment.PGRST_PRE_REQUEST = "";
      #service.environment.PGRST_SERVER_PROXY_URI = "";
      service.stop_signal = "SIGINT";
      service.depends_on = [ "db-service" ];
      service.links = [ "db-service:db-service" ];
    };

    java-analyzer-uploader-service = {
      image.enableRecommendedContents = true;
      service.useHostStore = true;
      service.command = [ "${pkgs.geckodriver-wrapp}/bin/geckodriver" ];
      service.volumes = [ "/tmp:/tmp:rw" ];
      service.defaultExec = [
        "sh"
        "-c"
        "${pkgs.java-analyzer-uploader.java-analyzer-uploader-exe.outPath}/bin/java-analyzer-uploader --pageUrl https://en.swissquote.com/forex/pricing/trading-conditions --uploadUrl http://postgrest-service:3000/api/updateSQTradingCondition"
      ];
      service.ports = [ ];
      service.environment.ENV1 =
        "${pkgs.java-analyzer-uploader.java-analyzer-uploader-exe.outPath}/share/doc";
      service.environment.LC_CTYPE = "en_US.UTF-8";
      service.environment.LC_LANG = "en_US.UTF-8";
      service.environment.LC_ALL = "en_US.UTF-8";
      service.stop_signal = "SIGINT";
      service.depends_on = [ "db-service" "postgrest-service" ];
      service.links =
        [ "db-service:db-service" "postgrest-service:postgrest-service" ];
    };
  };
}
