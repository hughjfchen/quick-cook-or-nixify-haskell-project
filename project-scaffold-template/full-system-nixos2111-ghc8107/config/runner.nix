{ config, lib, pkgs, env, ... }:

{
  imports = [ ./db.nix ];

  options = {
    runner = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          To enable the config for runner.
        '';
      };
      "command" = lib.mkOption {
        type = lib.types.enum [ "Start" "Stop" ];
        default = "Start";
        example = "Stop";
        description = ''
          The command for the runner.
        '';
      };
      "database.host" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.host}";
        example = "localhost";
        description = ''
          The database host.
        '';
      };
      "database.port" = lib.mkOption {
        type = lib.types.int;
        default = "${config.db.port}";
        description = ''
          The listening port for database.
        '';
      };
      "database.user" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.dataSchemaUser}";
        example = "myuser";
        description = ''
          The user name to connect to the database.
          Notice this is not the super user of the database.
          The super user of the database always is postgres.
        '';
      };
      "database.password" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.dataSchemaPassword}";
        example = "ThisIsSecret";
        description = ''
          The pasword using using to connect to the database.
        '';
      };
      "database.database" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.database}";
        example = "mydb";
        description = ''
          The database name.
        '';
      };
      "database.dataSchema" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.dataSchema}";
        description = ''
          The schema name for all real data put under.
          This backend services will use this schema
          to access the database.
        '';
      };
      "database.apiSchema" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.apiSchema}";
        description = ''
          The schema name for the exposed API access.
          This schema mostly be used by postgrest.
        '';
      };
      "pool.stripe" = lib.mkOption {
        type = lib.types.int;
        default = 1;
        example = 1;
        description = ''
          The strip of the database connection pool.
        '';
      };
      "pool.idletime" = lib.mkOption {
        type = lib.types.int;
        default = 1800;
        example = 1800;
        description = ''
          The idle time for the connection pool, in seconds.
        '';
      };
      "pool.size" = lib.mkOption {
        type = lib.types.int;
        default = 10;
        example = 10;
        description = ''
          The pool size of the database connection pool.
        '';
      };
      "oddjobsstartargs.webuiauth" = lib.mkOption {
        type = lib.types.str;
        default = "Nothing";
        example = "Nothing";
        description = ''
          The login name for the job admin web UI.
          Set to Nothing if not using the web UI.
        '';
      };
      "oddjobsstartargs.webuiport" = lib.mkOption {
        type = lib.types.int;
        default = 5555;
        example = 5555;
        description = ''
          The port number for the job admin web UI.
          Will not take effect if not using the web UI.
        '';
      };
      "oddjobsstartargs.daemonize" = lib.mkOption {
        type = lib.types.str;
        default = "True";
        example = "True";
        description = ''
          Should the web ui start as a daemon?
          This may affect if you are using odd-jobs as systemd service.
        '';
      };
      "oddjobsstartargs.pidfile" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.runDir}/my-job-runner.pid";
        example = "${env.runner.runDir}/my-job-runner.pid";
        description = ''
          The PID file path for the odd-jobs daemon.
          Will need this to stop it gracefully.
        '';
      };
      "oddjobsstopargs.timeout" = lib.mkOption {
        type = lib.types.int;
        default = 60;
        example = 60;
        description = ''
          The stop command timeout in seconds.
          set this to make it stop gracefully.
        '';
      };
      "oddjobsstopargs.pidfile" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.runDir}/my-job-runner.pid";
        example = "${env.runner.runDir}/my-job-runner.pid";
        description = ''
          The PID file path for the odd-jobs daemon.
          Will need this to stop it gracefully.
        '';
      };
      "oddjobsconfig.tablename" = lib.mkOption {
        type = lib.types.str;
        default = "${config.db.dataSchema}.jobs";
        example = "data.jobs";
        description = ''
          The table name for the job information.
          This should be prefixed by the dataSchema.
        '';
      };
      "oddjobsconfig.jobrunner" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The job runner implementation.
          Actually, I donnot konw how to config this
          because haskell function cannot be sereriable.
        '';
      };
      "oddjobsconfig.defaultmaxattempts" = lib.mkOption {
        type = lib.types.int;
        default = 10;
        example = 5;
        description = ''
          The default how many times should the job be retried
          before considering it is failed.
        '';
      };
      "oddjobsconfig.concurrencycontrol" = lib.mkOption {
        type = lib.types.int;
        default = 5;
        example = 5;
        description = ''
          How many jobs could be run on the same time
          on this machine.
        '';
      };
      "oddjobsconfig.dbpool" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "";
        description = ''
          The database pool implementation.
          I do not konw how to config this either.
        '';
      };
      "oddjobsconfig.pollinginterval" = lib.mkOption {
        type = lib.types.int;
        default = 5;
        example = 5;
        description = ''
          The intervall for pooling the job from database.
        '';
      };
      "oddjobsconfig.onjobsuccess" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "";
        description = ''
          What to do when job success.
          do not know how to config this.
        '';
      };
      "oddjobsconfig.onjobfailed" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "";
        description = ''
          What to do when job failed finally.
          do not know how to config this.
        '';
      };
      "oddjobsconfig.onjobstart" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "";
        description = ''
          What to do when job start.
          do not know how to config this.
        '';
      };
      "oddjobsconfig.onjobtimeout" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "";
        description = ''
          What to do when job success.
          do not know how to config this.
        '';
      };
      "oddjobsconfig.pidfile" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.runDir}/my-job-runner.pid";
        example = "${env.runner.runDir}/my-job-runner.pid";
        description = ''
          The PID file for the job runner.
        '';
      };
      "oddjobsconfig.logger" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The logger implementation in haskell.
          Actually, do not konw how to config it right now.
        '';
      };
      "oddjobsconfig.jobtype" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The job type list.
        '';
      };
      "oddjobsconfig.jobtypesql" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The sql statement to query the job type.
        '';
      };
      "oddjobsconfig.defaultjobtimeout" = lib.mkOption {
        type = lib.types.int;
        default = 600;
        example = 1800;
        description = ''
          The timeout when job would be consieder timeout.
        '';
      };
      "oddjobsconfig.jobtohtml" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The code turn job into html.
        '';
      };
      "oddjobsconfig.alljobtypes" = lib.mkOption {
        type = lib.types.str;
        default = "";
        example = "";
        description = ''
          The all job types list.
        '';
      };
      "cmdpath.xvfbpath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.xvfb-run}/bin/xvfb-run";
        example = "/usr/local/bin/xvfb-run";
        description = ''
          The full path to the xvfb-run command.
        '';
      };
      "cmdpath.wgetpath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.wget}/bin/wget";
        example = "/usr/local/bin/wget";
        description = ''
          The full path to the wget command.
        '';
      };
      "cmdpath.curlpath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.curl}/bin/curl";
        example = "/usr/local/bin/curl";
        description = ''
          The full path to the curl command.
        '';
      };
      "cmdpath.javapath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.jdk11}/bin/java";
        example = "/usr/local/bin/java";
        description = ''
          The full path to the java command.
        '';
      };
      "cmdpath.parsedumpshpath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.my-eclips-mat-with-dtfj}/mat/parseHeapDump.sh";
        example = "/usr/local/mat/parseHeapDump.sh";
        description = ''
          The full path to the MAT parseHeapDump.sh script.
        '';
      };
      "cmdpath.jcapath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.my-jca.src}";
        description = ''
          The full path to the jca jar.
        '';
      };
      "cmdpath.gcmvpath" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.my-gcmv}/bin/gcmv";
        description = ''
          The full path to the gcmv command.
        '';
      };
      "outputpath.fetcheddumphome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/raw_dump_files";
        description = ''
          The path to store the fetched dump file.
        '';
      };
      "outputpath.jcapreprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/preprocessed_report_jca";
        description = ''
          The path to store the proprocessed result of jca.
        '';
      };
      "outputpath.matpreprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/preprocessed_report_mat";
        description = ''
          The path the store the preprocessed result of mat.
        '';
      };
      "outputpath.gcmvpreprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/preprocessed_report_gcmv";
        description = ''
          The path to store the preprocessed result of gcmv.
        '';
      };
      "outputpath.jcareporthome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/parsed_report_jca";
        description = ''
          The path to store the parsed result of jca.
        '';
      };
      "outputpath.matreporthome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/parsed_report_mat";
        description = ''
          The path to store the parsed result of mat.
        '';
      };
      "outputpath.gcmvreporthome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/parsed_report_gcmv";
        description = ''
          The path to store the parsed result of gcmv.
        '';
      };
      "outputpath.jcapostprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/postprocessed_report_jca";
        description = ''
          The path to store the postprocessed result of jca.
        '';
      };
      "outputpath.matpostprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/postprocessed_report_mat";
        description = ''
          The path to store the parsed result of mat.
        '';
      };
      "outputpath.gcmvpostprocessorhome" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/postprocessed_report_gcmv";
        description = ''
          The path to store the parsed result of gcmv.
        '';
      };
      "jcacmdlineoptions.xmx" = lib.mkOption {
        type = lib.types.int;
        default = 2048;
        example = 4096;
        description = ''
          The max heap for the jca parser.
        '';
      };
      "matcmdlineoptions.xmx" = lib.mkOption {
        type = lib.types.int;
        default = 4096;
        example = 8192;
        description = ''
          The max heap for the mat parser.
        '';
      };
      "gcmvcmdlineoptions.xmx" = lib.mkOption {
        type = lib.types.int;
        default = 2048;
        example = 2048;
        description = ''
          The max heap for the gcmv parser.
        '';
      };
      "gcmvcmdlineoptions.jvm" = lib.mkOption {
        type = lib.types.str;
        default = "${pkgs.jdk11}";
        description = ''
          The path to the jvm.
        '';
      };
      "gcmvcmdlineoptions.preference" = lib.mkOption {
        type = lib.types.str;
        default = "${env.runner.dataDir}/default_preference.emf";
        description = ''
          The default preference of gcmv.
        '';
      };
      "curlcmdlineoptions.loginuser" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "test1@test1.com";
        description = ''
          The email to login to backend to upload the analyze report.
        '';
      };
      "curlcmdlineoptions.loginpin" = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        example = "loginpassword";
        description = ''
          The password to login to backend.
        '';
      };
      "curlcmdlineoptions.loginurl" = lib.mkOption {
        type = lib.types.str;
        default = "http://${config.api-gw.serverName}:${
            toString config.api-gw.listenPort
          }/rest/rpc/login";
        description = ''
          The url to login to backend.
        '';
      };
      "curlcmdlineoptions.uploadurl" = lib.mkOption {
        type = lib.types.str;
        default = "http://${config.api-gw.serverName}:${
            toString config.api-gw.listenPort
          }/uploadreport";
        description = ''
          The url to upload the report.
        '';
      };
    };
  };
}
