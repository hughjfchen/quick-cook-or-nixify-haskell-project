{ lib, pkgs, ... }:

let

  # define some utility function for release packing ( code adapted from setup-systemd-units.nix )
  mk-release-packer = { referencePath # : Path
    # paths to the corresponding reference file
    , component # : String
    # The name for the deployed component
    # e.g., "my-postgresql", "my-postgrest"
    , site # : String
    # The name for the deployed target site
    # e.g., "my-site", "local"
    , phase # : String
    # The name for the deployed target phase
    # e.g., "local", "test", "production"
    , innerTarballName # : String
    # The name for the deployed inner tarball
    # e.g., "component"+"site"+"phase".tar.gz
    , deployScript # : Path
    # the deploy script path
    , cleanupScript # : Path
    # the cleanup script path
    }:
    let
      namespace = lib.concatStringsSep "-" [ component site phase ];
      referenceKey = lib.concatStringsSep "." [ namespace "reference" ];
      reference = lib.attrsets.setAttrByPath [ referenceKey ] referencePath;
      static = pkgs.runCommand "${namespace}-reference-file-static" { } ''
        mkdir -p $out
        ${lib.concatStringsSep "\n"
        (lib.mapAttrsToList (nm: file: "ln -sv ${file} $out/${nm}") reference)}
      '';
      gen-changed-pkgs-list = name: file: ''
        oldReference=$(readlink -f "$referenceDir/${name}" || echo "$referenceDir/${name}")
        if [ -f "$oldReference" -a "$oldReference" != "${file}" ]; then
          LC_ALL=C comm -13 <(LC_ALL=C sort -u $oldReference) <(LC_ALL=C sort -u "${file}") > "$referenceDir/${name}.delta"
          fileListToPack="$referenceDir/${name}.delta"
        else
          fileListToPack="${file}"
        fi
        ln -sf "/nix/var/reference-file-static/${namespace}/${name}" \
          "$referenceDir/.${name}.tmp"
        mv -T "$referenceDir/.${name}.tmp" "$referenceDir/${name}"
      '';
    in pkgs.writeScriptBin "mk-release-packer-for-${site}-${phase}" ''
      #!${pkgs.bash}/bin/bash -e
      export PATH=${pkgs.coreutils}/bin:${pkgs.gnutar}/bin:${pkgs.gzip}/bin

      fileListToPack="${referencePath}"

      referenceDir=/nix/var/reference-file
      mkdir -p "$referenceDir"

      oldStatic=$(readlink -f /nix/var/reference-file-static/${namespace} || true)
      if [ "$oldStatic" != "${static}" ]; then
        ${
          lib.concatStringsSep "\n"
          (lib.mapAttrsToList gen-changed-pkgs-list reference)
        }
        mkdir -p /nix/var/reference-file-static
        ln -sfT ${static} /nix/var/reference-file-static/.${namespace}.tmp
        mv -T /nix/var/reference-file-static/.${namespace}.tmp /nix/var/reference-file-static/${namespace}
      else
        echo "Dependence reference file not exist or unchanged, will do a full release pack" >&2
      fi

      # pack the systemd service or executable sh and dependencies with full path
      tar zPcf ./${innerTarballName} -T "$fileListToPack"

      # pack the previous tarball and the two scripts for distribution
      packDirTemp=$(mktemp -d)
      cp "${deployScript}" $packDirTemp/deploy-${component}-to-${site}-${phase}
      cp "${cleanupScript}" $packDirTemp/cleanup-${component}-on-${site}-${phase}
      mv ./${innerTarballName}  $packDirTemp
      tar zcf ./${namespace}-dist.tar.gz \
        -C $packDirTemp \
        deploy-${component}-to-${site}-${phase} \
        cleanup-${component}-on-${site}-${phase} \
        ${innerTarballName}
      rm -fr $packDirTemp

    '';
  mk-deploy-sh =
    { env # : AttrsSet the environment for the deployment target machine
    , payloadPath # : Path the nix path to the program service or wrapping script
    , innerTarballName # : String the tarball file name for the inner package tar
    , execName # : String the executable file name
    , startCmd ? "" # : String command line to start the program, default ""
    , stopCmd ? "" # : String command line to stop the program, default ""
    }:
    pkgs.writeScript "mk-deploy-sh" ''
      #!/usr/bin/env bash

      # this script need to be run with root or having sudo permission
      [ $EUID -ne 0 ] && ! sudo -v >/dev/null 2>&1 && echo "need to run with root or sudo" && exit 127

      # some command fix up for systemd service, especially web server
      getent group nogroup > /dev/null || sudo groupadd nogroup

      # create user and group
      getent group "${env.processUser}" > /dev/null || sudo groupadd "${env.processUser}"
      getent passwd "${env.processUser}" > /dev/null || sudo useradd -m -p Passw0rd -g "${env.processUser}" "${env.processUser}"

      # create directories
      for dirToMk in "${env.runDir}" "${env.dataDir}"
      do
        if [ ! -d "$dirToMk" ]; then
           sudo mkdir -p "$dirToMk"
           sudo chown -R ${env.processUser}:${env.processUser} "$dirToMk"
        fi
      done

      # now unpack(note we should preserve the /nix/store directory structure)
      sudo tar zPxf ./${innerTarballName}
      sudo chown -R ${env.processUser}:${env.processUser} /nix

      # setup the systemd service or create a link to the executable
      ${lib.concatStringsSep "\n" (if env.isSystemdService then
        [ "sudo ${payloadPath}/bin/setup-systemd-units" ]
      else [''
        # there is a previous version here, stop it first
        if [ -e ${env.runDir}/stop.sh ]; then
          echo "stopping ${execName}"
          ${env.runDir}/stop.sh
        fi

        # since the payload path changed for every deployment,
        # the start/stop scripts must be generated each deployment
        {
          echo "#!/usr/bin/env bash"
          echo "exec ${payloadPath}/bin/${execName} ${startCmd} \"\$@\""
        } > ${env.runDir}/start.sh
        {
          echo "#!/usr/bin/env bash"
          echo "exec ${payloadPath}/bin/${execName} ${stopCmd} \"\$@\""
        } > ${env.runDir}/stop.sh
        chmod +x ${env.runDir}/start.sh ${env.runDir}/stop.sh
        echo "starting the program ${execName}"
        ${env.runDir}/start.sh
        echo "check the scripts under ${env.runDir} to start or stop the program."''])}

    '';
  mk-cleanup-sh = { env # the environment for the deployment target machine
    , payloadPath # the nix path to the program service or wrapping script
    , innerTarballName # : String the tarball file name for the inner package tar
    , execName # : String the executable file name
    }:
    pkgs.writeScript "mk-cleanup-sh" ''
      #!/usr/bin/env bash

      # this script need to be run with root or having sudo permission
      [ $EUID -ne 0 ] && ! sudo -v >/dev/null 2>&1 && echo "need to run with root or sudo" && exit 127

      # how do we unsetup the systemd unit? we do not unsetup the systemd service for now
      # we just stop it before doing the cleanup
      ${lib.concatStringsSep "\n" (if env.isSystemdService then [''
        if [ -e ${payloadPath}/bin/setup-systemd-units ]; then
           unitName=$(awk 'BEGIN { FS="\"" } /unitsToStart\+\=\(/ {print $2}' ${payloadPath}/bin/setup-systemd-units)
           sudo systemctl stop $unitName
        fi''] else
        [ "[ -e ${env.runDir}/stop.sh ] && ${env.runDir}/stop.sh" ])}

      for dirToRm in "${env.runDir}" "${env.dataDir}"
      do
        if [ -d "$dirToRm" ]; then
           sudo rm -fr "$dirToRm"
        fi
      done

      # do we need to delete the program and all its dependencies in /nix/store?
      # we do not do that for now
      # sudo rm -fr /nix/store/xxx ( maybe a file list for the package and its references )

      # well, shall we remove the user and group? maybe not
      # we do not do that for now.
      # getent passwd "${env.processUser}" > /dev/null && sudo userdel -fr "${env.processUser}"
      # getent group "${env.processUser}" > /dev/null && sudo groupdel -f "${env.processUser}"

    '';
in { inherit mk-release-packer mk-deploy-sh mk-cleanup-sh; }
