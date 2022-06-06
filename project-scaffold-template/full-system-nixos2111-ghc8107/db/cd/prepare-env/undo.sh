#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "deploy env unprepare"

if [ -d /var/"$RELEASE_USER_NAME" ]; then
    info "/var/$RELEASE_USER_NAME directory found, delete it..."
    sudo rm -fr /var/"$RELEASE_USER_NAME"
fi

set +e
myUser2=$(awk -F":" '{print $1}' /etc/passwd | grep -w "$RELEASE_USER_NAME")
if [ "X${myUser2}" != "X" ]; then
    info "$RELEASE_USER_NAME user defined, delete it..."
    sudo userdel -fr "$RELEASE_USER_NAME"
fi

myGroup2=$(awk -F":" '{print $1}' /etc/group | grep -w "$RELEASE_USER_NAME")
if [ "X${myGroup2}" != "X" ]; then
    info "$RELEASE_USER_NAME group defined, delete it..."
    sudo groupdel -f "$RELEASE_USER_NAME"
fi
set -e

if [ -n "$RELEASE_DOCKER_ON_TARGET" ]; then
    if type docker >/dev/null 2>&1; then
        info "docker found, trying to uninstall it"
        case ${THE_DISTRIBUTION_ID} in
            rhel) info "This script not know how to install docker-ce on RHEL, so that docker cna't be installed by this script. Ignore."
                  ;;
            debian|ubuntu) my_arch=$(uname -m)
                           if [ "${my_arch}" = "aarch64" ]; then
                               docker_arm="arm64"
                           else
                               docker_arm="amd64"
                           fi
                           sudo apt-get purge -y docker-ce docker-ce-cli containerd.io
                           ;;
            centos) sudo yum erase -y docker-ce docker-ce-cli containerd.io
                    ;;
            Darwin) if type brew > /dev/null 2>&1; then
                        brew cask uninstall docker
                        brew cask unlink docker
                    else
                        info "Don't know how to uninstall the docker desktop for macOS package."
                        info "please refer to the installation web page for uninstallation instructions."
                    fi
                    ;;
            *) warn "Not supported distribution OS."
               ;;
        esac
    fi

    if type docker-compose >/dev/null 2>&1; then
        info "docker-compose found, trying to uninstall it"
        case ${THE_DISTRIBUTION_ID} in
            rhel) info "This script not know how to install docker-compose on RHEL, so that docker-compose cna't be installed by this script. Ignore."
                  ;;
            debian|ubuntu) sudo apt-get purge -y docker-compose
                           ;;
            centos) [[ -e /usr/local/bin/docker-compose ]] && sudo rm /usr/local/bin/docker-compose
                    [[ -L /usr/bin/docker-compose ]] && sudo rm /usr/bin/docker-compose
                    ;;
            Darwin) info "docker-compose will be uninstalled with docker for desktop package. skip."
                    ;;
            *) warn "Not supported distribution OS"
               ;;
        esac
    fi
fi

done_banner "Top level" "deploy env unprepare"
