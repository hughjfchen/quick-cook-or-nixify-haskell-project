#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "project scaffold existing"

#set +u to temp work around the nix script
set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

case ${THE_DISTRIBUTION_ID} in
  debian|ubuntu|rhel|centos)
    SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")
    mkdir -p "$1/$2"
    cd "$1/$2"
    TEMPLATE_PATHS=$(find "${SCRIPT_ABS_PATH}" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}")
    for TEMPLATE_PATH in "${TEMPLATE_PATHS}"
    do
    	TEMPLATE_NAME=$(basename "${TEMPLATE_PATH}")
	cp "${TEMPLATE_PATH}/project.yml.orig" "${TEMPLATE_PATH}/project.yml"
	sed -i "s/MY_PROJECT_NAME/$2/g" "${TEMPLATE_PATH}/project.yml"
    	"${SCRIPT_ABS_PATH}"/rob add "${TEMPLATE_NAME}" "${TEMPLATE_PATH}"
    done
    "${SCRIPT_ABS_PATH}"/rob new
    ;;
  *)
    nix-shell '<nixpkgs>' -p haskellPackages.rob --run "mkdir -p $1/$2; cd $1/$2; rob new"
    #nix-shell '<nixpkgs>' -p hello --run "mkdir -p $1/$2; hello"
    ;;
esac

#nix-shell '<nixpkgs>' -p haskellPackages.summoner --run "mkdir -p $1; cd $1; summon new $2"

done_banner "Top level" "project scaffold existing"
