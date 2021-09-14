#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "project scaffold based on template"

#set +u to temp work around the nix script
set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

# define a function to handle paths which are provided by a find command
function prepare_project_info_for_rob () {
	local TEMPLATE_NAME
    	TEMPLATE_NAME=$(basename "$3")
	cp "$3/project.yml.orig" "$3/project.yml"
	sed -i "s/MY_PROJECT_NAME/$2/g" "$3/project.yml"
    	"$1"/rob add "${TEMPLATE_NAME}" "$3"
}
# export the above function so that can be used by other command or subshell
export -f prepare_project_info_for_rob

case ${THE_DISTRIBUTION_ID} in
  debian|ubuntu|rhel|centos)
    SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")
    mkdir -p "$1/$2"
    cd "$1/$2"
    find "${SCRIPT_ABS_PATH}" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}" -exec bash -c 'prepare_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$2" {} \;
    "${SCRIPT_ABS_PATH}"/rob new
    "${SCRIPT_ABS_PATH}"/niv update
    ;;
  *)
    nix-shell '<nixpkgs>' -p haskellPackages.rob niv --run "mkdir -p $1/$2; cd $1/$2; rob new; niv update"
    #nix-shell '<nixpkgs>' -p hello --run "mkdir -p $1/$2; hello"
    ;;
esac

#nix-shell '<nixpkgs>' -p haskellPackages.summoner --run "mkdir -p $1; cd $1; summon new $2"

done_banner "Top level" "project scaffold based on template"
