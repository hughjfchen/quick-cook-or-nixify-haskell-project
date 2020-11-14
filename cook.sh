#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/common/common.sh

usage_and_exit () {
            echo "Usage: cook.sh <project root path> <project name>"
            exit 1
}

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

if [ "$#" != 2 ]; then 
	usage_and_exit
fi

begin_banner "Top level" "project cooking"

mkdir -p "$1/$2"

cd "$1/$2"

"${SCRIPT_ABS_PATH}/prepare-env/do.sh"

#"${SCRIPT_ABS_PATH}/project-scaffold/do.sh"

cp -R "${SCRIPT_ABS_PATH}"/build-framework/* "$1/$2/"

for FILE_TO_SED in $(grep -R MY_PROJECT_NAME "$1/$2/"*|awk -F":" '{print $1}'|sort|uniq)
do
   sed -i.bak.for.sed.inplace.edit "s/MY_PROJECT_NAME/$2/g" ${FILE_TO_SED}
   rm -fr ${FILE_TO_SED}.bak.for.sed.inplace.edit
done

#update index-state in the cabal.project file
echo "index-state : $(date +%Y-%m-%dT00:00:00Z)" > "$1/$2"/cabal.project

#update niv sources list
set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

nix-shell -p haskellPackages.niv --run "niv update"

# everything's done
done_banner "Top level" "project cooking"
