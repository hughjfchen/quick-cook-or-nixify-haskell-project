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
    cd "$1/$2" || exit 225
    find "${SCRIPT_ABS_PATH}" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}" -exec bash -c 'prepare_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$2" {} \;
    "${SCRIPT_ABS_PATH}"/rob new
    if [ -f "./nix/sources.json" ]; then
        "${SCRIPT_ABS_PATH}"/niv update
    else
        NIVSRCDIR=$(dirname "$(find . -name "sources.json"|head -1)")
        if [ "X$NIVSRCDIR" != "X" ] && [ -f "$NIVSRCDIR/sources.nix" ]; then
            NIVSRCTMP=$(mktemp -d)
            mkdir -p "$NIVSRCTMP/nix"
            cp "$NIVSRCDIR/sources.json" "$NIVSRCTMP/nix/sources.json"
            cp "$NIVSRCDIR/sources.nix" "$NIVSRCTMP/nix/sources.nix"
            ln -sf "$NIVSRCTMP/nix" ./nix
            "${SCRIPT_ABS_PATH}"/niv update
            for COMP_DIR in $(dirname $(find "." -name "sources.json"))
            do
                cp "$NIVSRCTMP/nix/sources.json" "$COMP_DIR"
                cp "$NIVSRCTMP/nix/sources.nix" "$COMP_DIR"
            done
            rm -fr ./nix
            rm -fr "$NIVSRCTMP"
        fi
    fi
    cd - > /dev/null || exit 225
    ;;
  *)
    nix-env -i haskellPackages.rob haskellPackages.niv
    SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")
    mkdir -p "$1/$2"
    cd "$1/$2" || exit 225
    find "${SCRIPT_ABS_PATH}" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}" -exec bash -c 'prepare_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$2" {} \;
    rob new
    if [ -f "./nix/sources.json" ]; then
        niv update
    else
        NIVSRCDIR=$(dirname "$(find . -name "sources.json"|head -1)")
        if [ "X$NIVSRCDIR" != "X" ] && [ -f "$NIVSRCDIR/sources.nix" ]; then
            NIVSRCTMP=$(mktemp -d)
            mkdir -p "$NIVSRCTMP/nix"
            cp "$NIVSRCDIR/sources.json" "$NIVSRCTMP/nix/sources.json"
            cp "$NIVSRCDIR/sources.nix" "$NIVSRCTMP/nix/sources.nix"
            ln -sf "$NIVSRCTMP/nix" ./nix
            niv update
            for COMP_DIR in $(dirname $(find "." -name "sources.json"))
            do
                cp "$NIVSRCTMP/nix/sources.json" "$COMP_DIR"
                cp "$NIVSRCTMP/nix/sources.nix" "$COMP_DIR"
            done
            rm -fr ./nix
            rm -fr "$NIVSRCTMP"
        fi
    fi
    cd - > /dev/null || exit 225
    ;;
esac

# use haskell.nix internal index-state by default so that we sync cabal with nix
if [ -f "$1/$2/cabal.project" ]; then
    if [ -f "$1/$2/project.orig.name/default.nix" ]; then
        H_INTERNAL_INDEX_STATE=$(nix eval --impure --expr "(import $1/$2/project.orig.name/default.nix {}).pkgs.haskell-nix.internalHackageIndexState"|awk -F'"' '{print $2}')
        [[ -f "$1/$2/project.orig.name/cabal.project" ]] && echo "index-state : $H_INTERNAL_INDEX_STATE" >> "$1/$2/project.orig.name/cabal.project"
    else
        H_INTERNAL_INDEX_STATE=$(nix eval --impure --expr "(import $1/$2/default.nix {}).pkgs.haskell-nix.internalHackageIndexState"|awk -F'"' '{print $2}')
    fi
    echo "index-state : $H_INTERNAL_INDEX_STATE" >> "$1/$2/cabal.project"
fi

# make sure .ghci could be loaded by ensuring the mode
[[ -f "$1/$2/.ghci" ]] && chmod go-w "$1/$2/.ghci"
[[ -f "$1/$2/project.orig.name/.ghci" ]] && chmod go-w "$1/$2/project.orig.name/.ghci"
[[ -d "$1/$2" ]] && chmod go-w "$1/$2"
[[ -d "$1/$2/project.orig.name" ]] && chmod go-w "$1/$2/project.orig.name"


done_banner "Top level" "project scaffold based on template"
