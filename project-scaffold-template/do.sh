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
  debian|ubuntu|rhel|centos|nixos)
    SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")
    # clean up the template list first
    rm -fr "$HOME/.rob"

    # list the template and let user pick one or their combination
    # all templates will be put under two directories accordingly:
    # application or infrastructure

    TEMPLATE_NAME=""
    TEMPLATE_PATH=""
    for TEMPLATE_CLASS in application infrastructure
    do
        echo "pick the template or templcate combination for the $TEMPLATE_CLASS from following:"
        find "${SCRIPT_ABS_PATH}/$TEMPLATE_CLASS" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}/$TEMPLATE_CLASS" \
            -exec basename {} \;

        read -p "which template or template combination for the $TEMPLATE_CLASS do you want to use: " TEMP_TEMPLATE_NAME

        DEP_TEMPLATE=""
        for TEMP_TEMPLATE_NAME_PART in $TEMP_TEMPLATE_NAME
        do
            TEMP_DEP_TEMPLATE=$(sed -n "/$TEMP_TEMPLATE_NAME_PART:/p" "$SCRIPT_ABS_PATH/$TEMPLATE_CLASS/.template.dep"|awk -F':' '{print $2}')
            [[ -n "$TEMP_DEP_TEMPLATE" ]] && DEP_TEMPLATE+=" $TEMP_DEP_TEMPLATE"
        done
        [[ -n "$DEP_TEMPLATE" ]] && DEP_TEMPLATE=$(echo "${DEP_TEMPLATE/ /}" | tr -s ' ' | tr ' ' '\n' | sort | uniq | tr '\n' ' '|xargs)
        TEMPLATE_NAME+=" $TEMP_TEMPLATE_NAME"
        [[ -n "$DEP_TEMPLATE" ]] && TEMPLATE_NAME+=" $DEP_TEMPLATE"
        TEMP_TEMPLATE_PATH=$(echo "$TEMP_TEMPLATE_NAME"|tr ' ' '\n'|sed "s:^:$TEMPLATE_CLASS/:g"|tr '\n' ' '|xargs)
        TEMPLATE_PATH+=" $TEMP_TEMPLATE_PATH"
        [[ -n "$DEP_TEMPLATE" ]] && DEP_TEMP_TEMPLATE_PATH=$(echo "$DEP_TEMPLATE"|tr ' ' '\n'|sed "s:^:$TEMPLATE_CLASS/:g"|tr '\n' ' '|xargs)
        [[ -n "$DEP_TEMPLATE" ]] && TEMPLATE_PATH+=" $DEP_TEMP_TEMPLATE_PATH"
    done
    # ok, combine them and create a new template
    TEMPLATE_NAME_COMBINED=$(echo "${TEMPLATE_NAME/ /}" | tr -s ' ' | tr ' ' '-')
    NEW_TEMPLATE_DIR="/tmp/$TEMPLATE_NAME_COMBINED"
    mkdir -p "$NEW_TEMPLATE_DIR"
    for TEMPLATE_DIR in $TEMPLATE_PATH
    do
        cp -na "$SCRIPT_ABS_PATH/$TEMPLATE_DIR"/. "$NEW_TEMPLATE_DIR"
    done

    # and copy the rob project.yml and some supporting files
    for SUPPORTING_FILE in project.yml.orig .project.orig.name .project.snake.name .project.pascal.name
    do
        cp "$SCRIPT_ABS_PATH/$SUPPORTING_FILE" "$NEW_TEMPLATE_DIR"
    done

    prepare_project_info_for_rob "${SCRIPT_ABS_PATH}" "$2" "$NEW_TEMPLATE_DIR"

    mkdir -p "$1/$2"
    cd "$1/$2" || exit 225
    "${SCRIPT_ABS_PATH}"/rob new

    # do not need the template and combination any more, clean up
    rm -fr "$NEW_TEMPLATE_DIR"

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
    find "${SCRIPT_ABS_PATH}" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}" \
        -exec bash -c 'prepare_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$2" {} \;
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
        H_INTERNAL_INDEX_STATE=$(nix eval --impure \
            --expr "(import $1/$2/project.orig.name/default.nix {}).pkgs.haskell-nix.internalHackageIndexState" \
            | awk -F'"' '{print $2}')
        [[ -f "$1/$2/project.orig.name/cabal.project" ]] \
            && echo "index-state : $H_INTERNAL_INDEX_STATE" >> "$1/$2/project.orig.name/cabal.project"
    else
        H_INTERNAL_INDEX_STATE=$(nix eval --impure \
            --expr "(import $1/$2/default.nix {}).pkgs.haskell-nix.internalHackageIndexState" \
            | awk -F'"' '{print $2}')
    fi
    echo "index-state : $H_INTERNAL_INDEX_STATE" >> "$1/$2/cabal.project"
fi

# make sure .ghci could be loaded by ensuring the mode
[[ -f "$1/$2/.ghci" ]] && chmod go-w "$1/$2/.ghci"
[[ -f "$1/$2/project.orig.name/.ghci" ]] && chmod go-w "$1/$2/project.orig.name/.ghci"
[[ -d "$1/$2" ]] && chmod go-w "$1/$2"
[[ -d "$1/$2/project.orig.name" ]] && chmod go-w "$1/$2/project.orig.name"


done_banner "Top level" "project scaffold based on template"
