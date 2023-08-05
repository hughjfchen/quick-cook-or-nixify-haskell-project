#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/common/common.sh

usage_and_exit () {
            echo "Usage: cook.sh <project parent path> <project name>"
            exit 1
}

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

if [ "$#" != 2 ]; then
    usage_and_exit
fi

begin_banner "Top level" "project cooking"

"${SCRIPT_ABS_PATH}"/prepare-env/do.sh

"${SCRIPT_ABS_PATH}"/project-scaffold-template/do.sh "$1" "$2"

"${SCRIPT_ABS_PATH}"/fix-up/do.sh "$1" "$2"


# everything's done
done_banner "Top level" "project cooking"
