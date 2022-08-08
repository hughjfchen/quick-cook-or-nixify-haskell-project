#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/common/common.sh

usage_and_exit () {
            echo "Usage: cook.sh <project root path> <project name> <generate|template>"
            exit 1
}

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

if [ "$#" != 3 ]; then 
    usage_and_exit
fi

begin_banner "Top level" "project cooking"

"${SCRIPT_ABS_PATH}"/prepare-env/do.sh

case $3 in
  generate)
         "${SCRIPT_ABS_PATH}"/project-scaffold-generate/do.sh "$1" "$2"

         "${SCRIPT_ABS_PATH}"/build-framework/do.sh "$1" "$2"

         # copy the common and prepare-env for CI for the generated project
         # because it may need to setup env within a CI environment
         mkdir -p "$1/$2/ci"
         cp -R "${SCRIPT_ABS_PATH}"/common "$1/$2/ci/"
         cp -R "${SCRIPT_ABS_PATH}"/prepare-env "$1/$2/ci/"
          ;;
  template)
         "${SCRIPT_ABS_PATH}"/project-scaffold-template/do.sh "$1" "$2"

         "${SCRIPT_ABS_PATH}"/fix-up/do.sh "$1" "$2"
         # for executables, we need to copy to override them after applying tempalte(Why?)
         [[ -d "$1/$2/cd" ]] && cp "${SCRIPT_ABS_PATH}"/deployment-framework/arion "$1/$2/cd/" && chmod +x "$1/$2/cd/arion"
         [[ -d "$1/$2/$2/cd" ]] && cp "${SCRIPT_ABS_PATH}"/deployment-framework/arion "$1/$2/$2/cd/" && chmod +x "$1/$2/$2/cd/arion"
         ;;
  *)
         ;;
esac


# everything's done
done_banner "Top level" "project cooking"
