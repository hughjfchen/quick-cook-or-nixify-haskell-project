#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

begin_banner "Top level" "fix up"

PROJECT_PASCAL_NAME=$(cat "$1/$2/.project.pascal.name")
PROJECT_SNAKE_NAME=$(cat "$1/$2/.project.snake.name")
mv "$1/$2/template.cabal" "$1/$2/$2.cabal"
mv "$1/$2/src/Core/Template.hs" "$1/$2/src/Core/${PROJECT_PASCAL_NAME}.hs"
mv "$1/$2/src/Capability/Template.hs" "$1/$2/src/Capability/${PROJECT_PASCAL_NAME}.hs"
mv "$1/$2/app/AppCapability/Template.hs" "$1/$2/app/AppCapability/${PROJECT_PASCAL_NAME}.hs"

chmod +x "$1/$2/start-dev"
find "$1/$2" -name *.sh -exec chmod +x {} \;

# reverse the project.yml back to orig
TEMPLATE_PATHS=$(find "${SCRIPT_ABS_PATH}/../project-scaffold-template" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}/../project-scaffold-template")
for TEMPLATE_PATH in "${TEMPLATE_PATHS}"
do
  TEMPLATE_NAME=$(basename "${TEMPLATE_PATH}")
  cp "${TEMPLATE_PATH}/project.yml.orig" "${TEMPLATE_PATH}/project.yml"
  #sed -i "s/MY_PROJECT_NAME/$2/g" "${TEMPLATE_PATH}/project.yml"
  #"${SCRIPT_ABS_PATH}"/../project-scaffold-template/rob remove
done

done_banner "Top level" "fix up"
