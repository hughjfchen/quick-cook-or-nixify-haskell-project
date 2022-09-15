#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "fix up"

# define a function to fix-up the paths based on different template name
function fix_up_paths_based_on_template_name () {
        local PROJECT_NAME_REAL_PATH
        PROJECT_NAME_REAL_PATH=${3//$1/$COMPPATH}
        #PROJECT_NAME_REAL_PATH=$(echo "$3" | sed "s/$1/$COMPPATH/g")
        mv "$3" "${PROJECT_NAME_REAL_PATH}"
}
export -f fix_up_paths_based_on_template_name

if [ -d "$1/$2/project.orig.name" ]; then
    mv "$1/$2/project.orig.name" "$1/$2/$2"
    COMPPATH="$2/$2"
else
    COMPPATH="$2"
fi

PROJECT_PASCAL_NAME=$(cat "$1/$COMPPATH/.project.pascal.name")
PROJECT_SNAKE_NAME=$(cat "$1/$COMPPATH/.project.snake.name")
while read -r PROJECT_ORIG_NAME_PATH; do
    #[[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" $(echo "${PROJECT_ORIG_NAME_PATH}" | sed "s/project.orig.name/$COMPPATH/g")
    [[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" "${PROJECT_ORIG_NAME_PATH//project.orig.name/$2}"
done <<< "$(find "$1/$COMPPATH" -name "*project.orig.name*" ! -wholename "$1/$COMPPATH/.project.orig.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
while read -r PROJECT_SNAKE_NAME_PATH; do
    #[[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" $(echo "${PROJECT_SNAKE_NAME_PATH}" | sed "s/project.snake.name/${PROJECT_SNAKE_NAME}/g")
    [[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" "${PROJECT_SNAKE_NAME_PATH//project.snake.name/${PROJECT_SNAKE_NAME}}"
done <<< "$(find "$1/$COMPPATH" -name "*project.snake.name*" ! -wholename "$1/$COMPPATH/.project.snake.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
while read -r PROJECT_PASCAL_NAME_PATH; do
    #[[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" $(echo "${PROJECT_PASCAL_NAME_PATH}" | sed "s/project.pascal.name/${PROJECT_PASCAL_NAME}/g")
    [[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" "${PROJECT_PASCAL_NAME_PATH//project.pascal.name/${PROJECT_PASCAL_NAME}}"
done <<< "$(find "$1/$COMPPATH" -name "*project.pascal.name*" ! -wholename "$1/$COMPPATH/.project.pascal.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"

if [[ -d "$1/$2/env" ]]; then
  while read -r PROJECT_ORIG_NAME_PATH; do
    #[[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" $(echo "${PROJECT_ORIG_NAME_PATH}" | sed "s/project.orig.name/$COMPPATH/g")
    [[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" "${PROJECT_ORIG_NAME_PATH//project.orig.name/$2}"
  done <<< "$(find "$1/$2/env" -name "*project.orig.name*" ! -wholename "$1/$2/env/.project.orig.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
  while read -r PROJECT_SNAKE_NAME_PATH; do
    #[[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" $(echo "${PROJECT_SNAKE_NAME_PATH}" | sed "s/project.snake.name/${PROJECT_SNAKE_NAME}/g")
    [[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" "${PROJECT_SNAKE_NAME_PATH//project.snake.name/${PROJECT_SNAKE_NAME}}"
  done <<< "$(find "$1/$2/env" -name "*project.snake.name*" ! -wholename "$1/$2/env/.project.snake.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
  while read -r PROJECT_PASCAL_NAME_PATH; do
    #[[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" $(echo "${PROJECT_PASCAL_NAME_PATH}" | sed "s/project.pascal.name/${PROJECT_PASCAL_NAME}/g")
    [[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" "${PROJECT_PASCAL_NAME_PATH//project.pascal.name/${PROJECT_PASCAL_NAME}}"
  done <<< "$(find "$1/$2/env" -name "*project.pascal.name*" ! -wholename "$1/$2/env/.project.pascal.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
fi

if [[ -d "$1/$2/config" ]]; then
  while read -r PROJECT_ORIG_NAME_PATH; do
    #[[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" $(echo "${PROJECT_ORIG_NAME_PATH}" | sed "s/project.orig.name/$COMPPATH/g")
    [[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" "${PROJECT_ORIG_NAME_PATH//project.orig.name/$2}"
  done <<< "$(find "$1/$2/config" -name "*project.orig.name*" ! -wholename "$1/$2/config/.project.orig.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
  while read -r PROJECT_SNAKE_NAME_PATH; do
    #[[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" $(echo "${PROJECT_SNAKE_NAME_PATH}" | sed "s/project.snake.name/${PROJECT_SNAKE_NAME}/g")
    [[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" "${PROJECT_SNAKE_NAME_PATH//project.snake.name/${PROJECT_SNAKE_NAME}}"
  done <<< "$(find "$1/$2/config" -name "*project.snake.name*" ! -wholename "$1/$2/config/.project.snake.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
  while read -r PROJECT_PASCAL_NAME_PATH; do
    #[[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" $(echo "${PROJECT_PASCAL_NAME_PATH}" | sed "s/project.pascal.name/${PROJECT_PASCAL_NAME}/g")
    [[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" "${PROJECT_PASCAL_NAME_PATH//project.pascal.name/${PROJECT_PASCAL_NAME}}"
  done <<< "$(find "$1/$2/config" -name "*project.pascal.name*" ! -wholename "$1/$2/config/.project.pascal.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
fi

find "$1/$2" -name develop -exec chmod +x {} \;
find "$1/$2" -name build -exec chmod +x {} \;
find "$1/$2" -name deploy -exec chmod +x {} \;
find "$1/$2" -name arion -exec chmod +x {} \;
find "$1/$2" -name "*.sh" -exec chmod +x {} \;
find "$1/$2" -name "*.bash" -exec chmod +x {} \;
find "$1/$2" -type d -name bin -exec chmod -R +x {} \;

# run the fixup for the specific template
find "$1/$2" -name fixup -exec bash {} \;

find "$1/$2" -name "project.yml.orig" -exec rm -fr {} \;
find "$1/$2" -name "project.xml" -exec rm -fr {} \;
find "$1/$2" -name "fixup" -exec rm -fr {} \;

# define a function to handle paths which are provided by a find command
function reverse_project_info_for_rob () {
        local TEMPLATE_NAME
        TEMPLATE_NAME=$(basename "$3")
        cp "$3/project.yml.orig" "$3/project.yml"
        #sed -i "s/MY_PROJECT_NAME/$COMPPATH/g" "${TEMPLATE_PATH}/project.yml"
        #"${SCRIPT_ABS_PATH}"/../project-scaffold-template/rob remove
}
# export the above function so that can be used by other command or subshell
export -f reverse_project_info_for_rob

# reverse the project.yml back to orig
find "${SCRIPT_ABS_PATH}/../project-scaffold-template" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}/../project-scaffold-template" -exec bash -c 'reverse_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$COMPPATH" {} \;

done_banner "Top level" "fix up"
