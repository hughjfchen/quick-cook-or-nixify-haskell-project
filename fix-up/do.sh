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
  	PROJECT_NAME_REAL_PATH=${3//$1/$2}
  	#PROJECT_NAME_REAL_PATH=$(echo "$3" | sed "s/$1/$2/g")
  	mv "$3" "${PROJECT_NAME_REAL_PATH}"
}
export -f fix_up_paths_based_on_template_name

PROJECT_PASCAL_NAME=$(cat "$1/$2/.project.pascal.name")
PROJECT_SNAKE_NAME=$(cat "$1/$2/.project.snake.name")
while read -r PROJECT_ORIG_NAME_PATH; do
	#[[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" $(echo "${PROJECT_ORIG_NAME_PATH}" | sed "s/project.orig.name/$2/g")
	[[ -n "${PROJECT_ORIG_NAME_PATH}" ]] && mv "${PROJECT_ORIG_NAME_PATH}" "${PROJECT_ORIG_NAME_PATH//project.orig.name/$2}"
done <<< "$(find "$1/$2" -name "*project.orig.name*" ! -wholename "$1/$2/.project.orig.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
while read -r PROJECT_SNAKE_NAME_PATH; do
	#[[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" $(echo "${PROJECT_SNAKE_NAME_PATH}" | sed "s/project.snake.name/${PROJECT_SNAKE_NAME}/g")
	[[ -n "${PROJECT_SNAKE_NAME_PATH}" ]] && mv "${PROJECT_SNAKE_NAME_PATH}" "${PROJECT_SNAKE_NAME_PATH//project.snake.name/${PROJECT_SNAKE_NAME}}"
done <<< "$(find "$1/$2" -name "*project.snake.name*" ! -wholename "$1/$2/.project.snake.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
while read -r PROJECT_PASCAL_NAME_PATH; do
	#[[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" $(echo "${PROJECT_PASCAL_NAME_PATH}" | sed "s/project.pascal.name/${PROJECT_PASCAL_NAME}/g")
	[[ -n "${PROJECT_PASCAL_NAME_PATH}" ]] && mv "${PROJECT_PASCAL_NAME_PATH}" "${PROJECT_PASCAL_NAME_PATH//project.pascal.name/${PROJECT_PASCAL_NAME}}"
done <<< "$(find "$1/$2" -name "*project.pascal.name*" ! -wholename "$1/$2/.project.pascal.name" | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"

[[ -f "$1/$2/develop" ]] && chmod +x "$1/$2/develop"
[[ -f "$1/$2/build" ]] && chmod +x "$1/$2/build"
[[ -f "$1/$2/deploy" ]] && chmod +x "$1/$2/deploy"
find "$1/$2" -name "*.sh" -exec chmod +x {} \;
find "$1/$2" -name "*.bash" -exec chmod +x {} \;
find "$1/$2" -type d -name bin -exec chmod -R +x {} \;

# run the fixup for the specific template
if [[ -f "$1/$2/fixup" ]]; then
	MY_PWD_FOR_FIXUP="$(pwd)"
	cd "$1/$2"
	bash ./fixup
	cd "${MY_PWD_FOR_FIXUP}"
fi
[[ -f "$1/$2/project.yml.orig" ]] && rm -fr "$1/$2/project.yml.orig"
[[ -f "$1/$2/project.yml" ]] && rm -fr "$1/$2/project.yml"
[[ -f "$1/$2/fixup" ]] && rm -fr "$1/$2/fixup"

# define a function to handle paths which are provided by a find command
function reverse_project_info_for_rob () {
        local TEMPLATE_NAME
        TEMPLATE_NAME=$(basename "$3")
  	    cp "$3/project.yml.orig" "$3/project.yml"
  	#sed -i "s/MY_PROJECT_NAME/$2/g" "${TEMPLATE_PATH}/project.yml"
  	#"${SCRIPT_ABS_PATH}"/../project-scaffold-template/rob remove
}
# export the above function so that can be used by other command or subshell
export -f reverse_project_info_for_rob

# reverse the project.yml back to orig
find "${SCRIPT_ABS_PATH}/../project-scaffold-template" -maxdepth 1 -type d ! -name . ! -wholename "${SCRIPT_ABS_PATH}/../project-scaffold-template" -exec bash -c 'reverse_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$2" {} \;

done_banner "Top level" "fix up"
