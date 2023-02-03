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

PROJECT_ORIG_NAME="$2"
PROJECT_PASCAL_NAME=$(cat "$1/$COMPPATH/.project.pascal.name")
PROJECT_SNAKE_NAME=$(cat "$1/$COMPPATH/.project.snake.name")
for THE_NAME in PROJECT_ORIG_NAME PROJECT_SNAKE_NAME PROJECT_PASCAL_NAME
do
  THE_LOWER_NAME=$(echo ${THE_NAME} | tr '[:upper:]' '[:lower:]' | tr '_' '.')
  while read -r THE_NAME_PATH1; do
    [[ -n "${THE_NAME_PATH1}" ]] \
        && mv "${THE_NAME_PATH1}" "${THE_NAME_PATH1//${THE_LOWER_NAME}/${!THE_NAME}}"
  done <<< "$(find "$1/$COMPPATH" -name "*${THE_LOWER_NAME}*" \
      ! -wholename "$1/$COMPPATH/.${THE_LOWER_NAME}" \
      | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
  for CD_EXTRA_PATH in env config
  do
    if [[ -d "$1/$2/$CD_EXTRA_PATH" ]]; then
      while read -r THE_NAME_PATH2; do
      [[ -n "${THE_NAME_PATH2}" ]] \
          && mv "${THE_NAME_PATH2}" "${THE_NAME_PATH2//${THE_LOWER_NAME}/${!THE_NAME}}"
      done <<< "$(find "$1/$2/$CD_EXTRA_PATH" -name "*${THE_LOWER_NAME}*" \
          ! -wholename "$1/$2/$CD_EXTRA_PATH/.${THE_LOWER_NAME}" \
          | awk '{ print length() "|" $0 | "sort -nr | cut -f2 -d\"|\""}')"
    fi
  done
done

# make some file executable
for NEED_TO_FIX1 in "develop" "build" "deploy" "arion" "*.sh" "*.bash"
do
  find "$1/$2" -name "$NEED_TO_FIX1" -exec chmod +x {} \;
done
find "$1/$2" -type d -name bin -exec chmod -R +x {} \;

# run the fixup for the specific template
find "$1/$2" -name fixup -exec bash {} \;

# clean up some files
for NEED_TO_FIX2 in "project.yml.orig" "project.xml" "fixup"
do
  find "$1/$2" -name "$NEED_TO_FIX2" -exec rm -fr {} \;
done

# no need to do following
# define a function to handle paths which are provided by a find command
#function reverse_project_info_for_rob () {
#        local TEMPLATE_NAME
#        TEMPLATE_NAME=$(basename "$3")
#        cp "$3/project.yml.orig" "$3/project.yml"
        #sed -i "s/MY_PROJECT_NAME/$COMPPATH/g" "${TEMPLATE_PATH}/project.yml"
        #"${SCRIPT_ABS_PATH}"/../project-scaffold-template/rob remove
#}
# export the above function so that can be used by other command or subshell
#export -f reverse_project_info_for_rob

# reverse the project.yml back to orig
#find "${SCRIPT_ABS_PATH}/../project-scaffold-template" -maxdepth 1 -type d \
#    ! -name . ! -wholename "${SCRIPT_ABS_PATH}/../project-scaffold-template" \
#    -exec bash -c 'reverse_project_info_for_rob "$0" "$1" "$2"' "${SCRIPT_ABS_PATH}" "$COMPPATH" {} \;

done_banner "Top level" "fix up"
