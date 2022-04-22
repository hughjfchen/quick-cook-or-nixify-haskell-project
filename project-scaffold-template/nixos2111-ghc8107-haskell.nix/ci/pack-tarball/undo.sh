
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

begin_banner "Top level" "project deploy - unpacking"

tar zPxf "${SCRIPT_ABS_PATH}/{{name|toSnake}}.tar.gz

done_banner "Top level" "project deploy - unpacking"
