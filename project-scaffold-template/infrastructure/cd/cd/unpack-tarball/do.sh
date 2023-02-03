#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "project deploy - unpacking"

if [ -e "${SCRIPT_ABS_PATH}"/../../"$RELEASE_TARBALL_NAME".tar.gz ]; then
  if [ -n "$RELEASE_USER_NAME" ]; then
    sudo tar zPxf "$SCRIPT_ABS_PATH/../../$RELEASE_TARBALL_NAME.tar.gz" --owner "$RELEASE_USER_NAME" --group "$RELEASE_USER_NAME"
  else
    sudo tar zPxf "$SCRIPT_ABS_PATH/../../$RELEASE_TARBALL_NAME.tar.gz"
  fi
else
  my_exit "No ${SCRIPT_ABS_PATH}/../../$RELEASE_TARBALL_NAME.tar.gz found, can't unpack tarball" 127
fi

done_banner "Top level" "project deploy - unpacking"
