#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "project deploy - fix-up"

# some command fix up for systemd service, especially web server
getent group nogroup > /dev/null || sudo groupadd nogroup

if [ -n "$NIX_STORE_PATH" ]; then
  if [ -n "$RELEASE_HAS_SYSTEMD_SERVICE" ] && [ "true" == "$RELEASE_HAS_SYSTEMD_SERVICE" ]; then
    sudo "$NIX_STORE_PATH"/bin/setup-systemd-units
  else
    info "To use the program, type $NIX_STORE_PATH/bin/java-analyzer-runner-bin-sh at the command prompt."
  fi
else
  my_exit "cannot determine the build artifect path without the NIX_STORE_PATH environment variable."
fi

done_banner "Top level" "project deploy - fix-up"
