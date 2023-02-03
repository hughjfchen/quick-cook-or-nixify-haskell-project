#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "project deploy - fixup - undo"

set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . "$HOME"/.nix-profile/etc/profile.d/nix.sh
set -u

if [ -n "$RELEASE_HAS_SYSTEMD_SERVICE" ]; then
    THE_SERVICE_NAME=$(awk -F'"' '/unitsToStart\+\=/ {print $2}' "$NIX_STORE_PATH"/bin/setup-systemd-units)
    # [ -n "$THE_SERVICE_NAME" ] && sudo systemctl stop "$THE_SERVICE_NAME" && sudo systemctl disable "$THE_SERVICE_NAME"
    [ -n "$THE_SERVICE_NAME" ] && sudo systemctl stop "$THE_SERVICE_NAME"
fi

done_banner "Top level" "project deploy - fixup - undo"
