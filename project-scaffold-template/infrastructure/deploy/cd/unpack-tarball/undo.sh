#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "project deploy - unpacking - undo"

set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

# do we need to remove the nix stoer path? but there're unique, leave it for now.a
# ok, I want to do it
if [ -f "$SCRIPT_ABS_PATH/../../$RELEASE_TARBALL_NAME.tar.gz" ]; then
    tar zPtvf "$SCRIPT_ABS_PATH/../../$RELEASE_TARBALL_NAME.tar.gz"|awk '{print $NF}'|grep '/nix/store/'|awk -F'/' '{print "/nix/store/" $4}'|sort|uniq|xargs sudo rm -fr
else
    warn "cannot find the release tarball at $SCRIPT_ABS_PATH/../../$RELEASE_TARBALL_NAME.tar.gz, skip the undo phase."
fi

done_banner "Top level" "project deploy - unpacking - undo"
