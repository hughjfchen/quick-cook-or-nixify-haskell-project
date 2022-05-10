#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "project build - fix-up"

if [ -f "${SCRIPT_ABS_PATH}"/../../"$1".tar.gz ]; then
  [[ -e ${SCRIPT_ABS_PATH}/../../"$1"_dist.tar.gz ]] && rm -fr "${SCRIPT_ABS_PATH}"/../../"$1"_dist.tar.gz
  tar zcf "${SCRIPT_ABS_PATH}"/../../"$1"_dist.tar.gz -C "$SCRIPT_ABS_PATH/../../" ./"$1".tar.gz ./cd/common ./cd/prepare-env ./cd/unpack-tarball ./cd/fix-up ./.build.output.nix.store.path ./.release.has.systemd.service ./.release.user.name ./.release.tarball.name ./"$2"
else
  warn "No ${SCRIPT_ABS_PATH}/$1.tar.gz found , can't pack distributable tarball"
fi

done_banner "Top level" "project build - fix-up"
