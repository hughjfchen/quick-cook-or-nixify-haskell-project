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
  # make a temp dir, make a $1 dir under the temp dir, copy all need-to-pack files under that dir, cd to that temp and pack them up
  STORE_PACK_FILES_TEMP_DIR=$(mktemp -d)
  PACK_FILE_DIR="$STORE_PACK_FILES_TEMP_DIR/$1"
  mkdir -p "$PACK_FILE_DIR"
  cp -R "$SCRIPT_ABS_PATH/../../$1.tar.gz" \
    "$SCRIPT_ABS_PATH"/../../cd \
    "$SCRIPT_ABS_PATH"/../../.built.attribute.name \
    "$SCRIPT_ABS_PATH"/../../.build.output.nix.store.path \
    "$SCRIPT_ABS_PATH"/../../.release.has.systemd.service \
    "$SCRIPT_ABS_PATH"/../../.release.user.name \
    "$SCRIPT_ABS_PATH"/../../.release.tarball.name \
    "$SCRIPT_ABS_PATH"/../../"$2" \
    "$PACK_FILE_DIR"

  # clean up some unused stuff
  rm -fr "$PACK_FILE_DIR"/cd/nix "$PACK_FILE_DIR"/cd/arion

  tar zcf "${SCRIPT_ABS_PATH}"/../../"$1"_dist.tar.gz -C "$STORE_PACK_FILES_TEMP_DIR" ./"$1"
  rm -fr "$STORE_PACK_FILES_TEMP_DIR"
else
  warn "No ${SCRIPT_ABS_PATH}/$1.tar.gz found , can't pack distributable tarball"
fi

done_banner "Top level" "project build - fix-up"
