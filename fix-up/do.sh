#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

begin_banner "Top level" "fix up"

mv "$1/$2/template01.cabal" "$1/$2/$2.cabal"
mv "$1/$2/src/Core/Template01.hs" "$1/$2/src/Core/$2.hs"
mv "$1/$2/src/Capability/Template01.hs" "$1/$2/src/Capability/$2.hs"
mv "$1/$2/app/AppCapability/Template01.hs" "$1/$2/app/AppCapability/$2.hs"

chmod +x "$1/$2/start-dev"

MY_INDEX_STATE=$(date +%Y-%m-%dT00:00:00Z)
#update index-state in the cabal.project file
echo "index-state : ${MY_INDEX_STATE}" >> "$1/$2"/cabal.project

done_banner "Top level" "fix up"
