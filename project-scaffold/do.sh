#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "project scaffold"

#set +u to temp work around the nix script
set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

nix-shell '<nixpkgs>' -p haskellPackages.summoner --run "mkdir -p $1; cd $1; summon new $2"
#nix-shell '<nixpkgs>' -p hello --run "mkdir -p $1; cd $1; echo $2; hello"

done_banner "Top level" "project scaffold"
