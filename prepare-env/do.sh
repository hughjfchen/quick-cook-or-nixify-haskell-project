#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

begin_banner "Top level" "project env prepare"

set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] \
  && . "$HOME/.nix-profile/etc/profile.d/nix.sh"
set -u

if ! type nix-build >/dev/null 2>&1; then
    info "no nix-build found, trying to install it"
    case ${THE_DISTRIBUTION_ID} in
      debian)
        [[ -e /proc/sys/kernel/unprivileged_userns_clone ]] \
          && sudo sysctl kernel.unprivileged_userns_clone=1
        curl -L https://nixos.org/nix/install | sh
        ;;
      ubuntu)
        [[ -e /proc/sys/kernel/unprivileged_userns_clone ]] \
          && sudo sysctl kernel.unprivileged_userns_clone=1
        curl -L https://nixos.org/nix/install | sh
        ;;
      Darwin)
        curl -L https://nixos.org/nix/install | sh
        ;;
      rhel|centos)
        curl -L https://nixos.org/releases/nix/nix-2.4/install | sh
        ;;
      *) ;;
    esac
    set +u
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
    set -u
    switch_to_last_stable_nix_channel
fi

# add iohk binary cache
if ! [ -f ~/.config/nix/nix.conf ] || ! grep "cache.iog.io" ~/.config/nix/nix.conf > /dev/null 2>&1 ; then
  mkdir -p ~/.config/nix
  {
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ;
    echo "substituters = https://cache.nixos.org/ https://cache.iog.io" ;
    echo "experimental-features = nix-command"
  } >> ~/.config/nix/nix.conf
fi

# in China, use the TUNA mirror for Nix binary cache
#if ! [ -f ~/.config/nix/nix.conf ] || ! grep "mirrors.tuna.tsinghua.edu.cn" ~/.config/nix/nix.conf > /dev/null 2>&1 ; then
#  mkdir -p ~/.config/nix
#  echo "substituters = https://mirrors.tuna.tsinghua.edu.cn/nix-channels/store https://cache.nixos.org/" >> ~/.config/nix/nix.conf
#fi

done_banner "Top level" "project env prepare"
