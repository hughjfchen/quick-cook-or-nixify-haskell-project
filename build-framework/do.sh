#!/usr/bin/env bash
if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

begin_banner "Top level" "build framework"

cp -R "${SCRIPT_ABS_PATH}"/template/* "$1/$2/"

for FILE_TO_SED in $(grep -R MY_PROJECT_NAME "$1/$2/"*|awk -F":" '{print $1}'|sort|uniq)
do
   sed -i.bak.for.sed.inplace.edit "s/MY_PROJECT_NAME/$2/g" "${FILE_TO_SED}"
   rm -fr "${FILE_TO_SED}.bak.for.sed.inplace.edit"
done

MY_INDEX_STATE=$(date +%Y-%m-%dT00:00:00Z)
#update index-state in the cabal.project file
echo "packages : ." > "$1/$2"/cabal.project
echo "index-state : ${MY_INDEX_STATE}" >> "$1/$2"/cabal.project
# touch a cabal.project.local to make sure emacs dante use new-impure-nix build method
touch "$1/$2"/cabal.project.local
# also for emacs dante target
echo "((nil . ((dante-target . \"test:$2-test\"))))" > "$1/$2"/.dir-locals.el
[ -d "$1/$2"/src ] && echo "((nil . ((dante-target . \"lib:$2\"))))" > "$1/$2"/src/.dir-locals.el
[ -d "$1/$2"/app ] && echo "((nil . ((dante-target . \"exe:$2\"))))" > "$1/$2"/app/.dir-locals.el
[ -d "$1/$2"/test ] && echo "((nil . ((dante-target . \"test:$2-test\"))))" > "$1/$2"/test/.dir-locals.el

# setup the .ghci for great experience
{ echo ':def hg \x -> return $ ":!hoogle \"" ++ x ++ "\""' ; echo ':def hgi \x -> return $ ":!hoogle --info \"" ++ x ++ "\""' ; echo ':def hgr \x -> return $ ":!lynx -dump -dont_wrap_pre -with_backspaces -nolist -nonumbers  \"" ++ "https://hoogle.haskell.org/?hoogle=" ++ x ++ "\""' ; } >> "$1/$2/.ghci"

#update niv sources list
set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . "$HOME/.nix-profile/etc/profile.d/nix.sh"
set -u

# Pin to the latest stable channel instead
#MY_CHANNEL=$(nix-channel --list | awk -F"/" '{print $NF}')
MY_CHANNEL=$(get_last_stable_nix_channel)
MY_CHANNEL_NUM=$(echo "${MY_CHANNEL}" | awk -F"-" '{print $2}')
#nix-shell '<nixpkgs>' -p haskellPackages.niv --run "cd $1/$2; niv init --no-nixpkgs; niv add NixOS/nixpkgs -n nixpkgs -b nixos-${MY_CHANNEL}; niv add NixOS/nixpkgs -n nixpkgs-darwin -b nixpkgs-${MY_CHANNEL_NUM}-darwin; niv add input-output-hk/haskell.nix"

case ${THE_DISTRIBUTION_ID} in
  debian|ubuntu|rhel|centos|nixos)
    cd "$1/$2"
    "${SCRIPT_ABS_PATH}"/niv init --no-nixpkgs
    # following is for Linux
    "${SCRIPT_ABS_PATH}"/niv add NixOS/nixpkgs -n nixpkgs -b "nixos-${MY_CHANNEL_NUM}"
    # following is for OSX
    "${SCRIPT_ABS_PATH}"/niv add NixOS/nixpkgs -n nixpkgs-darwin -b "nixpkgs-${MY_CHANNEL_NUM}-darwin"
    "${SCRIPT_ABS_PATH}"/niv add input-output-hk/haskell.nix
    ;;
  *)
    nix-shell '<nixpkgs>' -p haskellPackages.niv --run "cd $1/$2; niv init --no-nixpkgs; niv add NixOS/nixpkgs -n nixpkgs -b nixos-${MY_CHANNEL}; niv add NixOS/nixpkgs -n nixpkgs-darwin -b nixpkgs-${MY_CHANNEL_NUM}-darwin; niv add input-output-hk/haskell.nix"
    ;;
esac

# set the nixpkgs to the latest stable channel in the nix file
# also set the ghc version and cabal version accordingly.
MY_NIXPKGS=$(echo "${MY_CHANNEL}" | sed 's/\-darwin//g' | sed 's/nixos/nixpkgs/g' | sed 's/\.//g')
MY_NIXPKGS_URL=$(NIX_PATH="mypkg=$1/$2/nix/sources.nix" nix-instantiate --eval  -E '(import <mypkg>).nixpkgs.url' | sed 's/\(^"\)\(.*\)\("$\)/\2/g')
MY_GHC_VER=$(nix-env -f "${MY_NIXPKGS_URL}" --quiet -qaP ghc -A haskellPackages | sed -n '/ghc\ /p' | awk '{print $NF}' | sed 's/\-//g' | sed 's/\.//g')
MY_CABAL_VER=$(nix-env -f "${MY_NIXPKGS_URL}" --quiet -qaP cabal-install -A haskellPackages | sed -n '/cabal\-install\ /p' | awk '{print $NF}' | awk -F"-" '{print $NF}') 
MY_HLINT_VER=$(nix-env -f "${MY_NIXPKGS_URL}" --quiet -qaP hlint -A haskellPackages | sed -n '/hlint\ /p' | awk '{print $NF}' | awk -F"-" '{print $NF}') 
sed -i.bak.for.replace.my_nixpkgs "s/MY_NIXPKGS/${MY_NIXPKGS}/g" "$1/$2/default.nix"
sed -i.bak.for.replace.my_ghc_ver "s/MY_GHC_VER/${MY_GHC_VER}/g" "$1/$2/default.nix"
sed -i.bak.for.replace.my_index_state "s/MY_INDEX_STATE/${MY_INDEX_STATE}/g" "$1/$2/default.nix"
sed -i.bak.for.replace.my_cabal_ver "s/MY_CABAL_VER/${MY_CABAL_VER}/g" "$1/$2/shell.nix"
sed -i.bak.for.replace.my_hlint_ver "s/MY_HLINT_VER/${MY_HLINT_VER}/g" "$1/$2/shell.nix"
rm -fr "$1/$2"/default.nix.bak.for.replace.my_nixpkgs
rm -fr "$1/$2"/default.nix.bak.for.replace.my_ghc_ver
rm -fr "$1/$2"/default.nix.bak.for.replace.my_index_state
rm -fr "$1/$2"/shell.nix.bak.for.replace.my_cabal_ver
rm -fr "$1/$2"/shell.nix.bak.for.replace.my_hlint_ver

# remove the cabal.project.local and .dir-locals.el from the .gitignore to make sure it can add to the git repostory
if [ -f "$1/$2"/.gitignore ]; then
  sed -i.bak.for.deleting.cabal.project.local "/cabal.project.local/d" "$1/$2"/.gitignore
  sed -i.bak.for.deleting.dir-locals "/.dir-locals.el/d" "$1/$2"/.gitignore
  rm -fr "$1/$2"/.gitignore.bak.for.deleting.cabal.project.local
  rm -fr "$1/$2"/.gitignore.bak.for.deleting.dir-locals
fi

done_banner "Top level" "build framework"
