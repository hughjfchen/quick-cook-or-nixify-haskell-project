#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

# ShellCheck source=/dev/null
. "$(dirname "$0")"/common/common.sh

usage_and_exit () {
            echo "Usage: nixify.sh <project root path> <project name>"
            exit 1
}

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

if [ "$#" != 2 ]; then
	usage_and_exit
fi

begin_banner "Top level" "project nixifying"

"${SCRIPT_ABS_PATH}"/prepare-env/do.sh

# make a temp dir
quick_cook_nixify_tmp_dir=$(mktemp -d -t quick-cook-nixify-"$(date +%Y%m%d%H%M%S)"-XXXXXXXXXX)

"${SCRIPT_ABS_PATH}"/project-scaffold-template/do.sh "${quick_cook_nixify_tmp_dir}" "$2"

"${SCRIPT_ABS_PATH}"/fix-up/do.sh "${quick_cook_nixify_tmp_dir}" "$2"

for DIR_NAME1 in "ci" "cd" "nix" "env" "config" "k8s"
do
    [ -d "$1/$2/$DIR_NAME1" ] && mv "$1/$2/$DIR_NAME1" "$1/$2/$DIR_NAME1.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
    [ -d  "${quick_cook_nixify_tmp_dir}/$2/$DIR_NAME1" ] && cp -R "${quick_cook_nixify_tmp_dir}/$2/$DIR_NAME1" "$1/$2"
done

for FILE_NAME1 in "default.nix" "shell.nix" "release.nix" "cross-build.nix" \
    "docker.nix" "tarball.nix" ".editorconfig" ".env" \
    "hie.yaml" "fourmolu.yaml" ".gitignore" ".dir-locals.el" ".ghci" \
    ".hlint.yaml" "develop" "build" "deploy" ".ghcid" ".ghcid.lib" ".ghcid.app"
do
    [ -f "$1/$2/$FILE_NAME1" ] && mv "$1/$2/$FILE_NAME1" "$1/$2/$FILE_NAME1.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
    [ -f "${quick_cook_nixify_tmp_dir}/$2/$FILE_NAME1" ] && cp "${quick_cook_nixify_tmp_dir}/$2/$FILE_NAME1" "$1/$2"
done

[ ! -f "$1/$2/cabal.project.local" ] && [ -f "${quick_cook_nixify_tmp_dir}/$2/cabal.project.local" ] \
    && cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project.local" "$1/$2"

if [ -f "$1/$2/cabal.project" ]; then
    THE_INDEX_STATE=$(grep index-state "${quick_cook_nixify_tmp_dir}/$2/cabal.project")
    grep -v "index-state" "$1/$2/cabal.project" > "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig"
    echo "${THE_INDEX_STATE}" >> "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig"
    mv "$1/$2/cabal.project" "$1/$2/cabal.project.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
    cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig" "$1/$2/cabal.project"
else
    [ -f "${quick_cook_nixify_tmp_dir}/$2/cabal.project" ] && cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project" "$1/$2"
fi

mkdir -p "$1/$2/.github/workflows"
[ -f "$1/$2/.github/workflows/nix-ci.yml" ] \
    && mv "$1/$2/.github/workflows/nix-ci.yml" "$1/$2/.github/workflows/nix-ci.yml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
[ -f "${quick_cook_nixify_tmp_dir}/$2/.github/workflows/nix-ci.yml" ] && cp "${quick_cook_nixify_tmp_dir}/$2/.github/workflows/nix-ci.yml" "$1/$2/.github/workflows/"

if [ -f "$1/$2/$2.cabal" ] && [ -f "$1/$2/stack.yaml" ]; then
    mv "$1/$2/stack.yaml" "$1/$2/stack.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
fi
if [ -f "$1/$2/$2.cabal" ] && [ -f "$1/$2/package.yaml" ]; then
    mv "$1/$2/package.yaml" "$1/$2/package.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
fi

# clean up
rm -fr "${quick_cook_nixify_tmp_dir}"

# everything's done
done_banner "Top level" "project nixifying"
