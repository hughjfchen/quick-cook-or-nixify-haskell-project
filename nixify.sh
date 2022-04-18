#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/common/common.sh

usage_and_exit () {
            echo "Usage: nixify.sh <project root path> <project name> <generate|template>"
            exit 1
}

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path "$0")

if [ "$#" != 3 ]; then 
	usage_and_exit
fi

begin_banner "Top level" "project nixifying"

"${SCRIPT_ABS_PATH}"/prepare-env/do.sh

# make a temp dir
quick_cook_nixify_tmp_dir=$(mktemp -d -t quick-cook-nixify-"$(date +%Y%m%d%H%M%S)"-XXXXXXXXXX)

case $3 in
  generate)
         echo "nixify only support template"

          ;;
  template)
         "${SCRIPT_ABS_PATH}"/project-scaffold-template/do.sh "${quick_cook_nixify_tmp_dir}" "$2"

         "${SCRIPT_ABS_PATH}"/fix-up/do.sh "${quick_cook_nixify_tmp_dir}" "$2"

         [ -d "$1/$2/ci" ] && mv "$1/$2/ci" "$1/$2/ci.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp -R "${quick_cook_nixify_tmp_dir}/$2/ci" "$1/$2"
         [ -d "$1/$2/cd" ] && mv "$1/$2/cd" "$1/$2/cd.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp -R "${quick_cook_nixify_tmp_dir}/$2/cd" "$1/$2"
         [ -d "$1/$2/nix" ] && mv "$1/$2/nix" "$1/$2/nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp -R "${quick_cook_nixify_tmp_dir}/$2/nix" "$1/$2"
         [ -f "$1/$2/default.nix" ] && mv "$1/$2/default.nix" "$1/$2/default.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/default.nix" "$1/$2"
         [ -f "$1/$2/shell.nix" ] && mv "$1/$2/shell.nix" "$1/$2/shell.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/shell.nix" "$1/$2"
         [ -f "$1/$2/cross-build.nix" ] && mv "$1/$2/cross-build.nix" "$1/$2/cross-build.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/cross-build.nix" "$1/$2"
         [ -f "$1/$2/docker.nix" ] && mv "$1/$2/docker.nix" "$1/$2/docker.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/docker.nix" "$1/$2"
         [ -f "$1/$2/tarball.nix" ] && mv "$1/$2/tarball.nix" "$1/$2/tarball.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/tarball.nix" "$1/$2"
         [ -f "$1/$2/arion-pkgs.nix" ] && mv "$1/$2/arion-pkgs.nix" "$1/$2/arion-pkgs.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/arion-pkgs.nix" "$1/$2"
         [ -f "$1/$2/arion-compose.nix" ] && mv "$1/$2/arion-compose.nix" "$1/$2/arion-compose.nix.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/arion-compose.nix" "$1/$2"
         [ -f "$1/$2/hie.yaml" ] && mv "$1/$2/hie.yaml" "$1/$2/hie.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/hie.yaml" "$1/$2"
         [ -f "$1/$2/fourmolu.yaml" ] && mv "$1/$2/fourmolu.yaml" "$1/$2/fourmolu.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/fourmolu.yaml" "$1/$2"
         [ ! -f "$1/$2/cabal.project.local" ] && cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project.local" "$1/$2"
         if [ -f "$1/$2/cabal.project" ]; then
             THE_INDEX_STATE=$(grep index-state "${quick_cook_nixify_tmp_dir}/$2/cabal.project")
             grep -v "index-state" "$1/$2/cabal.project" > "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig"
             echo "${THE_INDEX_STATE}" >> "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig"
             mv "$1/$2/cabal.project" "$1/$2/cabal.project.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
             cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project.orig" "$1/$2/cabal.project"
         else
             cp "${quick_cook_nixify_tmp_dir}/$2/cabal.project" "$1/$2"
         fi
         [ -f "$1/$2/.gitignore" ] && mv "$1/$2/.gitignore" "$1/$2/.gitignore.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/.gitignore" "$1/$2"
         [ -f "$1/$2/.dir-locals.el" ] && mv "$1/$2/.dir-locals.el" "$1/$2/.dir-locals.el.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/.dir-locals.el" "$1/$2"
         [ -f "$1/$2/.ghci" ] && mv "$1/$2/.ghci" "$1/$2/.ghci.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/.ghci" "$1/$2"
         [ -f "$1/$2/.hlint.yaml" ] && mv "$1/$2/.hlint.yaml" "$1/$2/.hlint.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/.hlint.yaml" "$1/$2"
         [ -f "$1/$2/develop" ] && mv "$1/$2/develop" "$1/$2/develop.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/develop" "$1/$2"
         [ -f "$1/$2/build" ] && mv "$1/$2/build" "$1/$2/build.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/build" "$1/$2"
         [ -f "$1/$2/deploy" ] && mv "$1/$2/deploy" "$1/$2/deploy.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/deploy" "$1/$2"
	 # for executables, we need to copy to override them after applying tempalte(Why?)
	 cp "${SCRIPT_ABS_PATH}"/deployment-framework/arion "$1/$2/cd/" && chmod +x "$1/$2/cd/arion"
         mkdir -p "$1/$2/.github/workflows"
         [ -f "$1/$2/.github/workflows/nix-ci.yml" ] && mv "$1/$2/.github/workflows/nix-ci.yml" "$1/$2/.github/workflows/nix-ci.yml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         cp "${quick_cook_nixify_tmp_dir}/$2/.github/workflows/nix-ci.yml" "$1/$2/.github/workflows/"

         if [ -f "$1/$2/$2.cabal" ] && [ -f "$1/$2/stack.yaml" ]; then
             mv "$1/$2/stack.yaml" "$1/$2/stack.yaml.bak.by.nixify.$(date +%Y%m%d%H%M%S)"
         fi
	 ;;
 *)
	 ;;
esac


# clean up
rm -fr "${quick_cook_nixify_tmp_dir}"

# everything's done
done_banner "Top level" "project nixifying"
