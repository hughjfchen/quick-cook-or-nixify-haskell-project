
# Table of Contents

1.  [Features](#org6e482eb)
2.  [Usage](#orgb6d6f9b)
3.  [How does it work](#orga0ea929)
    1.  [Nix provision](#org7012b8a)
    2.  [Generate haskell project](#org599bc98)
    3.  [Generate the build framework based on the `haskell.nix` infrastructure](#orgec1f45f)

This is a tool which can help you generate a haskell project with the haskell.nix as the build framework.


<a id="org6e482eb"></a>

# Features

This tool has following features at this moment:

-   install nix if needed
-   multiple platforms supported(currently linux and macos are supported, windows with WSL may work but not tested)
-   generate haskell project with the summoner tool
-   generate most usable nix files, including:
    -   **default.nix:** this is the major nix file which is used by all other nix files.
    -   **shell.nix:** to setup a development environment with nix-shell
    -   **cross-build.nix:** this nix expression can be used to build cross platform targets, including fully static binary linked with musl library
    -   **docker.nix:** this file can be used to build a docker image for the project


<a id="orgb6d6f9b"></a>

# Usage

Following these steps to use this tool:

1.  clone the repository.
2.  run the following command under the clone directory:
    
        /cook.sh <the directory where the project will be put> <the name of the project>
3.  follow the prompt of the screen til everything is done.


<a id="orga0ea929"></a>

# How does it work


<a id="org7012b8a"></a>

## Nix provision

This tool will check if `nix` present on the running machine, if not, it will connect to the official `nix` web site and install `nix` first, it also set the default `nix` channel to the latest stable one.


<a id="org599bc98"></a>

## Generate haskell project

This tool uses the `summoner` to do this job, for more information during the project generation, please refer to the [official site](https://github.com/kowainik/summoner).


<a id="orgec1f45f"></a>

## Generate the build framework based on the `haskell.nix` infrastructure

This tool generates the ready to go build framework for the generated project with following facts:

-   It pinned the `haskell.nix` version to the current date with the `niv` tool
-   It uses the `nixpkgs` source from the `haskell.nix` within the `default.nix` file and set the `nixpkgs` version to the latest stable nix channel
-   It sets the ghc version to the default one from the latest stable nix channel
-   It sets the `index-state` for the project within the `default.nix` to the current date
-   It generate a `shell.nix` file with following features:
    -   With the `hoogle` tool enabled
    -   With `cabal` and `hlint` enabled and set their versions to the default one of the latest stable `nix` channel
    -   You can optionally enable other tools, like `ghcid`, `niv` or `lorri`, just check the `shell.nix` file
-   It generate the `cross-build.nix` file which will build the fully static binary linked with `musl` library by default. If you would like to cross build for other platforms, check the `nix/cross-build/systems.nix` and comment out for which platforms you want
-   It also generates a `docker.nix` file which can be used to build a docker image
-   It also comes with overlay support. If you want to override some packages within the `hackage` database, you can add a `nix` file for this package under the directory `nix/overlay` and it will be picked up automatically.

