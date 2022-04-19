---
author: Hugh JF Chen
date: 2020-12-18
title: Readme
---

This is a tool which can help you quickly generate a haskell project
based on best practice to structure your program and with the
[`nix`](https://nixos.org) and
[`haskell.nix`](https://github.com/input-output-hk/haskell.nix) as the
build framework and `docker`, `docker-compose` and `arion-compose` as
the deployment framework.

# Motivation

We want to write code as quickly as possible when we build an
application. however, the reality is that we need to build the
application and deploy it for testing. Without the build and deploy, we
can't test it. And without testing, we don't know if our application
works as expected. So we need to put a lot of effort to make our
application build and deploy for testing. fortunately, we can reduce the
effort by generating the build and deploy infrastructure for our
application. On the other hand, our application need to follow some best
practice to structure our source code for maintenance. And this can also
achieve by generating project based on a template.

Based on the above, I create this project generator to quickly cook a
project if I want to build a haskell application.

# Features

This tool has following features at this moment:

-   install `nix`, `docker`, `docker-compose` if needed
-   multiple platforms supported(currently `linux` and `macos` are
    supported, `windows` with `WSL` may work but not tested)
-   create project based on existing project templates with
    [`rob`](https://github.com/GianlucaGuarini/rob)
    -   currently there are one existing templates
        -   `nixos2111-ghc8107-haskell.nix`
    -   very easy to add new project template with new build
        infrastructure
-   generate most usable nix files, including:
    default.nix  
    this is the major nix file which is used by all other nix files.

    shell.nix  
    to setup a development environment with nix-shell

    cross-build.nix  
    this nix expression can be used to build cross platform targets,
    including fully static binary linked with musl library

    docker.nix  
    this file can be used to build a docker image for the project

    tarball.nix  
    this file can be used to tar up the executable and its dependencies
    as a tarball for the project

    arioin-pkgs.nix  
    this file can be used to define the package set for `arion-compose`
    to build the container

    arion-compose.nix  
    this file can be used to compose services by `arion-compose` and
    orchestrate these services as `docker-compose`

    build  
    use this script to build the project

    deploy  
    use this script to deploy the project

    develop  
    use this script to start development of the project

    arion  
    the utility to check the deployment status
-   ready to be built project, just type `build` and you can get the
    project executable
-   ready to be deployed project, just type `deploy` and your project
    will be deployed as `docker` container
-   buildable source code stub following the `three layer cake` pattern
    practice
-   nixify existing projects, i.e., make existing projects build and
    deploy using this build and deploy framework

# Usage

Following these steps to use this tool:

1.  clone the repository.

2.  run the following command under the clone directory:

    ``` bash
    ./cook.sh <the directory where the project will be put> <the name of the project> <generate|template>
     - generate means generate new project with summon
     - template means create project based on existing template
    ```

3.  follow the prompt of the screen til everything is done.

# How does it work

## Nix provision

This tool will check if `nix` present on the running machine, if not, it
will connect to the official `nix` web site and install `nix` first, it
also set the default `nix` channel to the latest stable one.

## Generate haskell project

This tool uses the `summoner` to do this job, for more information
during the project generation, please refer to the [official
site](https://github.com/kowainik/summoner).

## Generate the build framework based on the `haskell.nix` infrastructure

This tool generates the ready to go build framework for the generated
project with following facts:

-   It pinned the `haskell.nix` version to the current date with the
    `niv` tool
-   It uses the `nixpkgs` source from the `haskell.nix` within the
    `default.nix` file and set the `nixpkgs` version to the latest
    stable nix channel
-   It sets the `ghc` version to the default one from the latest stable
    nix channel
-   It sets the `index-state` for the project within the `default.nix`
    to the one of the `haskell.nix` internal index state
-   It generate a `shell.nix` file with following features:
    -   With the `hoogle` tool enabled
    -   With `cabal`, `hasktages` and `haskell-language-server` enabled
        and set their versions to the default one of the latest stable
        `nix` channel
    -   You can optionally enable other tools, like `ghcid`, `niv` or
        `lorri`, just check the `shell.nix` file
-   It generate the `cross-build.nix` file which will build the fully
    static binary linked with `musl` library by default. If you would
    like to cross build for other platforms, check the
    `nix/cross-build/systems.nix` and comment out for which platforms
    you want
-   It also generates a `docker.nix` file which can be used to build a
    docker image
-   It also generates a `tarball.nix` file which can be used to build a
    tarball file
-   It also comes with overlay support. If you want to override some
    packages within the `hackage` database, you can add a `nix` file for
    this package under the directory `nix/overlay` and it will be picked
    up automatically.

# Setup development environment

Besides the `shell.nix`, this tool also generates some other files to
help you quick start the development:

-   The tool assumes you will use `emacs` with `haskell-mode` and `lsp`
    combination as the development environment. If you use other tools,
    you need to figure out how to setup the development environment
    yourself.
-   The tool will generate a `cabal.project` file with the `index-state`
    set to the one of the `haskell.nix` internal index state.
-   The tool will generate a `cabal.project.local` to make sure the
    haskell tools will use the new style cabal commands.
-   The tool will also generate a `.dir-locals.el` and set the `HLS`
    executable path.

With above files in place, you can just run the `nix-shell` under the
project directory to enter a `nix-shell` and start `emacs` within the
`nix-shell` and start to code.

# Build

TO BE WRITTEN

# Deploy

TO BE WRITTEN

# Nixify existing projects

TO BE WRITTEN

# Todos

Following are the incomplete list of the features I want to implement in
near future:

-   Add github action CI support based on `haskell.nix` DONE
-   Add more project templates
