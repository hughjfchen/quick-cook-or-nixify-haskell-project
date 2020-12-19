
# Table of Contents

1.  [Features](#org87edada)
2.  [Usage](#orgc7dd06a)

This is a tool which can help you generate a haskell project which uses the haskell.nix as the build framework.


<a id="org87edada"></a>

# Features

This tool has following features at this moment:

-   provision nix if needed
-   generate haskell project with the summoner tool
-   generate most usable nix files, including:
    -   **default.nix:** this is the major nix file
    -   **shell.nix:** to setup a development environment with nix-shell
    -   **cross-build.nix:** this nix expression can be used to build cross platform targets, including fully static linked binary with musl library
    -   **docker.nix:** this file can be used to build a docker image for the project


<a id="orgc7dd06a"></a>

# Usage

Following these steps to use this tool:

1.  clone the repository
2.  run the following command under the clone directory:
    
    <p class="verse">
    ./cook.sh <the porject directory> <the project name><br />
    </p>

