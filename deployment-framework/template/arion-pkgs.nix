# for a docker image, we're only interested in the fully staticlly linked exe based on x86-64 musl.
let
  x64musl = (import ./cross-build.nix { }).x86-musl64;
in
  x64musl.pkgs // { {{ name }} = x64musl.{{ name }}; }
