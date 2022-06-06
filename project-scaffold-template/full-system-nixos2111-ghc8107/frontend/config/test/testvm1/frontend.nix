{ pkgs, lib, config, ... }:
let inherit pkgs lib config;
in {
  "currentServer" = "www.detachmentsoft.top";
  "backendServer" = "testvm1";
}
