{ modules ? [ ], pkgs, env, ... }:

let _pkgs = pkgs;
in let
  pkgs = if builtins.typeOf _pkgs == "path" then
    import _pkgs
  else if builtins.typeOf _pkgs == "set" then
    _pkgs
  else
    builtins.abort
    "The pkgs argument must be an attribute set or a path to an attribute set.";

  inherit (pkgs)
  ;
  lib = pkgs.lib;

  configBuilder = lib.evalModules { modules = builtinModules ++ modules; };

  builtinModules = [ argsModule ] ++ [ enableModule ]
    ++ import ../../../../module-list.nix ++ import ./module-list.nix;

  argsModule = {
    _file = ./config.nix;
    key = ./config.nix;
    config._module.check = true;
    config._module.args.pkgs = lib.mkIf (pkgs != null) (lib.mkForce pkgs);
    config._module.args.env = lib.mkIf (env != null) (lib.mkForce env);
  };

  enableModule = {
    config.db.enable = true;
    config.db-gw.enable = true;
    config.api-gw.enable = true;
    config.messaging.enable = true;
    config.runner.enable = true;
  };

in configBuilder // {
  # throw in lib and pkgs for repl convenience
  inherit lib;
  inherit (configBuilder._module.args) pkgs;
}
