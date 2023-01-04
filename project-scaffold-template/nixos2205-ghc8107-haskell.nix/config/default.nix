{ pkgs }: 
with pkgs.lib; mapAttrs' (attr: _: { name = attr; value = import (./.  + "/${attr}") { inherit pkgs;} ; }) (filterAttrs (_: type: type == "directory" ) (builtins.readDir ./.)) 
