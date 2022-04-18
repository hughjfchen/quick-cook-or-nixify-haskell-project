let
  mypkgs = import ./default.nix { };
in
  mypkgs.pkgs // { {{name}} = mypkgs; }
