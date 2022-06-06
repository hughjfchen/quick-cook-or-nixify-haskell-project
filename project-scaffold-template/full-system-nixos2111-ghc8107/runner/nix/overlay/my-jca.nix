self: prev:
# java jar packages
prev.stdenv.mkDerivation {
  name = "my-jca";
  version = "4611";
  src = builtins.fetchurl {
    url =
      "https://public.dhe.ibm.com/software/websphere/appserv/support/tools/jca/jca4611.jar";
    sha256 = "16wrbxl229qr4bnmdpdi1swmgfgy8irq35gmbcicgaq3grga781q";
  };
  dontBuild = true;
  dontUnpack = true;
  unpackPhase = "";
  installPhase = ''
    mkdir -p $out/share/java
    cp $src $out/share/java/
  '';
}
