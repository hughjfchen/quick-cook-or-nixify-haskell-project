self: prev:
let
  # extra runtime dependencies
  ibm-dtfj-p2-site = prev.stdenv.mkDerivation rec {
    name = "ibm-dtfj-p2-site";
    pkgprefix = "com.ibm.dtfj";
    version = "1.12.29003.202006111057";
    src = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/p2.index";
      sha256 = "1kgn7jgndv52zd9v4cw2511i5d99k759fgykvpv7lfll12s1r46j";
    };
    content-jar = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/content.jar";
      sha256 = "0g2bnv74nh14i5lwrlhfafr9x5wa69jl34529bbchmm31z8yar2w";
    };
    content-xml-xz = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/content.xml.xz";
      sha256 = "1m3nzqw9fa4b78m98j9k16n6qnwgw77w3m46kdji858kyhg5gys9";
    };
    artifacts-jar = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/artifacts.jar";
      sha256 = "1d4djss1m2vclpy4hnps76f8sp5qq01rmzji30734sj0mhcw4x5i";
    };
    artifacts-xml-xz = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/artifacts.xml.xz";
      sha256 = "1d3g7xixn0yh0hdx2dfdv4nav6pf3955ljhlprs2526y5p6ks2ns";
    };
    features-jar = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/features/${pkgprefix}.feature_${version}.jar";
      sha256 = "1jfmb96qn422wrqkkbmd8n0lgdpx0c2g2lbhas00j8020w29yiw8";
    };
    plugins-api-jar = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/plugins/${pkgprefix}.api_${version}.jar";
      sha256 = "0qcmhdh2skbjqmfi42sq4i7zfr2arkvna0qb3k4ci1d36c21d4y1";
    };
    plugins-j9-jar = builtins.fetchurl {
      url =
        "https://public.dhe.ibm.com/ibmdl/export/pub/software/websphere/runtimes/tools/dtfj/plugins/${pkgprefix}.j9_${version}.jar";
      sha256 = "1v2vs3xwngsqsvy2vhajqm3i42dx8j99yacir1sp7xbicx5cdiy6";
    };
    dontUnpack = true;
    unpackPhase = "";
    buildCommand = ''
      mkdir -p $out
      mkdir -p $out/features
      mkdir -p $out/plugins
      cp $src $out/$(stripHash $src)
      cp ${content-jar} $out/$(stripHash ${content-jar})
      cp ${content-xml-xz} $out/$(stripHash ${content-xml-xz})
      cp ${artifacts-jar} $out/$(stripHash ${artifacts-jar})
      cp ${artifacts-xml-xz} $out/$(stripHash ${artifacts-xml-xz})
      cp ${features-jar} $out/features/$(stripHash ${features-jar})
      cp ${plugins-api-jar} $out/plugins/$(stripHash ${plugins-api-jar})
      cp ${plugins-j9-jar} $out/plugins/$(stripHash ${plugins-j9-jar})
    '';
  };
  # eclipse package and plugins
in prev.eclipse-mat.overrideAttrs (oldAttrs: {
  buildCommand = oldAttrs.buildCommand + ''
    # add ibm dtfj plugin to support ibm jdk heapdump
    P2_DIRECTOR=org.eclipse.equinox.p2.director
    DTFJ_REPO=file:${ibm-dtfj-p2-site}
    DTFJ_FEATURE=${ibm-dtfj-p2-site.pkgprefix}.feature.feature.group
    $out/mat/MemoryAnalyzer -application $P2_DIRECTOR -repository $DTFJ_REPO -installIU $DTFJ_FEATURE
    # sed '/-vmargs/ i -vm\n${prev.jdk11}/bin' $out/mat/MemoryAnalyzer.ini > $out/mat/MemoryAnalyzer.jdk11.ini
    awk '/-vmargs/ {print "-vm\n${prev.jdk11}/bin"} 1' $out/mat/MemoryAnalyzer.ini > $out/mat/MemoryAnalyzer.jdk11.ini
    cp $out/mat/MemoryAnalyzer.jdk11.ini $out/mat/MemoryAnalyzer.ini
    rm -fr $out/mat/MemoryAnalyzer.jdk11.ini
  '';
})
