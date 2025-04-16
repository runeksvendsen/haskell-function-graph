{ name ? "haskell-function-graph"
, pkgs ? import ./pkgs.nix
}:
with pkgs.pkgs;
let exe = import ../default.nix {};
    graphFileName = "all3.json";
    graphFileData = stdenv.mkDerivation {
      name = "haskell-function-graph-graph-data-file";
      src = ../data;
      installPhase = ''
        mkdir $out
        cp $src/${graphFileName} $out/
      '';
    };
    serverPort = 8080;
in dockerTools.buildImage {
  name = name;
  copyToRoot = buildEnv {
    name = "image-root";
    paths = [
      exe
      graphFileData
    ];
  };
  config = {
    Cmd = [
      "${exe}/bin/server-wrapped"
      "-p" "${toString serverPort}"
      "-g" "${graphFileData}/${graphFileName}"
    ];
    ExposedPorts = {
        "${toString serverPort}/tcp" = {};
    };
  };
}
