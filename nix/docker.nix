{ name ? "haskell-function-graph"
, pkgs ? import ./pkgs.nix {
    system = "x86_64-linux"; # putting e.g. macOS executables inside a Docker image doesn't work that well apparently
  }
, serverPort ? 8080
}:
with pkgs.pkgs;
let exe = import ../default.nix { nixpkgsRaw = pkgs.pkgs; };
    graphFileName = "all3.json";
    graphFileData = stdenv.mkDerivation {
      name = "haskell-function-graph-graph-data-file";
      src = ../data;
      installPhase = ''
        mkdir $out
        cp $src/${graphFileName} $out/
      '';
    };
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
