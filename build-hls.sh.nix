{ pkgs ? (import ./nix/pkgs.nix).pkgs
, hls-version ? "2.4.0.0"
, exe-dir-expression ? "$(pwd)/hls"
}:
let nix = pkgs.nixVersions.nix_2_14;
    coreutils = pkgs.coreutils;
    realpath = "${coreutils}/bin/realpath";
in
pkgs.writeScriptBin "build-hls.sh" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i bash -p bash -p git -p cabal-install -p haskell.compiler.ghc90 -p zlib

    set -e

    EXE_DIR="${exe-dir-expression}"

    cd "$(mktemp -d)"
    git clone https://github.com/haskell/haskell-language-server.git
    cd haskell-language-server
    git checkout ${hls-version}
    cabal build exe:haskell-language-server
    cabal install --installdir "$EXE_DIR" exe:haskell-language-server
    echo "Built haskell-language-server v${hls-version}"
    echo "Executables installed in $(realpath $EXE_DIR)"
''