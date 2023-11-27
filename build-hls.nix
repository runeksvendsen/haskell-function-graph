{ pkgs ? (import ./nix/pkgs.nix).pkgs
, hls-version ? "2.4.0.0"
, exe-dir ? "exe"
}:
let nix = pkgs.nixVersions.nix_2_14;
    coreutils = pkgs.coreutils;
    realpath = "${coreutils}/bin/realpath";
in
pkgs.writeScriptBin "build-hls" ''
    #! /usr/bin/env nix-shell
    #! nix-shell -i bash -p bash

    cd "$(mktemp -d)"
    git clone https://github.com/haskell/haskell-language-server.git
    cd haskell-language-server
    git checkout ${hls-version}
    cabal build exe:haskell-language-server
    mkdir bin
    cabal install --installdir ${exe-dir} exe:haskell-language-server
    echo "Built haskell-language-server v${hls-version}. Executables are in $(realpath ${exe-dir})"
''
