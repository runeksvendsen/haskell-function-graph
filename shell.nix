let
  pkgsCabal =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/0c924ab22aa8818c701ad9d94be34dc7934d692c.tar.gz";
      sha256 = "1llhad9y24bqp7ib8ls2fwk4jpqakyrzhy7p2djkqlbsnhzcvd1q";
    }) {};
in
with (import ./nix/pkgs.nix {});
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    pkgsCabal.cabal-install # use version of cabal-install with "multiple components"-support (https://github.com/haskell/cabal/pull/8726)
    pkgs.zlib.dev
    pkgs.pkg-config
    pkgs.git
    pkgs.graphviz
    pkgs.gnused # TODO.sh depends on GNU grep
  ];

  shellHook = ''
    export PATH="$(pwd)/hls":$PATH # Run the command '$(nix-build build-hls.sh.nix)/bin/build-hls.sh' to install HLS in this directory
  '';
}
