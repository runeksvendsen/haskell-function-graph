with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc810
    release-21-05.cabal-install
    pkgs.zlib
    pkgs.git
  ];
}
