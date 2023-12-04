with (import ./nix/pkgs.nix);
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc90
    release-21-05.cabal-install
    pkgs.zlib
    pkgs.git
  ];

  shellHook = ''
    export PATH=./hls:$PATH
  '';
}
