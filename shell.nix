let
  pkgs =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-23.05.tar.gz";
      sha256 = "0n00rsh3lagsbqypsnjram29242kvlv9j0zx305zlm3nc7n1jxbx";
    }) {};
  release-21-05 =
    import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-21.05.tar.gz";
      sha256 = "12q00nbd7fb812zchbcnmdg3pw45qhxm74hgpjmshc2dfmgkjh4n";
    }) {};
in
  pkgs.mkShell {
    nativeBuildInputs = [
      pkgs.haskell.compiler.ghc810
      # pkgs.haskell.compiler.ghc865Binary
      release-21-05.cabal-install
      pkgs.git
    ];
}
