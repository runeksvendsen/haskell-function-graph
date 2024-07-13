# TODO: runtime dep "graphviz"
# TODO: make "dump-decls" build by not running the "test" test suite
{ nixpkgs ? (import ./nix/pkgs.nix).pkgs
, compiler ? "ghc92"
}:
let
  bellman-ford = import (bellman-fordSrc + "/default.nix") { inherit nixpkgs compiler; };
  bellman-fordSrc = builtins.fetchGit {
    url = "https://github.com/runeksvendsen/bellman-ford.git";
    rev = "8896ea261c29af4b69b6b8b007873c59c886618c";
  };

  dump-decls = nixpkgs.haskell.lib.setBuildTargets
    (import (dump-declsSrc + "/default.nix") { inherit nixpkgs compiler; })
    ["lib:lib" "test:doctest" "test:unit"];

  dump-declsSrc = builtins.fetchGit {
    url = "https://github.com/runeksvendsen/dump-decls.git";
    rev = "1faa909fd541d5cb16a704478433f37b4734da30";
  };

  servant-errors =
    let servant-errorsSrc = builtins.fetchGit {
          url = "https://github.com/epicallan/servant-errors.git";
          rev = "7c564dff3574c35cae721b711bd90503b851438e";
        };
    in nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "servant-errors" servant-errorsSrc {};

  args =
    { bellman-ford = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
        (nixpkgs.pkgs.haskell.lib.overrideCabal bellman-ford)
        { };

      dump-decls = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
        (nixpkgs.pkgs.haskell.lib.overrideCabal dump-decls)
        { };

      servant-errors = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage
        (nixpkgs.pkgs.haskell.lib.overrideCabal servant-errors)
        { };
    };
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "function-graph" ./. args
