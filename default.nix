# TODO: runtime dep "graphviz"
# TODO: make "dump-decls" build by not running the "test" test suite
{ nixpkgs ? (import ./nix/pkgs.nix).pkgs
, compiler ? "ghc92"
}:
let
  bellman-ford =
    let src = builtins.fetchGit {
      url = "https://github.com/runeksvendsen/bellman-ford.git";
      rev = "8896ea261c29af4b69b6b8b007873c59c886618c";
    };
    in import src { inherit nixpkgs compiler; };


  dump-decls =
    let src = builtins.fetchGit {
      url = "https://github.com/runeksvendsen/dump-decls.git";
      rev = "e57c43f059e9a4affbeb230dffc8bc5fa733b670";
    };
    in nixpkgs.haskell.lib.setBuildTargets
      (import src { inherit nixpkgs compiler; })
      ["lib:dump-decls" "test:doctest" "test:unit"];

  servant-errors =
    let src = builtins.fetchGit {
      url = "https://github.com/epicallan/servant-errors.git";
      rev = "7c564dff3574c35cae721b711bd90503b851438e";
    };
    in nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "servant-errors" src {};

  args =
    { bellman-ford = bellman-ford;
      dump-decls = dump-decls;
      servant-errors = servant-errors;
    };
in
  nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "function-graph" ./. args
