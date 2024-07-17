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


  dump-decls-lib =
    let src = builtins.fetchGit {
      url = "https://github.com/runeksvendsen/dump-decls.git";
      rev = "496fc63c1279aedcdf7143c5ea85970e63a2ba0a";
    };
    in import (src + "/dump-decls-lib") { inherit nixpkgs compiler; };

  servant-errors =
    let src = builtins.fetchGit {
      url = "https://github.com/epicallan/servant-errors.git";
      rev = "7c564dff3574c35cae721b711bd90503b851438e";
    };
    in nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "servant-errors" src {};

  args =
    { bellman-ford = bellman-ford;
      dump-decls-lib = dump-decls-lib;
      servant-errors = servant-errors;
    };

  function-graph = nixpkgs.pkgs.haskell.lib.doBenchmark (
    nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "function-graph" ./. args
  );

  function-graph-server-wrapped = nixpkgs.runCommand "server-wrapped" {
      buildInputs = [ nixpkgs.makeWrapper ];
    }
    ''
        mkdir -p $out/bin
        makeWrapper ${function-graph}/bin/server $out/bin/server-wrapped \
          --set PATH ${nixpkgs.lib.makeBinPath [ nixpkgs.graphviz ]}
    '';
in
  function-graph-server-wrapped
