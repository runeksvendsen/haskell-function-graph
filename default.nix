{ nixpkgsRaw ? (import ./nix/pkgs.nix).pkgs
, compiler ? "ghc90"
}:
let
  nixpkgs =
    nixpkgsRaw.pkgs.lib.recursiveUpdate # See https://nix.dev/guides/best-practices.html#updating-nested-attribute-sets
      nixpkgsRaw
      { pkgs.haskell.packages.${compiler} = # NOTE: Previously, 'haskellPackages' was overridden. But this is not correct, since 'haskellPackages' simply points to 'pkgs.haskell.packages.${compiler}' with some nixpkgs-specified default value for '${compiler}'.
          nixpkgsRaw.pkgs.haskell.packages.${compiler}.override {
            overrides = self: super: {
              statistics = nixpkgsRaw.pkgs.haskell.lib.dontCheck super.statistics;
              flatparse = nixpkgsRaw.pkgs.haskell.lib.dontCheck super.flatparse;
            };
          };
      };

  bellman-ford =
    let src = builtins.fetchGit {
      url = "https://github.com/runeksvendsen/bellman-ford.git";
      rev = "9a511144d46f07ff0e3632120b7d6eecf1491814";
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

  # TODO: run 'test-web' executable as part of build and fail build on non-zero exit code
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
