#!/usr/bin/env bash

# Run CI steps locally

set -euxo pipefail

cat .github/workflows/cabal-in-nix-shell.yml|grep -v -e '^$'|grep 'run: nix-shell'|sed 's/[ ]*run: //' | while read line; do eval "$line"; done
