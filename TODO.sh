#!/usr/bin/env bash

##### Print TODOs

set -euo pipefail

## Get all source directories from the .cabal file.
##
## NOTE: Works only if the source directory is on the same line as the "hs-source-dirs"-field
SRC_DIRS=$(sed -rn 's/[ ]*hs-source-dirs:[ ]*(.*)/\1/p' function-graph.cabal)
# shellcheck disable=SC2086 # shellcheck is wrong: we want each source directory as a separate argument
grep --color -R --include \*.hs "TODO:" ${SRC_DIRS}
