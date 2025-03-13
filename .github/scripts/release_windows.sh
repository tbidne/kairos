#!/usr/bin/env bash

set -e

kairos_vers="0.1"

windows_vers=$1

arch="x86_64"

mkdir -p bin

suffix="_$kairos_vers-$arch-windows-$windows_vers-mingw64"

cabal install exe:kairos --installdir bin/ --program-suffix $suffix --project-file cabal.ghc9101.project --ghc-options -Werror

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix
