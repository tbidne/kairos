#!/usr/bin/env bash

set -e

windows_vers=$1

arch="x86_64"

mkdir -p bin

suffix="_$KAIROS_VERS-$arch-windows_$windows_vers-mingw64"

cabal install kairos --installdir bin/ --program-suffix $suffix --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix

echo "*** Computing sha256 ***"
sha256sum ./bin/kairos$suffix > ./bin/kairos$suffix.sha256
cat ./bin/kairos$suffix.sha256
