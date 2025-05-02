#!/usr/bin/env bash

set -e

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  # standardize name
  arch="aarch64"
fi

# x86_64-osx on macos-12/13, aarch64-osx on macos-14/15
cabal_build_dir="$arch-osx"

mkdir -p bin

suffix="_$KAIROS_VERS-$arch-macos_$apple_vers-darwin"

export KAIROS_HOME=$(pwd); cabal install kairos --installdir bin/ --program-suffix $suffix --project-file $CABAL_PROJ --ghc-options -Werror

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix

echo "*** Printing version ***"
./bin/kairos$suffix --version

echo "*** Computing sha256 ***"
sha256sum ./bin/kairos$suffix > ./bin/kairos$suffix.sha256
cat ./bin/kairos$suffix.sha256
