#!/usr/bin/env bash

set -e

kairos_vers="0.1"

# strip tab and/or spaces from output
apple_vers=$(sw_vers | grep ProductVersion | cut -d':' -f2 | tr -d ' \t')

# x86_64 on macos-12/13, arm64 on macos-14
arch=$(uname -m)

# x86_64-osx on macos-12/13, aarch64-osx on macos-14
if [[ $arch == 'arm64' ]]; then
  cabal_build_dir="aarch64-osx"
else
  cabal_build_dir="$arch-osx"
fi

mkdir -p bin

suffix="_$kairos_vers-$arch-macos_$apple_vers-darwin"

cabal install kairos --installdir bin/ --program-suffix $suffix --project-file cabal.ghc9101.project --ghc-options -Werror

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix
