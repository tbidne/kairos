#!/usr/bin/env bash

set -e

kairos_vers="0.1"

. /etc/lsb-release
ubuntu_vers=$(echo $DISTRIB_RELEASE)

arch=$(uname -m)

mkdir -p bin

suffix="_$kairos_vers-$arch-linux-ubuntu_$ubuntu_vers"

cabal install kairos --installdir bin/ --program-suffix $suffix --project-file cabal.ghc9101.project --ghc-options -Werror


echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix
