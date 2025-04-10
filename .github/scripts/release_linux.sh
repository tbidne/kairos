#!/usr/bin/env bash

set -e

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="_$KAIROS_VERS-$arch-linux"

docker build \
  -t kairos_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg CABAL_VERS=$CABAL_VERS \
  --build-arg CABAL_PROJ=$CABAL_PROJ \
  --build-arg GHC_VERS=$GHC_VERS \
  --build-arg suffix=$suffix \
  .

cp docker_out/kairos_* bin/

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix

echo "*** Computing sha256 ***"
sha256sum ./bin/kairos$suffix > ./bin/kairos$suffix.sha256
cat ./bin/kairos$suffix.sha256
