#!/usr/bin/env bash

set -e

kairos_vers="0.1"

arch=$(uname -m)

dir=$1

mkdir -p bin

suffix="_$kairos_vers-$arch-linux-static"

docker build \
  -t kairos_build:latest \
  -f "docker/$dir/Dockerfile" \
  -o docker_out \
  --build-arg suffix=$suffix \
  .

cp docker_out/kairos_* bin/

echo "*** Testing exe ***"
./bin/kairos$suffix --help

./bin/kairos$suffix

echo "*** Computing sha256 ***"
sha256sum ./bin/kairos$suffix > ./bin/kairos$suffix.sha256
