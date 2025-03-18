set -e

export LANG="C.UTF-8"

cabal haddock kairos-core --haddock-hyperlink-source --haddock-quickjump

mkdir -p docs/

# shellcheck disable=SC2038
find docs/ -type f | xargs -I % sh -c "rm -r %"

cp -r dist-newstyle/build/x86_64-linux/ghc-*/kairos-core-*/opt/doc/html/kairos-core/* docs/
