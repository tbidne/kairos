# core

.PHONY: build
build:
	cabal build all

.PHONY: test
test:
	RUN_DOCTEST=1 cabal test

.PHONY: doctest
doctest:
	RUN_DOCTEST=1 cabal run doctest

.PHONY: repl
repl:
	cabal repl

.PHONY: watch
watch:
	ghcid --command "cabal repl"

# ci

.PHONY: ci_ck
ci_ck: format_ck lint_ck haddock_ck

.PHONY: ci
ci: format lint

# formatting

.PHONY: format_ck
format_ck: cabalfmt_ck hsformat_ck nixpkgsfmt_ck

.PHONY: format
format: cabalfmt hsformat nixpkgsfmt

.PHONY: hsformat
hsformat:
	nix run github:tbidne/nix-hs-tools/0.5#ormolu -- --mode inplace

.PHONY: hsformat_ck
hsformat_ck:
	nix run github:tbidne/nix-hs-tools/0.5#ormolu -- --mode check

.PHONY: cabalfmt
cabalfmt:
	nix run github:tbidne/nix-hs-tools/0.5#cabal-fmt -- --inplace

.PHONY: cabalfmt_ck
cabalfmt_ck:
	nix run github:tbidne/nix-hs-tools/0.5#cabal-fmt -- --check

.PHONY: nixpkgsfmt
nixpkgsfmt:
	nix run github:tbidne/nix-hs-tools/0.5#nixpkgs-fmt

.PHONY: nixpkgsfmt_ck
nixpkgsfmt_ck:
	nix run github:tbidne/nix-hs-tools/0.5#nixpkgs-fmt -- --check

# linting

.PHONY: lint
lint:
	nix run github:tbidne/nix-hs-tools/0.5#hlint -- --refact

.PHONY: lint_ck
lint_ck:
	nix run github:tbidne/nix-hs-tools/0.5#hlint

.PHONY: haddock
haddock:
	cabal haddock --haddock-hyperlink-source

.PHONY: haddock_ck
haddock_ck:
	nix run github:tbidne/nix-hs-tools/0.5#haddock -- .