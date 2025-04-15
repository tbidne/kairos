{
  description = "CLI app for converting between timezones";

  # nix
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.exception-utils = {
    url = "github:tbidne/exception-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.fs-utils = {
    url = "github:tbidne/fs-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.gitrev = {
    url = "github:tbidne/gitrev/dev";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.monad-effects = {
    url = "github:tbidne/monad-effects";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
    inputs.exception-utils.follows = "exception-utils";
    inputs.fs-utils.follows = "fs-utils";
    inputs.smart-math.follows = "smart-math";
  };
  inputs.smart-math = {
    url = "github:tbidne/smart-math";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
    inputs.nixpkgs.follows = "nixpkgs";

    inputs.algebra-simple.follows = "algebra-simple";
    inputs.bounds.follows = "bounds";
  };
  outputs =
    inputs@{
      flake-parts,
      monad-effects,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                path = hlib.dontCheck prev.path_0_9_6;
                stan = hlib.doJailbreak prev.stan;
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
                "gitrev"
              ]
              // nix-hs-utils.mkRelLibs "${monad-effects}/lib" final [
                "effects-env"
                "effects-fs"
                "effects-ioref"
                "effects-optparse"
                "effects-stm"
                "effects-terminal"
                "effects-time"
              ];
          };
          hlib = pkgs.haskell.lib;
          mkPkg =
            name: root: source-overrides: returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit
                compiler
                name
                pkgs
                returnShellEnv
                root
                source-overrides
                ;

              # TODO: Once hlint is back to working with our GHC we can
              # use nix-hs-utils.mkDevTools ++ otherDeps.
              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];

              modifier =
                drv:
                drv.overrideAttrs (oldAttrs: {
                  KAIROS_HASH = "${self.shortRev or self.dirtyShortRev}";
                });
            };
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack --add-flags "--no-nix --system-ghc"
            '';
          };
          compilerPkgs = {
            inherit compiler pkgs;
          };

          mkKairosCore = mkPkg "kairos-core" ./lib/core { };
          mkKairosExe = mkPkg "kairos" ./lib/exe {
            kairos-core = ./lib/core;
          } false;
        in
        {
          packages = {
            core = mkKairosCore false;
            default = mkKairosExe;
            exe = mkKairosExe;
          };
          devShells = {
            default = mkKairosCore true;

            stack = pkgs.mkShell {
              buildInputs = [
                compiler.ghc
                pkgs.zlib
                stack-wrapped
              ];
            };
          };

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            #lint = nix-hs-utils.lint compilerPkgs;
            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
