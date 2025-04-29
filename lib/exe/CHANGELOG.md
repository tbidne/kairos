# Revision history for kairos

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

Note that PVP is applied to the _application_, **not** the library. That is,
the major/minor/patch definitions apply to the application's interface / usage
(e.g. cli args, config file), not the haskell exe or `runner` library.

For those unfamiliar with PVP, it is essentially
[SemVer](https://semver.org/spec/v2.0.0.html), except the PVP major version is
the first _two_ components (e.g. `0.9` in `0.9.1`), and PVP has no exception
for versions < 1.

## [Unreleased]
### Added
* Added git revision info in `--help` and `--version`.
* Group `--help` options.

## [0.1.1] -- 2025-04-10
### Added
* Add `--print-aliases` option for printing toml aliases.
* Add `--color` to control `--print-aliases` coloring.

## [0.1] -- 2025-03-22

* First version. Released on an unsuspecting world.

[Unreleased]: https://github.com/tbidne/kairos/compare/0.1.1...main
[0.1.1]: https://github.com/tbidne/kairos/compare/0.1...0.1.1
[0.1]: https://github.com/tbidne/kairos/releases/tag/0.1
