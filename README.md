<div align="center">

# kairos

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/kairos?include_prereleases&sort=semver)](https://github.com/tbidne/kairos/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/kairos/ci.yaml?branch=main)](https://github.com/tbidne/kairos/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/kairos?color=blue)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![osx](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)
![windows](https://img.shields.io/static/v1?label=&message=windows&logo=windows&labelColor=2f353e&color=blue)

</div>

---

### Table of Contents

* [Introduction](#introduction)
* [Options](#options)
  * [Config options](#config-options)
    * [Config](#config)
  * [Formatting options](#formatting-options)
    * [Color](#color)
    * [Format In](#format-in)
    * [Format Out](#format-out)
  * [Misc options](#misc-options)
    * [Print aliases](#print-aliases)
  * [Time options](#time-options)
    * [Date](#date)
    * [Time String](#time-string)
  * [Timezone options](#timezone-options)
    * [Destination Timezone](#destination-timezone)
    * [Source Timezone](#source-timezone)
* [Installation](#installation)
* [Building](#building)
  * [Cabal](#cabal)
  * [Stack](#stack)
  * [Nix](#nix)

# Introduction

`kairos` is a tool for converting between timezones. There are two primary use-cases.

1. Converting local system time into a different timezone:

    ```
    $ kairos -d europe/paris
    Sat, 18 Jun 2022 03:19:58 CEST

    # -d sets the "destination" timezone
    # no "time string" means we read the local system time
    ```

2. Converting a "time string" from one timezone to another:

    ````
    $ kairos --date 2022-06-17 -s america/new_york 18:30
    Sat, 18 Jun 2022 10:30:00 NZST

    # -s sets the "source" timezone
    # no dest means we convert to local time
    # i.e. 6:30 pm in New York on 2022-06-17 will be 10:30 am NZST (18 Jun 2022)
    ````

    We can also convert between two non-local timezones:

    ```
    $ kairos -s america/new_york -d europe/paris 18:30
    Mon, 17 Mar 2025 23:30:00 CET

    # no --date information means we use the current date, as determined by
    # the source i.e. 18:30 in nyc on the current date (2025-03-17).
    ```

The timezone names are based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

# Options

## Config Options

### Config

**Arg:** `-c,--config PATH`

**Description:** Path to `toml` config file. Can be used to define aliases for tz_database labels. See the [examples](./lib/exe/examples/) directory for examples.

**Examples:**

```
$ kairos -c ./lib/exe/examples/config.toml -d la
Thu, 20 Apr 2023 22:25:37 PDT
```

## Formatting Options

### Color

**Arg:** `--color (true | false)`

**Description:** Determines if we color the [`--print-aliases`](#print-aliases) output. Defaults to `true`.

**Examples:**

```
$ kairos -c ./lib/exe/examples/config.toml --print-aliases --color true
 - la:          america/los_angeles
 - ny:          america/new_york
 - paris:       europe/paris
 - some_offset: +0700
 - uk:          europe/london
 - zagreb:      europe/zagreb
```

### Format In

**Arg:** `-f,--format-in FMT_STR`

**Description:** Glibc-style format string for parsing the time string. Should not contain a timezone flag like `%Z` (see [`--src-tz`](#source_timezone)) nor a date (see [`--date`](#date)). Defaults to standard 12 and 24 hour formats e.g. `17:00`, `0300`, `4:30 pm`, `2 am`. See 'man date' for basic examples.

**Examples:**

```
# default formats
$ kairos 08:30
Tue, 18 Mar 2025 08:30:00 NZDT

$ kairos 0830
Tue, 18 Mar 2025 08:30:00 NZDT

$ kairos '8:30 am'
Tue, 18 Mar 2025 08:30:00 NZDT

$ kairos 8am
Tue, 18 Mar 2025 08:30:00 NZDT

# custom format
$ kairos -f "%I:%M %p" "08:00 pm"
Tue, 18 Mar 2025 20:00:00 NZDT
```

### Format Out

**Arg:** `-o,--format-out (rfc822 | FMT_STR)`

**Description:** Like `--format-in` except it applies to the output format only. If `--format-out` is not given we default to `rfc822`.

**Examples:**

```
# using implicit rfc822 format for output
$ kairos 08:30
Tue, 18 Mar 2025 08:30:00 NZDT

# overriding output format
$ kairos -o %H:%M:%S 08:30
08:30:00
```

## Misc Options

### Print Aliases

**Arg:** `--print-aliases`

**Description:** Prints aliases from toml config.

**Examples:**

```
$ kairos -c ./lib/exe/examples/config.toml --print-aliases
 - la:          america/los_angeles
 - ny:          america/new_york
 - paris:       europe/paris
 - some_offset: +0700
 - uk:          europe/london
 - zagreb:      europe/zagreb
```

## Time Options

### Date

**Arg:** `--date YYYY-mm-dd`

**Description:** Date in which to read the string. This option requires [Time String](#time-string). No date uses the current date, as determined by the source.

**Examples:**

```
$ kairos 08:30
Tue, 18 Mar 2025 08:30:00 NZDT

$ kairos --date 2022-04-10 -s america/new_york 08:30
Mon, 11 Apr 2022 00:30:00 NZST
```

### Time String

**Arg:** `TIME_STR`

**Description:** This is the time string to parse. If none is given then we parse the local system time. To format the output, use [`--format-out`](format-out).

**Examples:**

```
$ kairos
Tue, 18 Mar 2025 15:33:30 NZDT

$ kairos -s america/new_york 08:30
Tue, 18 Mar 2025 01:30:00 NZDT
```

## Timezone Options

### Destination Timezone

**Arg:** `-d,--dest-tz TZ`

**Description:** This option allows one to convert the read timezone. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ kairos 08:30
Tue, 18 Mar 2025 08:30:00 NZDT

# using tz database name
$ kairos -d america/new_york 08:30
Mon, 17 Mar 2025 15:30:00 EDT

$ kairos -s america/new_york -d utc 08:30
Mon, 17 Mar 2025 12:30:00 UTC

$ kairos -d +0200 08:30
Mon, 17 Mar 2025 21:30:00 +0200
```

### Source Timezone

**Arg:** `-s,--src-tz TZ`

**Description:** Timezone in which to read the string. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone. This option requires [Time String](#time-string).

**Examples:**

```
# use the local system timezone
$ kairos 08:30
Tue, 18 Mar 2025 08:30:00 NZDT

# using tz database name
$ kairos -s america/new_york 08:30
Tue, 18 Mar 2025 01:30:00 NZDT

# use tz offset
$ kairos -s -13 08:30
Tue, 18 Mar 2025 10:30:00 NZDT
```

# Installation

The [releases](https://github.com/tbidne/kairos/releases) page has binaries built for several platforms. If there are no binaries for your platform, it is possible to [build kairos](#building) yourself.

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal-install 2.4+`](https://www.haskell.org/cabal/download.html)
* [`ghc 9.2 - 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

The current "blessed" version is `ghc-9.10.1`.

### Build Kairos

Once you have `cabal` and `ghc`, `kairos` can be built with `cabal build kairos` or installed globally (i.e. `~/.local/bin/kairos`) with `cabal install kairos`.

For further reproducibility, an optional freeze file can be used for the "blessed" compiler.

```sh
cabal build --project-file cabal.ghc<XYZ>.project
```

## Stack

### Prerequisites

* [`stack`](https://docs.haskellstack.org/en/stable/)

Like `cabal` and `ghc`, `stack` can be installed with [`ghcup`](https://www.haskell.org/ghcup/).

### Build Kairos

Once you have `stack`, `kairos` can be built with `stack build kairos` or installed globally (i.e. `~/.local/bin/kairos`) with `stack install kairos`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `kairos` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `kairos` is a flake, it can be built as part of a nix expression. For instance, if you want to add `kairos` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.kairos.url = "github:tbidne/kairos/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    kairos.packages."${system}".default
  ];
}
```
