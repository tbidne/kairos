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
  * [Config](#config)
  * [Date](#date)
  * [Destination Timezone](#destination-timezone)
  * [Format In](#format-in)
  * [Format Out](#format-out)
  * [Source Timezone](#source-timezone)
  * [Time String](#time-string)
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
    $ kairos --date today -s america/new_york 18:30
    Sat, 18 Jun 2022 11:30:00 NZST

    # --date today means "today's date" as determined by the source
    # -s sets the "source" timezone
    # no dest means we convert to local time
    # i.e. 6:30 pm in New York on its current day (17 Jun 2022) will be 11:30 am NZST (18 Jun 2022)
    ````

    We can also convert between two non-local timezones:

    ```
    $ kairos -s america/new_york -d europe/paris 18:30
    Fri,  2 Jan 1970 00:30:00 CET

    # no --date information means we assume the initial unix date, 1 Jan 1970.
    ```

The timezone names are based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

# Options

## Config

**Arg:** `-c,--config PATH `

**Description:** Path to `toml` config file. Can be used to define aliases for tz_database labels. See the [examples](./lib/exe/examples/) directory for examples.

**Examples:**

```
$ kairos -c ./lib/exe/examples/config.toml -d la
Thu, 20 Apr 2023 22:25:37 PDT
```

## Date

**Arg:** `--date (today | YYYY-mm-dd)`

**Description:** Date in which to read the string. Today uses the current date, as determined by the source. This option requires [Time String](#time-string).

**Examples:**

```
$ kairos 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# use today's date instead of initial unix time
$ kairos --date today 08:30
Thu, 20 Apr 2023 08:30:00 NZST

$ kairos --date today -s america/new_york 08:30
Thu, 20 Apr 2023 00:30:00 NZST

$ kairos --date 2022-04-10 -s america/new_york 08:30
Mon, 11 Apr 2022 00:30:00 NZST
```

## Destination Timezone

**Arg:** `-d,--dest-tz TZ`

**Description:** This option allows one to convert the read timezone. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ kairos 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ kairos -d america/new_york 08:30
Wed, 31 Dec 1969 15:30:00 EST

$ kairos -s america/new_york -d etc/utc 08:30
Thu,  1 Jan 1970 13:30:00 UTC

$ kairos -d +0200 08:30
Wed, 31 Dec 1969 21:30:00 +0200
```

## Format In

**Arg:** `-f,--format-in FMT_STR`

**Description:** Glibc-style format string for parsing the time string. Should not contain a timezone flag like `%Z` (see [`--src-tz`](#source_timezone)) nor a date (see [`--date`](#date)). Defaults to standard 12 and 24 hour formats e.g. `17:00`, `0300`, `4:30 pm`, `2 am`. See 'man date' for basic examples.

**Examples:**

```
# default formats
$ kairos 08:30
Thu,  1 Jan 1970 08:30:00 NZDT

$ kairos 0830
Thu,  1 Jan 1970 08:30:00 NZST

$ kairos '8:30 am'
Thu,  1 Jan 1970 08:30:00 NZST

$ kairos 8am
Thu,  1 Jan 1970 08:00:00 NZST

# custom format
$ kairos -f "%I:%M %p" "08:00 pm"
Thu,  1 Jan 1970 20:00:00 NZST
```

## Format Out

**Arg:** `-o,--format-out (rfc822 | FMT_STR)`

**Description:** Like `--format-in` except it applies to the output format only. If `--format-out` is not given we default to `rfc822`.

**Examples:**

```
# using implicit rfc822 format for output
$ kairos 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# overriding output format
$ kairos -o %H:%M:%S 08:30
08:30:00
```

## Source Timezone

**Arg:** `-s,--src-tz TZ`

**Description:** Timezone in which to read the string. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone. This option requires [Time String](#time-string).

**Examples:**

```
# use the local system timezone
$ kairos 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ kairos -s america/new_york 08:30
Fri,  2 Jan 1970 01:30:00 NZST

# use tz offset
$ kairos -s -13 08:30
Fri,  2 Jan 1970 10:30:00 NZDT
```

## Time String

**Arg:** `TIME_STR`

**Description:** This is the time string to parse. If none is given then we parse the local system time. To format the output, use [`--format-out`](format-out).

**Examples:**

```
$ kairos
Thu, 16 Jun 2022 21:30:00 NZST

$ kairos -d europe/paris
Thu, 16 Jun 2022 12:30:00 CEST
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`cabal 2.4+`](https://www.haskell.org/cabal/download.html)
* One of:
  * [`ghc 9.2`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)
  * [`ghc 9.4`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)
  * [`ghc 9.6`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)
  * [`ghc 9.8`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)
  * [`ghc 9.10`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)
  * [`ghc 9.12`](https://gitlab.haskell.org/ghc/ghc/-/wikis/GHC%20Status)

The easiest way to install these is generally [`ghcup`](https://www.haskell.org/ghcup/).

### Build Kairos

Once you have `cabal` and `ghc`, `kairos` can be built with `cabal build kairos` or installed globally (i.e. `~/.local/bin/kairos`) with `cabal install kairos`.

For further reproducibility, optional freeze files can be used e.g.

```sh
cabal build kairos --project-file cabal.ghc9101.project
```

> [!NOTE]
>
> Freeze files are provided for only select compilers.

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

Because `kairos` is a flake, it be built as part of a nix expression. For instance, if you want to add `kairos` to `NixOS`, your `flake.nix` should have:

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
