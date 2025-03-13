<div align="center">

# time-conv

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/time-conv?include_prereleases&sort=semver)](https://github.com/tbidne/time-conv/releases/)
[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/hs-template/ci.yaml?branch=main)](https://github.com/tbidne/hs-template/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/time-conv?color=blue)](https://opensource.org/licenses/MIT)

![linux](https://img.shields.io/static/v1?label=&message=linux&logo=linux&logoColor=white&labelColor=2f353e&color=blue)
![osx](https://img.shields.io/static/v1?label=&message=osx&logo=apple&labelColor=2f353e&color=blue)
![windows](https://img.shields.io/static/v1?label=&message=windows&logo=windows&labelColor=2f353e&color=blue)

</div>

---

### Table of Contents

* [Introduction](#introduction)
  * [Usage](#usage)
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
  * [Nix](#nix)

# Introduction

`time-conv` is a tool for converting between timezones. There are two primary use-cases.

1. Converting local system time into a different timezone:

    ```
    $ time-conv -d europe/paris
    Sat, 18 Jun 2022 03:19:58 CEST

    # -d sets the "destination" timezone
    # no "time string" means we read the local system time
    ```

2. Converting a "time string" from one timezone to another:

    ````
    $ time-conv --date today -s america/new_york 18:30
    Sat, 18 Jun 2022 11:30:00 NZST

    # --date today means "today's date" as determined by the source
    # -s sets the "source" timezone
    # no dest means we convert to local time
    # i.e. 6:30 pm in New York on its current day (17 Jun 2022) will be 11:30 am NZST (18 Jun 2022)
    ````

    We can also convert between two non-local timezones:

    ```
    $ time-conv -s america/new_york -d europe/paris 18:30
    Fri,  2 Jan 1970 00:30:00 CET

    # no --date information means we assume the initial unix date, 1 Jan 1970.
    ```

The timezone names are based on the tz_database. See https://en.wikipedia.org/wiki/Tz_database for more information.

## Usage

```
time-conv: A tool for timezone conversions.

Usage: time-conv [-c|--config PATH] [--date (today | YYYY-mm-dd)]
                 [-d|--dest-tz TZ] [-f|--format-in FMT_STR]
                 [-o|--format-out (rfc822 | FMT_STR)] [-s|--src-tz TZ]
                 [TIME_STR] [-v|--version]

  time-conv reads time strings and converts between timezones. For the src and
  dest options, TZ refers to labels like 'America/New_York' or offsets like
  '+1300'. See https://en.wikipedia.org/wiki/Tz_database.

Available options:
  -c,--config PATH         Path to TOML config file. It not given we
                           automatically look in the XDG config e.g.
                           ~/.config/time-conv/config.toml.

  --no-config              Disables --config.

  --date (today | YYYY-mm-dd)
                           Date in which to read the string. Today uses the
                           current date, as determined by the source. This
                           option requires TIME_STR.

  --no-date                Disables --date. Useful for disabling the toml field
                           'today'.

  -d,--dest-tz TZ          Timezone in which to convert the read string. Must be
                           a tz database label or offset e.g.
                           'America/New_York', '+1300'. If none is given then we
                           use the local system timezone.

  -f,--format-in FMT_STR   Glibc-style format string for parsing the time
                           string. Should not contain a timezone flag like %Z
                           (see --src-tz) nor a date (see --date). Defaults to
                           %H:%M i.e. 24-hr hour:minute. See 'man date' for
                           basic examples.

  -o,--format-out (rfc822 | FMT_STR)
                           Like --format-in, but used for the output. If this is
                           not present we default to rfc822 i.e. RFC822.

  -s,--src-tz TZ           Timezone in which to read the string. Must be a tz
                           database label or offset e.g. 'America/New_York',
                           '+1300'. If none is given then we use the local
                           system timezone. This option requires TIME_STR.

  TIME_STR                 Time string to parse. If none is given then we parse
                           the local system time. To format the output, use
                           --format-out.

  -h,--help                Show this help text

Version: 0.1
```

# Options

## Config

**Arg:** `-c,--config PATH `

**Description:** Path to `toml` config file. Can be used to define aliases for tz_database labels. See [examples](./examples/) directory for examples.

**Examples:**

```
$ time-conv -c ./examples/config.toml -d la
Thu, 20 Apr 2023 22:25:37 PDT
```

## Date

**Arg:** `--date (today | YYYY-mm-dd)`

**Description:** Date in which to read the string. Today uses the current date, as determined by the source. This option requires [Time String](#time-string).

**Examples:**

```
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# use today's date instead of initial unix time
$ time-conv --date today 08:30
Thu, 20 Apr 2023 08:30:00 NZST

$ time-conv --date today -s america/new_york 08:30
Thu, 20 Apr 2023 00:30:00 NZST

$ time-conv --date 2022-04-10 -s america/new_york 08:30
Mon, 11 Apr 2022 00:30:00 NZST
```

## Destination Timezone

**Arg:** `-d,--dest-tz TZ`

**Description:** This option allows one to convert the read timezone. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ time-conv -d america/new_york 08:30
Wed, 31 Dec 1969 15:30:00 EST

$ time-conv -s america/new_york -d etc/utc 08:30
Thu,  1 Jan 1970 13:30:00 UTC
```

## Format In

**Arg:** `-f,--format-in FMT_STR`

**Description:** Glibc-style format string for parsing the time string. Should not contain a timezone flag like `%Z` (see [`--src-tz`](#source_timezone)) nor a date (see [`--date`](#date)). Defaults to `%H:%M` i.e. 24-hr hour:minute. See 'man date' for basic examples.

**Examples:**

```
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZDT

$ time-conv -f "%I:%M %p" "08:00 pm"
Thu,  1 Jan 1970 20:00:00 NZST
```

## Format Out

**Arg:** `-o,--format-out (rfc822 | FMT_STR)`

**Description:** Like `--format-in` except it applies to the output format only. If `--format-out` is not given we default to `rfc822`.

**Examples:**

```
# using implicit rc822 format for output
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# overriding output format
$ time-conv -o %H:%M:%S 08:30
08:30:00
```

## Source Timezone

**Arg:** `-s,--src-tz TZ`

**Description:** Timezone in which to read the string. Must be a tz database label or offset e.g. `America/New_York`, `+1300`. If none is given then we use the local system timezone. This option requires [Time String](#time-string).

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ time-conv -s america/new_york 08:30
Fri,  2 Jan 1970 01:30:00 NZST
```

## Destination Timezone

**Arg:** `-d,--dest-tz TZ_DB`

**Description:** This option allows one to convert the read timezone. Must be a tz database label like America/New_York. If none is given then we use the local system timezone.

**Examples:**

```
# use the local system timezone
$ time-conv 08:30
Thu,  1 Jan 1970 08:30:00 NZST

# using tz database name
$ time-conv -d america/new_york 08:30
Wed, 31 Dec 1969 15:30:00 EST

$ time-conv -s america/new_york -d etc/utc 08:30
Thu,  1 Jan 1970 13:30:00 UTC
```

## Time String

**Arg:** `TIME_STR`

**Description:** This is the time string to parse. If none is given then we parse the local system time. To format the output, use [`--format-out`](format-out).

**Examples:**

```
$ time-conv
Thu, 16 Jun 2022 21:30:00 NZST

$ time-conv -d europe/paris
Thu, 16 Jun 2022 12:30:00 CEST
```

# Building

If you have never built a haskell program before, [Cabal](#cabal) is probably the best choice.

## Cabal

### Prerequisites

* [`ghcup`](https://www.haskell.org/ghcup/)

Using `ghcup`, install `cabal 2.4+` and one of:

- `ghc 9.2`
- `ghc 9.4`
- `ghc 9.6`
- `ghc 9.8`
- `ghc 9.10`
- `ghc 9.12`

### Build Time-Conv

Once you have `cabal` and `ghc`, `time-conv` can be built with `cabal build` or installed globally (i.e. `~/.cabal/bin/`) with `cabal install`.

## Nix

### Prerequisites

* [nix](https://nixos.org/download.html)

### Manually

Building with `nix` uses [flakes](https://nixos.wiki/wiki/Flakes). `time-conv` can be built with `nix build`, which will compile and run the tests.

### Nix expression

Because `time-conv` is a flake, it be built as part of a nix expression. For instance, if you want to add `time-conv` to `NixOS`, your `flake.nix` should have:

```nix
# flake.nix
{
  inputs.time-conv.url = "github:tbidne/time-conv/main";
}
```

Then include this in the `systemPackages`:

```nix
# wherever your global packages are defined
{
  environment.systemPackages = [
    time-conv.packages."${system}".default
  ];
}
```
