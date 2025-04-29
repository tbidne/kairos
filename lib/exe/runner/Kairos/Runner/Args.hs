{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI args for Kairos.
--
-- @since 0.1
module Kairos.Runner.Args
  ( Args (..),
    parserInfo,
  )
where

import Data.Functor ((<&>))
import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (showVersion)
import Effects.Optparse (OsPath, osPath)
import FileSystem.OsString (OsString)
import FileSystem.OsString qualified as OsString
import Kairos.Runner.Args.TH qualified as TH
import Kairos.Types.Date (Date)
import Kairos.Types.Date qualified as Date
import Kairos.Types.TimeFormat (TimeFormat)
import Kairos.Types.TimeFormat qualified as TimeFmt
import Options.Applicative
  ( FlagFields,
    Mod,
    Parser,
    ParserInfo
      ( ParserInfo,
        infoFailureCode,
        infoFooter,
        infoFullDesc,
        infoHeader,
        infoParser,
        infoPolicy,
        infoProgDesc
      ),
    (<**>),
  )
import Options.Applicative qualified as OA
import Options.Applicative.Help (Chunk (Chunk), Doc)
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import Paths_kairos qualified as Paths
import System.Info qualified as Info

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { color :: Maybe Bool,
    config :: Maybe OsPath,
    noConfig :: Bool,
    date :: Maybe Date,
    destTZ :: Maybe Text,
    formatIn :: Maybe TimeFormat,
    formatOut :: Maybe TimeFormat,
    printAliases :: Bool,
    srcTZ :: Maybe Text,
    stacktrace :: Bool,
    timeString :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | Optparse-Applicative info.
--
-- @since 0.1
parserInfo :: ParserInfo Args
parserInfo =
  ParserInfo
    { infoParser = parseArgs,
      infoFullDesc = True,
      infoProgDesc = desc,
      infoHeader = Chunk header,
      infoFooter = Chunk footer,
      infoFailureCode = 1,
      infoPolicy = Intersperse
    }
  where
    header = Just "kairos: A tool for timezone conversions."
    footer = Just $ fromString versShort
    desc =
      Chunk.vsepChunks
        [ Chunk.paragraph $
            mconcat
              [ "Kairos reads time strings and converts between timezones. ",
                "For the src and dest options, TZ refers to labels like ",
                "'America/New_York' or offsets like '+1300'. See ",
                "https://en.wikipedia.org/wiki/Tz_database."
              ],
          Chunk.paragraph
            "See https://github.com/tbidne/kairos#README for full documentation.",
          Chunk.paragraph "Examples:",
          mkExample
            [ "$ kairos",
              "Mon, 17 Mar 2025 10:12:10 NZDT"
            ],
          mkExample
            [ "$ kairos -d europe/paris",
              "Sun, 16 Mar 2025 22:12:10 CET"
            ],
          mkExample
            [ "$ kairos --date 2025-04-17 -s america/new_york 18:30",
              "Fri, 18 Apr 2025 10:30:00 NZST"
            ]
        ]

    mkExample :: [String] -> Chunk Doc
    mkExample =
      Chunk.vcatChunks
        . fmap (fmap (Pretty.indent 2) . Chunk.stringChunk)

parseArgs :: Parser Args
parseArgs =
  parseArgs'
    <**> OA.helper
    <**> version
  where
    parseArgs' = do
      ~(config, noConfig) <- parseConfigGroup
      ~(color, formatIn, formatOut) <- parseFormattingGroup
      printAliases <- parseMiscGroup
      ~(date, timeString) <- parseTimeGroup
      ~(destTZ, srcTZ) <- parseTimezonesGroup

      stacktrace <- parseStacktrace

      pure $
        MkArgs
          { color,
            config,
            noConfig,
            date,
            destTZ,
            formatIn,
            formatOut,
            printAliases,
            srcTZ,
            stacktrace,
            timeString
          }
      where
        parseConfigGroup =
          OA.parserOptionGroup "Config options:" $
            (,) <$> parseConfig <*> parseNoConfig

        parseFormattingGroup =
          OA.parserOptionGroup "Formatting options:" $
            (,,) <$> parseColor <*> parseFormatIn <*> parseFormatOut

        parseMiscGroup = OA.parserOptionGroup "Misc options:" $ parsePrintAliases

        parseTimezonesGroup =
          OA.parserOptionGroup "Timezones options:" $
            (,) <$> parseDestTZ <*> parseSrcTZ

        parseTimeGroup =
          OA.parserOptionGroup "Time options:" $
            (,) <$> parseDate <*> parseTimeStr

parseStacktrace :: Parser Bool
parseStacktrace = parseSwitch "stacktrace" [OA.internal]

parseConfig :: Parser (Maybe OsPath)
parseConfig =
  OA.optional
    $ OA.option
      osPath
    $ mconcat
      [ OA.long "config",
        OA.short 'c',
        OA.metavar "PATH",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Path to TOML config file. If not given, we automatically look in ",
          "the XDG config e.g. ~/.config/kairos/config.toml."
        ]

parseNoConfig :: Parser Bool
parseNoConfig = parseSwitch "no-config" [OA.hidden, mkHelpNoLine helpTxt]
  where
    helpTxt = "Disables --config."

parseDestTZ :: Parser (Maybe Text)
parseDestTZ =
  OA.optional
    $ OA.option
      OA.str
    $ mconcat
      [ OA.long "dest-tz",
        OA.short 'd',
        OA.metavar "TZ",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Timezone in which to convert the read string. Must be a tz database ",
          "label or offset e.g. 'America/New_York', '+1300'. If none is given ",
          "then we use the local system timezone."
        ]

parseFormatIn :: Parser (Maybe TimeFormat)
parseFormatIn =
  OA.optional
    $ OA.option
      (fromString <$> OA.str)
    $ mconcat
      [ OA.long "format-in",
        OA.short 'f',
        OA.metavar "FMT_STR",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string for parsing the time string. Should not ",
          "contain a timezone flag like %Z (see --src-tz) nor a date ",
          "(see --date). Defaults to standard 12 and 24 hour formats e.g. ",
          "'17:00', '0300', '4:30 pm', '2 am'. See 'man date' for basic examples."
        ]

parseFormatOut :: Parser (Maybe TimeFormat)
parseFormatOut =
  OA.optional
    $ OA.option
      readFormat
    $ mconcat
      [ OA.long "format-out",
        OA.short 'o',
        OA.metavar "(rfc822 | FMT_STR)",
        mkHelpNoLine helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Like --format-in, but used for the output. If this is not ",
          "present we default to rfc822 i.e. RFC822."
        ]

readFormat :: ReadM TimeFormat
readFormat =
  OA.str <&> \case
    "rfc822" -> TimeFmt.rfc822
    other -> fromString other

parsePrintAliases :: Parser Bool
parsePrintAliases =
  parseSwitch
    "print-aliases"
    [mkHelpNoLine "Prints aliases from toml config."]

parseColor :: Parser (Maybe Bool)
parseColor =
  OA.optional
    $ OA.option
      readColor
    $ mconcat
      [ OA.long "color",
        OA.metavar "(true | false)",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      "Determines if we color the --print-aliases output. Defaults to 'true'."
    readColor =
      OA.str >>= \case
        "true" -> pure True
        "false" -> pure False
        other -> fail $ "Unexpected color: " ++ other

parseSwitch :: String -> [Mod FlagFields Bool] -> Parser Bool
parseSwitch name mods = OA.switch (mconcat $ OA.long name : mods)

parseSrcTZ :: Parser (Maybe Text)
parseSrcTZ =
  OA.optional
    $ OA.option
      OA.str
    $ mconcat
      [ OA.long "src-tz",
        OA.short 's',
        OA.metavar "TZ",
        mkHelpNoLine helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Timezone in which to read the string. Must be a tz database ",
          "label or offset e.g. 'America/New_York', '+1300'. If none is given ",
          "then we use the local system timezone. This option requires TIME_STR."
        ]

parseDate :: Parser (Maybe Date)
parseDate =
  OA.optional $
    OA.option readDate $
      mconcat
        [ OA.long "date",
          OA.metavar "YYYY-mm-dd",
          mkHelp helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Date in which to read the string. This option requires TIME_STR. ",
          "No date uses the current date, as determined by the source."
        ]
    readDate = OA.str >>= Date.parseDateString

parseTimeStr :: Parser (Maybe Text)
parseTimeStr =
  OA.optional $
    T.pack
      <$> OA.argument
        OA.str
        (OA.metavar "TIME_STR" <> mkHelpNoLine helpTxt)
  where
    helpTxt =
      "Time string to parse. If none is given then we parse the"
        <> " local system time. To format the output, use --format-out."

version :: Parser (a -> a)
version = OA.infoOption versLong (OA.long "version" <> OA.short 'v' <> OA.hidden)

versShort :: String
versShort =
  mconcat
    [ "Version: ",
      showVersion Paths.version,
      " (",
      OsString.decodeLenient versionInfo.gitShortHash,
      ")"
    ]

versLong :: String
versLong =
  L.intercalate
    "\n"
    [ "Kairos: " <> showVersion Paths.version,
      " - Git revision: " <> OsString.decodeLenient versionInfo.gitHash,
      " - Commit date:  " <> OsString.decodeLenient versionInfo.gitCommitDate,
      " - GHC version:  " <> versionInfo.ghc
    ]

data VersionInfo = MkVersionInfo
  { ghc :: String,
    gitCommitDate :: OsString,
    gitHash :: OsString,
    gitShortHash :: OsString
  }

versionInfo :: VersionInfo
versionInfo =
  MkVersionInfo
    { ghc = showVersion Info.compilerVersion,
      gitCommitDate = d,
      gitHash = h,
      gitShortHash = sh
    }
  where
    (d, h, sh) = $$(TH.gitData)

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph

mkHelpNoLine :: String -> OA.Mod f a
mkHelpNoLine =
  OA.helpDoc
    . Chunk.unChunk
    . Chunk.paragraph
