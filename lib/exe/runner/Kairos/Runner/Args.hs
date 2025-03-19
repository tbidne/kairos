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
import Data.Version (Version (versionBranch))
import Effects.Optparse (OsPath, osPath)
import Kairos.Types.Date (Date)
import Kairos.Types.Date qualified as Date
import Kairos.Types.TimeFormat (TimeFormat)
import Kairos.Types.TimeFormat qualified as TimeFmt
import Options.Applicative
  ( Parser,
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

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { config :: Maybe OsPath,
    noConfig :: Bool,
    date :: Maybe Date,
    destTZ :: Maybe Text,
    formatIn :: Maybe TimeFormat,
    formatOut :: Maybe TimeFormat,
    srcTZ :: Maybe Text,
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
    footer = Just $ fromString versNum
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
  MkArgs
    <$> parseConfig
    <*> parseNoConfig
    <*> parseDate
    <*> parseDestTZ
    <*> parseFormatIn
    <*> parseFormatOut
    <*> parseSrcTZ
    <*> parseTimeStr
      <**> OA.helper
      <**> version

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
parseNoConfig =
  OA.switch
    ( mconcat
        [ OA.long "no-config",
          OA.hidden,
          mkHelp helpTxt
        ]
    )
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
        mkHelp helpTxt
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

parseSrcTZ :: Parser (Maybe Text)
parseSrcTZ =
  OA.optional
    $ OA.option
      OA.str
    $ mconcat
      [ OA.long "src-tz",
        OA.short 's',
        OA.metavar "TZ",
        mkHelp helpTxt
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
        (OA.metavar "TIME_STR" <> mkHelp helpTxt)
  where
    helpTxt =
      "Time string to parse. If none is given then we parse the"
        <> " local system time. To format the output, use --format-out."

version :: Parser (a -> a)
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v' <> OA.hidden)

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
