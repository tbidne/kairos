{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

-- | CLI args for TimeConv.
--
-- @since 0.1
module TimeConv.Runner.Args
  ( Args (..),
    parserInfo,
  )
where

import Data.Functor ((<&>))
import Data.List qualified as L
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Conversion.Types.Date (Date)
import Data.Time.Conversion.Types.Date qualified as Date
import Data.Time.Conversion.Types.TimeFormat (TimeFormat)
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Version (Version (versionBranch))
import Effects.Optparse (OsPath, osPath)
import Optics.Core ((^.))
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
import Options.Applicative.Help (Chunk (Chunk))
import Options.Applicative.Help.Chunk qualified as Chunk
import Options.Applicative.Help.Pretty qualified as Pretty
import Options.Applicative.Types (ArgPolicy (Intersperse), ReadM)
import Paths_time_conv qualified as Paths

-- | CLI args.
--
-- @since 0.1
data Args = MkArgs
  { config :: Maybe OsPath,
    noConfig :: Bool,
    date :: Maybe Date,
    noDate :: Bool,
    destTZ :: Maybe Text,
    formatIn :: TimeFormat,
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
    header = Just "time-conv: A tool for timezone conversions."
    footer = Just $ fromString versNum
    desc =
      Chunk.paragraph $
        "time-conv reads time strings and converts between timezones. "
          <> "For the src and dest options, TZ refers to labels like "
          <> "'America/New_York' or offsets like '+1300'. See "
          <> "https://en.wikipedia.org/wiki/Tz_database."

parseArgs :: Parser Args
parseArgs =
  MkArgs
    <$> parseConfig
    <*> parseNoConfig
    <*> parseDate
    <*> parseNoDate
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
        [ "Path to TOML config file. It not given we automatically look in ",
          "the XDG config e.g. ~/.config/time-conv/config.toml."
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

parseFormatIn :: Parser TimeFormat
parseFormatIn =
  OA.option
    (fromString <$> OA.str)
    $ mconcat
      [ OA.value TimeFmt.defaultTimeFormat,
        OA.long "format-in",
        OA.short 'f',
        OA.metavar "FMT_STR",
        mkHelp helpTxt
      ]
  where
    helpTxt =
      mconcat
        [ "Glibc-style format string for parsing the time string. Should not ",
          "contain a timezone flag like %Z (see --src-tz) nor a date ",
          "(see --date). Defaults to ",
          defFormatStr,
          " i.e. 24-hr hour:minute. See 'man date' for basic examples."
        ]
    defFormatStr = T.unpack $ TimeFmt.defaultTimeFormat ^. #unTimeFormat

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
          OA.metavar "(today | YYYY-mm-dd)",
          mkHelp helpTxt
        ]
  where
    helpTxt =
      mconcat
        [ "Date in which to read the string. Today uses the current date, as ",
          "determined by the source.  This option requires TIME_STR."
        ]
    readDate = OA.str >>= Date.parseDate

parseNoDate :: Parser Bool
parseNoDate =
  OA.switch $
    mconcat
      [ OA.long "no-date",
        OA.hidden,
        mkHelp "Disables --date. Useful for disabling the toml field 'today'."
      ]

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
version = OA.infoOption versNum (OA.long "version" <> OA.short 'v')

versNum :: String
versNum = "Version: " <> L.intercalate "." (show <$> versionBranch Paths.version)

mkHelp :: String -> OA.Mod f a
mkHelp =
  OA.helpDoc
    . fmap (<> Pretty.hardline)
    . Chunk.unChunk
    . Chunk.paragraph
