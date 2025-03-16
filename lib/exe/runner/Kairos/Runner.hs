{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Internal library for testing.
--
-- @since 0.1
module Kairos.Runner
  ( runKairos,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Effects.FileSystem.FileReader (MonadFileReader, readFileUtf8ThrowM)
import Effects.FileSystem.PathReader
  ( MonadPathReader (doesFileExist),
    OsPath,
    getXdgConfig,
  )
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import FileSystem.OsPath (osp, (</>))
import GHC.Stack.Types (HasCallStack)
import Kairos qualified
import Kairos.Runner.Args
  ( Args
      ( config,
        date,
        destTZ,
        formatIn,
        formatOut,
        noConfig,
        noDate,
        srcTZ,
        timeString
      ),
    parserInfo,
  )
import Kairos.Runner.Toml (Toml)
import Kairos.Types.Date (Date (DateToday))
import Kairos.Types.Exception
  ( DateNoTimeStringException (MkDateNoTimeStringException),
    ParseTZInputException (MkParseTZInputException),
    SrcTZNoTimeStringException (MkSrcTZNoTimeStringException),
  )
import Kairos.Types.TZInput (TZInput)
import Kairos.Types.TZInput qualified as TZInput
import Kairos.Types.TimeFormat qualified as TimeFmt
import Kairos.Types.TimeFormat qualified as TimeFormat
import Kairos.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        formats,
        srcTZ,
        timeString
      ),
  )
import Optics.Core
  ( Prism',
    preview,
    prism,
    (%),
    (^.),
    _Just,
  )
import Optics.Core.Extras (is)
import TOML qualified

-- | Runs kairos with CLI args.
--
-- @since 0.1
runKairos ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m
  ) =>
  m ()
runKairos = do
  args <- execParser parserInfo
  runWithArgs args

-- | Runs kairos with given args.
--
-- @since 0.1
runWithArgs ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadPathReader m,
    MonadTerminal m,
    MonadTime m
  ) =>
  Args ->
  m ()
runWithArgs args = do
  case args.timeString of
    Just _ -> pure ()
    Nothing -> do
      case args.srcTZ of
        Just _ -> throwM MkSrcTZNoTimeStringException
        Nothing -> pure ()
      case args.date of
        Just _ -> throwM MkDateNoTimeStringException
        Nothing -> pure ()

  let formatOut = fromMaybe TimeFmt.rfc822 args.formatOut
      formatOutStr = T.unpack $ formatOut ^. #unTimeFormat

  mToml <-
    if args.noConfig
      then pure Nothing
      else mGetToml args.config

  let aliases :: Maybe (Map Text Text)
      aliases = preview (_Just % #aliases % _Just) mToml

      today :: Bool
      today = is (_Just % #today % _Just % _True) mToml

  mTimeReader <- case args.timeString of
    Nothing -> pure Nothing
    Just timeString -> do
      srcTZ <- parseTZ aliases args.srcTZ

      -- CLI date (date string or literal 'today') can be overridden
      --    in exactly one scenario:
      --
      --    1. noDate is False (--no-date unspecified).
      --    2. CLI --date unspecified.
      --    3. toml.today = true
      let date
            -- 1. --no-date specified: overrides all.
            | args.noDate = Nothing
            | otherwise = case args.date of
                -- 2. --date specified: use it.
                Just d -> Just d
                -- 3. Overwrite w/ DateToday iff it is set on the toml.
                Nothing -> if today then Just DateToday else Nothing

      let formats :: NonEmpty TimeFormat.TimeFormat
          formats = case args.formatIn of
            Nothing -> TimeFormat.defaultTimeFormats
            Just fmt -> NE.singleton fmt

      pure $
        Just $
          MkTimeReader
            { formats,
              date,
              srcTZ,
              timeString
            }

  destTZ <- parseTZ aliases args.destTZ

  readAndHandle mTimeReader destTZ formatOutStr
  where
    readAndHandle ::
      (HasCallStack) =>
      Maybe TimeReader ->
      Maybe TZInput ->
      String ->
      m ()
    readAndHandle tr d fmt = do
      time <- Kairos.readConvertTime tr d
      let result = T.pack $ Format.formatTime locale fmt time
      T.putTextLn result
    -- NOTE: It seems that the locale's timezone info is not used when
    -- formatting the output, so we do not have to worry about including
    -- extra tz info here.
    locale = Format.defaultTimeLocale

parseTZ ::
  forall m.
  ( HasCallStack,
    MonadThrow m
  ) =>
  -- | Maybe aliases map.
  Maybe (Map Text Text) ->
  Maybe Text ->
  m (Maybe TZInput)
parseTZ _ Nothing = pure Nothing
parseTZ mAliasMap (Just txt) = do
  -- If the map exists, lookup the txt in it first, falling back to itself
  -- if it is not found. Then attempt to parse the result.
  let toParse = maybe txt (Map.findWithDefault txt txt) mAliasMap
  Just <$> parseFromTxt toParse
  where
    parseFromTxt :: Text -> m TZInput
    parseFromTxt t = case TZInput.parseTZInput t of
      Just tz -> pure tz
      Nothing -> throwM $ MkParseTZInputException t

mGetToml ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Path to toml config file.
  Maybe OsPath ->
  -- | Reads toml, if we are given a path or we find it via XDG.
  m (Maybe Toml)
mGetToml mconfigPath = do
  case mconfigPath of
    Nothing -> do
      configDir <- getXdgConfig [osp|kairos|]
      let configPath = configDir </> [osp|config.toml|]
      exists <- doesFileExist configPath
      if exists
        then Just <$> readToml configPath
        else pure Nothing
    Just configPath -> Just <$> readToml configPath
  where
    readToml configPath = do
      contents <- readFileUtf8ThrowM configPath
      case TOML.decode @Toml contents of
        Left ex -> throwM ex
        Right toml -> pure toml

_True :: Prism' Bool ()
_True =
  prism
    (const True)
    ( \case
        True -> Right ()
        other -> Left other
    )
