{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Internal library for testing.
--
-- @since 0.1
module Kairos.Runner
  ( -- * Runners
    runKairos,
    runKairosIO,

    -- * Exceptions
    PrintAliasesE (..),
  )
where

import Control.Exception (Exception (displayException))
import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Control.Monad (unless, when)
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
import Effects.FileSystem.FileWriter (MonadFileWriter)
import Effects.FileSystem.FileWriter qualified as FW
import Effects.FileSystem.PathReader
  ( MonadPathReader (doesFileExist),
    OsPath,
    getXdgConfig,
    getXdgState,
  )
import Effects.FileSystem.PathWriter (MonadPathWriter)
import Effects.FileSystem.PathWriter qualified as PW
import Effects.Optparse (MonadOptparse (execParser))
import Effects.System.Terminal (MonadTerminal)
import Effects.System.Terminal qualified as T
import Effects.Time (MonadTime)
import FileSystem.OsPath (decodeLenient, osp, (</>))
import GHC.Stack.Types (HasCallStack)
import Kairos qualified
import Kairos.Runner.Args
  ( Args
      ( color,
        config,
        date,
        destTZ,
        formatIn,
        formatOut,
        printAliases,
        srcTZ,
        stacktrace,
        timeString
      ),
    WithDisabled (Disabled, With),
    parserInfo,
  )
import Kairos.Runner.Toml (Toml (aliases, color))
import Kairos.Types.Exception
  ( DateNoTimeStringException (MkDateNoTimeStringException),
    LocalSystemTimeException,
    LocalTZException,
    LocalTimeZoneException,
    ParseTZInputException (MkParseTZInputException),
    ParseTimeException,
    SrcTZNoTimeStringException (MkSrcTZNoTimeStringException),
  )
import Kairos.Types.TZInput (TZInput, locale)
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
import Optics.Core (preview, (%), _Just)
import TOML qualified

-- | 'IO'-specialized version of 'runKairos' that hides Kairos exception
-- callstacks iff the (internal) --stacktrace flag is _not_ set (default).
--
-- @since 0.1
runKairosIO :: (HasCallStack) => IO ()
runKairosIO = do
  (xdgKairosState, args) <- getArgs

  unless args.stacktrace $ AnnUtils.setIgnoreKnownCallStackHandler proxies

  runWithArgs xdgKairosState args
  where
    proxies =
      [ MkExceptionProxy @DateNoTimeStringException,
        MkExceptionProxy @LocalSystemTimeException,
        MkExceptionProxy @LocalTimeZoneException,
        MkExceptionProxy @LocalTZException,
        MkExceptionProxy @ParseTimeException,
        MkExceptionProxy @ParseTZInputException,
        MkExceptionProxy @PrintAliasesE,
        MkExceptionProxy @SrcTZNoTimeStringException
      ]

getArgs ::
  ( MonadFileReader m,
    MonadOptparse m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  m (OsPath, Args)
getArgs = do
  xdgKairosState <- getXdgState [osp|kairos|]
  aliasCompletions <- readAliasCompletions xdgKairosState
  args <- execParser (parserInfo aliasCompletions)
  pure (xdgKairosState, args)

-- | Runs kairos with CLI args.
--
-- @since 0.1
runKairos ::
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadOptparse m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  m ()
runKairos = do
  (xdgKairosState, args) <- getArgs
  runWithArgs xdgKairosState args

-- | Exception for printing aliases.
data PrintAliasesE
  = -- | --print-aliases and --config off specified, nonsensical.
    PrintAliasesNoConfig
  | -- | No toml was found.
    PrintAliasesNoToml
  deriving stock (Show)

instance Exception PrintAliasesE where
  displayException PrintAliasesNoConfig =
    "--print-aliases was specified with --config off."
  displayException PrintAliasesNoToml =
    "--print-aliases was specified, but no config file was found."

-- | Runs kairos with given args.
--
-- @since 0.1
runWithArgs ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  OsPath ->
  Args ->
  m ()
runWithArgs xdgStateKairos args =
  if args.printAliases
    then handlePrintAliases xdgStateKairos args
    else handleMain xdgStateKairos args

handlePrintAliases ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  OsPath ->
  Args ->
  m ()
handlePrintAliases xdgStateKairos args = do
  when (args.config == Just Disabled) (throwM PrintAliasesNoConfig)

  mGetTomlAndPath args.config >>= \case
    Nothing -> throwM PrintAliasesNoToml
    Just (path, toml) -> do
      let shouldColor = case args.color of
            -- CLI args takes priority
            Just c -> c
            Nothing -> case toml.color of
              -- Default to True
              Nothing -> True
              Just c -> c
          emptyMsg =
            "No aliases found in toml: " <> T.pack (decodeLenient path)
      case toml.aliases of
        Nothing -> T.putTextLn emptyMsg
        Just aliases -> do
          let txt =
                if null aliases
                  then emptyMsg
                  else fmtAliases shouldColor aliases
          T.putTextLn txt
          saveAliasCompletions xdgStateKairos (Just aliases)
  where
    fmtAliases :: Bool -> Map Text Text -> Text
    fmtAliases shouldColor aliases =
      unlinesNoTrailingNL $
        if shouldColor
          then fmtColor
          else fmtNoColor
      where
        -- avoid T.unlines to skip trailing newline.
        unlinesNoTrailingNL = T.intercalate "\n"

        maxLen = Map.foldlWithKey' findMaxLen 0 aliases

        findMaxLen :: Int -> Text -> Text -> Int
        findMaxLen !maxSoFar k _ = max maxSoFar (T.length k)

        fmtColor = fmtLineColor <$> zip colorStream (Map.toList aliases)
        fmtNoColor = fmtLineNoColor <$> Map.toList aliases

        fmtLineColor :: (Text, (Text, Text)) -> Text
        fmtLineColor (c, (k, v)) = c <> fmtLineNoColor (k, v) <> endColor

        fmtLineNoColor :: (Text, Text) -> Text
        fmtLineNoColor (k, v) =
          mconcat
            [ "- ",
              k,
              ": ",
              T.replicate numPad " ",
              v
            ]
          where
            numPad = maxLen - T.length k

        c1 = "\ESC[34m"
        c2 = "\ESC[32m"
        endColor = "\ESC[0m"

        colorStream = c1 : c2 : colorStream

handleMain ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadFileReader m,
    MonadFileWriter m,
    MonadPathReader m,
    MonadPathWriter m,
    MonadTerminal m,
    MonadTime m
  ) =>
  OsPath ->
  Args ->
  m ()
handleMain xdgStateKairos args = do
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
      formatOutStr = T.unpack $ formatOut.unTimeFormat

  mToml <- mGetToml args.config

  let aliases :: Maybe (Map Text Text)
      aliases = preview (_Just % #aliases % _Just) mToml

  mTimeReader <- case args.timeString of
    Nothing -> pure Nothing
    Just timeString -> do
      srcTZ <- parseTZ aliases args.srcTZ

      let formats :: NonEmpty TimeFormat.TimeFormat
          formats = case args.formatIn of
            Nothing -> TimeFormat.defaultTimeFormats
            Just fmt -> NE.singleton fmt

      pure $
        Just $
          MkTimeReader
            { formats,
              date = args.date,
              srcTZ,
              timeString
            }

  destTZ <- parseTZ aliases args.destTZ

  readAndHandle mTimeReader destTZ formatOutStr

  saveAliasCompletions xdgStateKairos aliases
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
  Maybe (WithDisabled OsPath) ->
  -- | Reads toml, if we are given a path or we find it via XDG.
  m (Maybe Toml)
mGetToml = (fmap . fmap) snd . mGetTomlAndPath

mGetTomlAndPath ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  -- | Path to toml config file.
  Maybe (WithDisabled OsPath) ->
  -- | Reads toml, if we are given a path or we find it via XDG.
  m (Maybe (OsPath, Toml))
mGetTomlAndPath mconfigPath = do
  case mconfigPath of
    Nothing -> do
      configDir <- getXdgConfig [osp|kairos|]
      let configPath = configDir </> [osp|config.toml|]
      exists <- doesFileExist configPath
      if exists
        then Just . (configPath,) <$> readToml configPath
        else pure Nothing
    Just Disabled -> pure Nothing
    Just (With configPath) -> Just . (configPath,) <$> readToml configPath
  where
    readToml configPath = do
      contents <- readFileUtf8ThrowM configPath
      case TOML.decode @Toml contents of
        Left ex -> throwM ex
        Right toml -> pure toml

readAliasCompletions ::
  ( HasCallStack,
    MonadFileReader m,
    MonadPathReader m,
    MonadThrow m
  ) =>
  OsPath ->
  m [Text]
readAliasCompletions xdgState = do
  exists <- doesFileExist compsPath
  if exists
    then do
      contents <- readFileUtf8ThrowM compsPath
      pure $ T.lines $ T.strip contents
    else pure []
  where
    compsPath = mkAliasCompletionsPath xdgState

saveAliasCompletions ::
  ( HasCallStack,
    MonadFileWriter m,
    MonadPathWriter m
  ) =>
  OsPath ->
  Maybe (Map Text Text) ->
  m ()
saveAliasCompletions _ Nothing = pure ()
saveAliasCompletions xdgState (Just aliases) = do
  PW.createDirectoryIfMissing True xdgState
  FW.writeFileUtf8 compsPath (formatAliases aliases)
  where
    formatAliases = T.intercalate "\n" . Map.keys

    compsPath = mkAliasCompletionsPath xdgState

mkAliasCompletionsPath :: OsPath -> OsPath
mkAliasCompletionsPath xdgState = xdgState </> [osp|aliases.txt|]
