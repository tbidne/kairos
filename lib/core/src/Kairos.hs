{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides functions for reading time strings. We also provide
-- functions for converting between timezones.
--
-- @since 0.1
module Kairos
  ( -- * High-level parsing/conversion
    readConvertTime,
    readTime,
    convertTime,

    -- * Low-level functions

    -- ** Parsing time strings
    readTimeFormatM,
    readTimeFormat,

    -- ** Converting ZonedTime
    convertZoned,
    convertLocalToZoned,

    -- * Types
    Date (..),
    TimeFormat (..),
    TimeReader (..),
    TZInput (..),

    -- ** Re-exports
    TZLabel (..),
    ZonedTime (..),

    -- ** Exceptions
    ParseTimeException (..),
    ParseTZInputException (..),
    LocalTimeZoneException (..),
    LocalTZException (..),
    LocalSystemTimeException (..),
  )
where

import Control.Applicative (asum)
import Control.Exception.Utils (catchSync, trySync)
import Control.Monad.Catch
  ( MonadCatch,
    MonadThrow,
    throwM,
  )
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (ParseTime)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay,
    TimeZone,
    ZonedTime
      ( ZonedTime,
        zonedTimeToLocalTime,
        zonedTimeZone
      ),
  )
import Data.Time.LocalTime qualified as Local
import Data.Time.Zones (TZ)
import Data.Time.Zones qualified as Zones
import Data.Time.Zones.All (TZLabel (..))
import Data.Time.Zones.All qualified as All
import Effects.System.Environment (MonadEnv)
import Effects.System.Environment qualified as Env
import Effects.Time (MonadTime (getSystemZonedTime, getTimeZone, loadLocalTZ))
import GHC.Stack (HasCallStack)
import Kairos.Internal qualified as Internal
import Kairos.Types.Date (Date (MkDateString))
import Kairos.Types.Exception
  ( LocalSystemTimeException (MkLocalSystemTimeException),
    LocalTZException (MkLocalTZException),
    LocalTimeZoneException (MkLocalTimeZoneException),
    ParseTZInputException (MkParseTZInputException),
    ParseTimeException (MkParseTimeException),
  )
import Kairos.Types.TZInput (TZInput (TZActual, TZDatabase))
import Kairos.Types.TZInput qualified as TZInput
import Kairos.Types.TimeFormat
  ( TimeFormat (MkTimeFormat, unTimeFormat),
  )
import Kairos.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        formats,
        srcTZ,
        timeString
      ),
  )

-- | Reads the given time string based on the source 'TimeReader' and
-- converts to the destination timezone. This is the composition of
-- 'readTime' and 'convertTime'. If the source is 'Nothing' then we read
-- the local system time. Similarly, if the dest is 'Nothing', we convert
-- to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalTZException': Error retrieving local tz_database name.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- @since 0.1
readConvertTime ::
  ( HasCallStack,
    MonadCatch m,
    MonadEnv m,
    MonadTime m
  ) =>
  -- | Source time.
  Maybe TimeReader ->
  -- | Dest timezone.
  Maybe TZInput ->
  -- | Converted time.
  m ZonedTime
readConvertTime mtimeReader destTZ =
  readTime mtimeReader >>= (`convertTime` destTZ)

-- | Reads a time based on the 'TimeReader'. If given 'Nothing' we read the
-- local system time instead.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalTZException': Error retrieving local tz_database name.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- @since 0.1
readTime ::
  ( HasCallStack,
    MonadCatch m,
    MonadEnv m,
    MonadTime m
  ) =>
  -- | Optional time reader.
  Maybe TimeReader ->
  -- | Read time.
  m ZonedTime
readTime (Just timeReader) = readTimeString timeReader
readTime Nothing = getSystemZonedTimeKairos

-- | Converts the given time to the destination timezone. If no destination
-- timezone is given then we convert to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- @since 0.1
convertTime ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  -- | Time to convert.
  ZonedTime ->
  -- | Optional destination timezone.
  Maybe TZInput ->
  -- | Converted time.
  m ZonedTime
convertTime inTime Nothing = do
  let inTimeUtc = Local.zonedTimeToUTC inTime
  currTZ <- getTimeZoneKairos inTimeUtc
  pure $ Local.utcToZonedTime currTZ inTimeUtc
convertTime inTime (Just tzOut) = pure $ convertZoned inTime tzOut

readTimeString ::
  forall m.
  ( HasCallStack,
    MonadCatch m,
    MonadEnv m,
    MonadTime m
  ) =>
  TimeReader ->
  m ZonedTime
readTimeString timeReader = case (timeReader.date, timeReader.srcTZ) of
  (Nothing, Nothing) -> onNoInputs
  (Nothing, Just srcTZ) -> onSrcTZ srcTZ
  (Just date, Nothing) -> onDate date
  (Just date, Just srcTZ) -> onDateAndSrcTZ date srcTZ
  where
    -- 1. We are given no date nor tz info. Interpret given TimeOfDay in
    -- current system timezone.
    onNoInputs :: (HasCallStack) => m ZonedTime
    onNoInputs = do
      ZonedTime (LocalTime currSysDay _) currSysTz <- getSystemZonedTimeKairos
      givenTod <- readTimeFormatM formats timeReader.timeString
      pure $ ZonedTime (LocalTime currSysDay givenTod) currSysTz

    -- 2. We are given no date but some source timezone info. Interpret given
    -- TimeOfDay as current source timezone.
    onSrcTZ :: (HasCallStack) => TZInput -> m ZonedTime
    onSrcTZ src = do
      currSysZonedTime <- getSystemZonedTimeKairos
      let ZonedTime (LocalTime currSrcDay _) curSrcTz =
            convertZoned currSysZonedTime src
      givenTod <- readTimeFormatM formats timeReader.timeString
      pure $ ZonedTime (LocalTime currSrcDay givenTod) curSrcTz

    -- 3. We are given date but no timezone info. Interpret given TimeOfDay in
    -- system timezone at that date.
    onDate :: (HasCallStack) => Date -> m ZonedTime
    onDate (MkDateString date) = do
      givenDay <- readTimeFormatM @Day dayFmt date
      givenTod <- readTimeFormatM @TimeOfDay formats timeReader.timeString
      let givenLocalTime = LocalTime givenDay givenTod

      -- see NOTE: [LocalTZException error message]
      localTZ <- loadLocalTZKairos

      let givenUtcTime = Zones.localTimeToUTCTZ localTZ givenLocalTime
          timeZone = Zones.timeZoneForUTCTime localTZ givenUtcTime

      pure $ ZonedTime givenLocalTime timeZone

    -- 4. We are given date and timezone info. Interpret given TimeOfDay in
    -- source timezone at that date.
    onDateAndSrcTZ :: (HasCallStack) => Date -> TZInput -> m ZonedTime
    onDateAndSrcTZ (MkDateString date) srcTZ = do
      givenDay <- readTimeFormatM @Day dayFmt date
      givenTod <- readTimeFormatM @TimeOfDay formats timeReader.timeString
      let givenLocalTime = LocalTime givenDay givenTod
      pure $ convertLocalToZoned givenLocalTime srcTZ

    formats :: NonEmpty TimeFormat
    formats = timeReader.formats

    dayFmt :: NonEmpty TimeFormat
    dayFmt = NE.singleton dateString

    dateString :: (IsString s) => s
    dateString = "%Y-%m-%d"

-- | 'readTimeFormatM' that throws 'ParseTimeException'.
--
-- @since 0.1
readTimeFormatM ::
  forall t m.
  ( HasCallStack,
    MonadThrow m,
    ParseTime t
  ) =>
  -- | Formats.
  NonEmpty TimeFormat ->
  -- | Text to parse.
  Text ->
  m t
readTimeFormatM formats timeStr = case readTimeFormat formats timeStr of
  Nothing -> throwM $ MkParseTimeException formats timeStr
  Just t -> pure t

-- | @readTimeFormat locale format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- @since 0.1
readTimeFormat :: (ParseTime t) => NonEmpty TimeFormat -> Text -> Maybe t
readTimeFormat formats timeStr =
  asum (parseFn . toFmtStr <$> formats)
  where
    parseFn f = Format.parseTimeM True TZInput.locale f timeStr'

    toFmtStr :: TimeFormat -> String
    toFmtStr = T.unpack . (.unTimeFormat)
    timeStr' = T.unpack timeStr

-- | Converts a local time to the given timezone.
--
-- @since 0.1
convertLocalToZoned :: LocalTime -> TZInput -> ZonedTime
convertLocalToZoned localTime tzInput =
  case tzInput of
    -- We have a TZLabel. We need to perform to/from UTC conversions to get
    -- the correct time, because the Label -> TimeZone function is not
    -- constant (consider e.g. daylight savings).
    TZDatabase label -> convertFromLabel Zones.localTimeToUTCTZ localTime label
    -- If we have a TimeZone then we can just create the ZonedTime directly.
    -- No need for any conversions.
    TZActual timeZone -> ZonedTime localTime timeZone

-- | Converts a zoned time to the given timezone.
--
-- @since 0.1
convertZoned :: ZonedTime -> TZInput -> ZonedTime
convertZoned zonedTime tzInput =
  case tzInput of
    TZDatabase label -> convertFromLabel f zonedTime label
    TZActual timeZone -> convertFromActual zonedTime timeZone
  where
    f :: forall a. a -> ZonedTime -> UTCTime
    f = const Local.zonedTimeToUTC

-- | Converts the ZonedTime to UTCTime and finally the requested TimeZone.
--
-- The parameter 'TimeZone' is the destination.
convertFromActual :: ZonedTime -> TimeZone -> ZonedTime
convertFromActual zt timeZone = Local.utcToZonedTime timeZone utcTime
  where
    -- Convert to UTC.
    utcTime = Local.zonedTimeToUTC zt

-- | Converts some time ('Data.Time.LocalTime' or 'Data.Time.ZonedTime') to
-- 'UTCTime' then finally a ZonedTime based on the given 'TZLabel'.
--
-- ZonedTime is expected to pass a toUtcTime function that ignored the
-- 'TZ' parameter, since it already has its own time zone info. the 'TZLabel'
-- is used purely to convert the destination.
--
-- LocalTime, on the other hand, is expected to use 'TZ' since it does not
-- have its own time zone info. This may seem silly, since we are using the
-- parameter to TZLabel to determine the (source) LocalTime's zone, converting
-- to UTC, then converting to (dest) ZonedTime based on the same zone. Why
-- are we "converting" to the same time zone?
--
-- There may well be a more direct method, but we have to do _something_
-- non-trivial here because the actual TimeZone will vary with the LocalTime
-- (e.g. daylight savings, leap seconds). Thus we first get a zoned time (UTC)
-- with the LocalTime and TZLabel. Then we convert that to the TimeZone of our
-- choice. The returned ZonedTime's LocalTime should be very similar, with the
-- only differences due to the aforementioned issues.
convertFromLabel :: (TZ -> a -> UTCTime) -> a -> TZLabel -> ZonedTime
convertFromLabel toUtcTime t tzLabel = Local.utcToZonedTime timeZone utcTime
  where
    -- This is a TZ i.e. the preliminary timezone corresponding to our
    -- label e.g. America/New_York -> TZ. This type is a stepping stone
    -- to the actual ZonedTime we want.
    tz = All.tzByLabel tzLabel
    -- Convert to UTC. Localtime will use the tz param to derive TimeZone
    -- information. ZonedTime will ignore it as it already carries TimeZone
    -- info.
    utcTime = toUtcTime tz t
    -- Get the final TimeZone from TZ and the UTC time. We need
    -- the time as the TimeZone can vary with the actual time e.g.
    -- America/New_York -> EST / EDT.
    timeZone = Zones.timeZoneForUTCTime tz utcTime

-- These get*Kairos functions are just wrapperes for upstream functions that
-- we wrap in our own exceptions.

getSystemZonedTimeKairos ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  m ZonedTime
getSystemZonedTimeKairos =
  getSystemZonedTime `catchSync` (throwM . MkLocalSystemTimeException)

getTimeZoneKairos ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  UTCTime ->
  m TimeZone
getTimeZoneKairos utc =
  getTimeZone utc `catchSync` (throwM . MkLocalTimeZoneException)

-- | NOTE: [LocalTZException error message]
--
-- The exception here, LocalTZException, has a highly specific message
-- that only applies when we are trying to find the user's local TZ
-- because it was not explicitly given.
--
-- This is fine because this function is only called in the following scenario:
--
-- - User supplies a 'time string' but no --source, hence we need to interpret
--   it in the local timezone.
--
-- - User supplies --date, hence we cannot take the _current_ system timezone,
--   as it may not be correct for the given date. Thus we use time-agnostic
--   loadLocalTZ (tz_database).
--
-- This is not always reliable e.g. windows, so it pays to have a good error
-- message. But note that the error message might need to change if this
-- function was used more widely.
loadLocalTZKairos ::
  ( HasCallStack,
    MonadCatch m,
    MonadEnv m,
    MonadTime m
  ) =>
  m TZ
loadLocalTZKairos = do
  trySync loadLocalTZ >>= \case
    Right tz -> pure tz
    Left ex -> do
      -- FIXME: This is wrong :-(. See
      --
      -- https://learn.microsoft.com/en-us/cpp/c-runtime-library/reference/tzset?view=msvc-170
      --
      -- In particular, it does not appear that windows provides a way to get
      -- the tz_database label? Its TZ labels is totally different, sadly.
      --
      -- Hmm, maybe see https://learn.microsoft.com/en-us/cpp/standard-library/tzdb-struct?view=msvc-170
      Env.lookupEnv "TZ" >>= \case
        Nothing -> throwM $ MkLocalTZException ex
        Just tzStr -> case Internal.tzNameToTZLabel (T.pack tzStr) of
          Nothing -> throwM $ MkLocalTZException ex
          Just tzLabel -> pure $ All.tzByLabel tzLabel
