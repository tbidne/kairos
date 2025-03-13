{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | This module provides functions for reading time strings. We also provide
-- functions for converting between timezones.
--
-- @since 0.1
module Data.Time.Conversion
  ( -- * High-level parsing/conversion
    readConvertTime,
    readTime,
    convertTime,

    -- * Low-level functions

    -- ** Parsing time strings
    readInLocalTimeZone,
    readTimeFormatLocal,
    readTimeFormatZoned,
    readTimeFormat,

    -- ** Converting ZonedTime
    convertZoned,

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
    LocalSystemTimeException (..),
  )
where

import Control.Exception.Utils (catchSync)
import Control.Monad.Catch
  ( MonadCatch,
    MonadThrow,
    throwM,
  )
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Conversion.Types.Date
  ( Date (DateLiteral, DateToday),
    unDateString,
  )
import Data.Time.Conversion.Types.Exception
  ( LocalSystemTimeException (MkLocalSystemTimeException),
    LocalTimeZoneException (MkLocalTimeZoneException),
    ParseTZInputException (MkParseTZInputException),
    ParseTimeException (MkParseTimeException),
  )
import Data.Time.Conversion.Types.TZInput (TZInput (TZActual, TZDatabase))
import Data.Time.Conversion.Types.TimeFormat
  ( TimeFormat (MkTimeFormat, unTimeFormat),
  )
import Data.Time.Conversion.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        format,
        srcTZ,
        timeString
      ),
  )
import Data.Time.Format (ParseTime, TimeLocale)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime
  ( LocalTime,
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
import Effects.Time (MonadTime (getSystemZonedTime))
import GHC.Stack (HasCallStack)
import Optics.Core ((^.))

-- $setup
-- >>> import Control.Exception (SomeException)
-- >>> import Data.Functor (void)
-- >>> import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
-- >>> import Data.Time.Conversion.Types.TimeReader (defaultTimeReader)
-- >>> import Effects.Exception (catchCS)
-- >>> let parseTimeEx = \(e :: ParseTimeException) -> putStrLn "parse time exception"
-- >>> let parseTzDbEx = \(e :: ParseTZDatabaseException) -> putStrLn "parse tzdb exception"

-- | Reads the given time string based on the source 'TimeReader' and
-- converts to the destination timezone. This is the composition of
-- 'readTime' and 'convertTime'. If the source is 'Nothing' then we read
-- the local system time. Similarly, if the dest is 'Nothing', we convert
-- to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- ==== __Examples__
--
-- >>> let toUtc = Just $ TZDatabaseLabel Etc__UTC
-- >>> let reader = defaultTimeReader "17:24"
--
-- >>> -- convert from est
-- >>> let nyReader = reader { srcTZ = Just (TZDatabaseLabel America__New_York) }
-- >>> readConvertTime (Just (nyReader { timeString = "08:15" })) toUtc
-- 1970-01-01 13:15:00 UTC
--
-- >>> let badTimeString = reader { timeString = "bad" }
-- >>> (void $ readConvertTime (Just badTimeString) toUtc) `catchCS` parseTimeEx
-- parse time exception
--
-- >>> let badTZDatabase = reader { srcTZ = Just (TZDatabaseText "America/NewYork")}
-- >>> (void $ readConvertTime (Just badTZDatabase) toUtc) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
readConvertTime ::
  (HasCallStack, MonadCatch m, MonadTime m) =>
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
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
-- * 'LocalSystemTimeException': Error retrieving local system time.
--
-- ==== __Examples__
-- >>> let reader = defaultTimeReader "17:24"
--
-- >>> -- convert from est
-- >>> readTime (Just (reader { srcTZ = Just (TZDatabaseLabel America__New_York) }))
-- 1970-01-01 17:24:00 EST
--
-- >>> let nyReader = reader { srcTZ = Just (TZDatabaseLabel America__New_York) }
-- >>> readTime (Just (nyReader { timeString = "08:15" }))
-- 1970-01-01 08:15:00 EST
--
-- >>> let badTimeString = reader { timeString = "bad" }
-- >>> (void $ readTime (Just badTimeString)) `catchCS` parseTimeEx
-- parse time exception
--
-- >>> let badTZDatabase = reader { srcTZ = Just (TZDatabaseText "America/NewYork")}
-- >>> (void $ readTime (Just badTZDatabase)) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
readTime ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  Maybe TimeReader ->
  m ZonedTime
readTime (Just timeReader) = readTimeString timeReader
readTime Nothing =
  getSystemZonedTime `catchSync` (throwM . MkLocalSystemTimeException)

-- | Converts the given time to the destination timezone. If no destination
-- timezone is given then we convert to the local system timezone.
--
-- __Throws:__
--
-- * 'ParseTZDatabaseException': Error parsing the tz database name.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- ==== __Examples__
--
-- >>> import Data.Time.Clock (UTCTime (..))
-- >>> import Data.Time.LocalTime (utc, utcToZonedTime)
-- >>> let zoned = utcToZonedTime utc (UTCTime (toEnum 50_000) 0)
-- >>> zoned
-- 1995-10-10 00:00:00 UTC
--
-- >>> let toNy = Just $ TZDatabaseText "America/New_York"
-- >>> convertTime zoned toNy
-- 1995-10-09 20:00:00 EDT
--
-- >>> (void $ convertTime zoned (Just $ TZDatabaseText "America/NewYork")) `catchCS` parseTzDbEx
-- parse tzdb exception
--
-- @since 0.1
convertTime ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  ZonedTime ->
  Maybe TZInput ->
  m ZonedTime
convertTime inTime Nothing = do
  let inTimeUtc = Local.zonedTimeToUTC inTime
  currTZ <-
    getCurrentTimeZone
      `catchSync` (throwM . MkLocalTimeZoneException)
  pure $ Local.utcToZonedTime currTZ inTimeUtc
convertTime inTime (Just tzOut) = pure $ convertZoned inTime tzOut

readTimeString ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  TimeReader ->
  m ZonedTime
readTimeString timeReader =
  case timeReader ^. #srcTZ of
    -- read in local timezone
    Nothing -> do
      -- add system date if specified
      (timeStrDate, formatDate) <- maybeAddDate Nothing
      readInLocalTimeZone formatDate timeStrDate
    Just tzInput -> do
      -- add src date if specified
      (timeStrDate, formatDate) <- maybeAddDate (Just tzInput)

      -- Read string as a LocalTime, no TZ info. This allow us to correctly
      -- get the source's timezone, taking the desired date into account.
      localTime <-
        maybe
          (throwParseEx formatDate timeStrDate)
          pure
          (readTimeFormatLocal Format.defaultTimeLocale formatDate timeStrDate)

      pure $ convertLocalToZoned localTime tzInput
  where
    format = timeReader ^. #format
    timeStr = timeReader ^. #timeString

    throwParseEx :: (HasCallStack, MonadThrow m) => TimeFormat -> Text -> m void
    throwParseEx f = throwM . MkParseTimeException f

    maybeAddDate ::
      ( HasCallStack,
        MonadCatch m,
        MonadTime m
      ) =>
      -- Maybe source timezone
      Maybe TZInput ->
      m (Text, TimeFormat)
    maybeAddDate mTZ = case timeReader ^. #date of
      Nothing -> pure (timeStr, format)
      Just (DateLiteral dateStr) -> do
        let str = unDateString dateStr
        pure (str +-+ timeStr, dateString +-+ format)
      Just DateToday -> do
        -- get the current date in the source timezone
        currDateStr <- currentDate mTZ
        pure (T.pack currDateStr +-+ timeStr, dateString +-+ format)

currentDate ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  Maybe TZInput ->
  m String
currentDate mTZ = do
  currTime <-
    getSystemZonedTime
      `catchSync` (throwM . MkLocalSystemTimeException)

  -- Convert into the given label if present. Otherwise keep in system
  -- timezone.
  let currTime' = maybe currTime (convertZoned currTime) mTZ

  pure $ Format.formatTime Format.defaultTimeLocale dateString currTime'

dateString :: (IsString s) => s
dateString = "%Y-%m-%d"

tzString :: (IsString s) => s
tzString = "%z"

-- | @readInLocalTimeZone locale format timeStr@ attempts to parse the
-- @timeStr@ given the expected @format@. We parse into the current
-- system timezone, so:
--
-- * @format@ should __not__ mention "%Z"
-- * @timeStr@ should __not__ contain timezone information.
--
-- @
-- λ. readInLocalTimeZone "%H" "17"
-- Just 1970-01-01 17:00:00 NZST
-- @
--
-- __Throws:__
--
-- * 'ParseTimeException': Error parsing the time string.
-- * 'LocalTimeZoneException': Error retrieving local timezone.
--
-- @since 0.1
readInLocalTimeZone ::
  ( HasCallStack,
    MonadCatch m,
    MonadTime m
  ) =>
  TimeFormat ->
  Text ->
  m ZonedTime
readInLocalTimeZone format timeStr = do
  localTz <-
    getCurrentTimeZone
      `catchSync` (throwM . MkLocalTimeZoneException)
  let tzStr = T.pack $ Local.timeZoneOffsetString localTz

      -- Add the local tz string to the time string, and the tz flag to the format
      timeStr' = timeStr +-+ tzStr
  case readTimeFormatZoned Format.defaultTimeLocale format' timeStr' of
    Just zt -> pure zt
    Nothing -> throwM $ MkParseTimeException format' timeStr'
  where
    format' = format +-+ tzString

-- | 'readTimeFormat' specialized to 'ZonedTime'.
--
-- ==== __Examples__
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hm "17:24"
-- Just 1970-01-01 17:24:00 +0000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hm12h "07:24 pm"
-- Just 1970-01-01 19:24:00 +0000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hmTZ "07:24 +5000"
-- Just 1970-01-01 07:24:00 +5000
--
-- >>> readTimeFormatZoned Format.defaultTimeLocale TimeFmt.hmTZ12h "07:24 pm -5000"
-- Just 1970-01-01 19:24:00 -5000
--
-- @since 0.1
readTimeFormatZoned :: TimeLocale -> TimeFormat -> Text -> Maybe ZonedTime
readTimeFormatZoned = readTimeFormat

-- | 'readTimeFormat' specialized to 'LocalTime'.
--
-- @since 0.1
readTimeFormatLocal :: TimeLocale -> TimeFormat -> Text -> Maybe LocalTime
readTimeFormatLocal = readTimeFormat

-- | @readTimeFormat locale format timeStr@ attempts to parse the @timeStr@ given
-- the expected @format@. No timezone is assumed, so if it is left off then
-- the result is UTC.
--
-- @since 0.1
readTimeFormat :: (ParseTime t) => TimeLocale -> TimeFormat -> Text -> Maybe t
readTimeFormat locale format timeStr = Format.parseTimeM True locale format' timeStr'
  where
    format' = T.unpack $ format ^. #unTimeFormat
    timeStr' = T.unpack timeStr

-- | Converts a local time to the given timezone.
--
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale TimeFmt.hm "18:00"
-- >>> convertLocalLabel sixPmUtc America__New_York
-- 1970-01-01 18:00:00 EST
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
-- ==== __Examples__
-- >>> let (Just sixPmUtc) = readTimeFormat Format.defaultTimeLocale TimeFmt.hm "18:00"
-- >>> convertZonedLabel sixPmUtc America__New_York
-- 1970-01-01 13:00:00 EST
--
-- @since 0.1
convertZoned :: ZonedTime -> TZInput -> ZonedTime
convertZoned zonedTime tzInput =
  case tzInput of
    TZDatabase label -> convertFromLabel f zonedTime label
    TZActual timeZone -> convertFromActual zonedTime timeZone
  where
    f = const Local.zonedTimeToUTC

-- | Converts the ZonedTime to UTCTime and finally the requested TimeZone.
--
-- The parameter 'TimeZone' is the destination.
convertFromActual :: ZonedTime -> TimeZone -> ZonedTime
convertFromActual zt timeZone = Local.utcToZonedTime timeZone utcTime
  where
    -- Convert to UTC.
    utcTime = Local.zonedTimeToUTC zt

-- | Converts some time ('LocalTime' or 'ZonedTime') to 'UTCTime' then finally
-- a ZonedTime based on the given 'TZLabel'.
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

-- concat with a space
(+-+) :: (Semigroup a, IsString a) => a -> a -> a
xs +-+ ys = xs <> " " <> ys

infixr 5 +-+

getCurrentTimeZone :: (HasCallStack, MonadTime m) => m TimeZone
getCurrentTimeZone = do
  ZonedTime _ tz <- getSystemZonedTime
  pure tz
