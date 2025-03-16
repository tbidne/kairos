-- | @since 0.1
module Kairos.Types.Exception
  ( ParseTimeException (..),
    ParseTZInputException (..),
    LocalTimeZoneException (..),
    LocalSystemTimeException (..),
    SrcTZNoTimeStringException (..),
    DateNoTimeStringException (..),
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (displayException))
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Kairos.Types.TimeFormat (TimeFormat (unTimeFormat))

-- | Exception parsing time string.
--
-- @since 0.1
data ParseTimeException = MkParseTimeException (NonEmpty TimeFormat) Text
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception ParseTimeException where
  displayException (MkParseTimeException fmts t) =
    mconcat
      [ "Could not parse time string '",
        T.unpack t,
        "' with format(s): ",
        fmtStrs
      ]
    where
      fmtStrs = foldMap (T.unpack . ("\n - " <>) . (.unTimeFormat)) fmts

-- | Exception parsing tz input names.
--
-- @since 0.1
newtype ParseTZInputException = MkParseTZInputException Text
  deriving stock
    ( -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance Exception ParseTZInputException where
  displayException (MkParseTZInputException tzdb) =
    "Could not parse tz from '"
      <> T.unpack tzdb
      <> "'. Wanted a name or offset e.g. 'America/New_York', '+0800'."

-- | Exception reading local system timezone.
--
-- @since 0.1
data LocalTimeZoneException
  = forall e. (Exception e) => MkLocalTimeZoneException e

-- | @since 0.1
deriving stock instance Show LocalTimeZoneException

-- | @since 0.1
instance Exception LocalTimeZoneException where
  displayException (MkLocalTimeZoneException e) =
    "Local timezone exception: " <> displayException e

-- | Exception reading local system time.
--
-- @since 0.1
data LocalSystemTimeException
  = forall e. (Exception e) => MkLocalSystemTimeException e

-- | @since 0.1
deriving stock instance Show LocalSystemTimeException

-- | @since 0.1
instance Exception LocalSystemTimeException where
  displayException (MkLocalSystemTimeException e) =
    "Local system time exception: " <> displayException e

-- | Exception for when --src-tz is specified but time string is not.
--
-- @since 0.1
data SrcTZNoTimeStringException = MkSrcTZNoTimeStringException
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception SrcTZNoTimeStringException where
  displayException MkSrcTZNoTimeStringException =
    "The --src-tz option was specified without required time string"

-- | Exception for when --date specified but time string is not.
--
-- @since 0.1
data DateNoTimeStringException = MkDateNoTimeStringException
  deriving stock
    ( -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Exception DateNoTimeStringException where
  displayException MkDateNoTimeStringException =
    "The --date option was specified without required time string"
