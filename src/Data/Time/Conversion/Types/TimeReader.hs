{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Data.Time.Conversion.Types.TimeReader
  ( TimeReader (..),
    defaultTimeReader,
  )
where

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.Text (Text)
import Data.Time.Conversion.Types.Date (Date)
import Data.Time.Conversion.Types.TZInput (TZInput)
import Data.Time.Conversion.Types.TimeFormat (TimeFormat, defaultTimeFormat)
import GHC.Generics (Generic)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- | Determines how to read a time string.
--
-- @since 0.1
data TimeReader = MkTimeReader
  { -- | Format used when parsing the time string. This should __not__ include
    -- timezone formatting e.g. @%Z@. Use 'srcTZ' instead. It should also
    -- not include date information. Use 'date' instead.
    --
    -- @since 0.1
    format :: TimeFormat,
    -- | Timezone in which to read the string. 'Nothing' corresponds to
    -- local timezone.
    --
    -- @since 0.1
    srcTZ :: Maybe TZInput,
    -- | Date corresponding to the 'timeString'. If 'Nothing', uses the
    -- unix epoch.
    --
    -- @since 0.1
    date :: Maybe Date,
    -- | The time string to parse. This should __not__ include a timezone
    -- e.g. EST. Use 'srcTZ' instead.
    --
    -- @since 0.1
    timeString :: Text
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance NFData TimeReader where
  rnf (MkTimeReader f s td ts) =
    f `deepseq` s `deepseq` td `deepseq` ts `deepseq` ()

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ TimeFormat, b ~ TimeFormat) =>
  LabelOptic "format" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (\format' -> MkTimeReader format' _srcTZ _today _timeString) (f _format)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe TZInput, b ~ Maybe TZInput) =>
  LabelOptic "srcTZ" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (\srcTZ' -> MkTimeReader _format srcTZ' _today _timeString) (f _srcTZ)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Date, b ~ Maybe Date) =>
  LabelOptic "date" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _date _timeString) ->
    fmap (\date' -> MkTimeReader _format _srcTZ date' _timeString) (f _date)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Text, b ~ Text) =>
  LabelOptic "timeString" k TimeReader TimeReader a b
  where
  labelOptic = lensVL $ \f (MkTimeReader _format _srcTZ _today _timeString) ->
    fmap (MkTimeReader _format _srcTZ _today) (f _timeString)
  {-# INLINE labelOptic #-}

-- | Given a time string, returns a default time reader.
--
-- ==== __Examples__
--
-- >>> defaultTimeReader "08:30"
-- MkTimeReader {format = MkTimeFormat {unTimeFormat = "%H:%M"}, srcTZ = Nothing, date = Nothing, timeString = "08:30"}
--
-- @since 0.1
defaultTimeReader :: Text -> TimeReader
defaultTimeReader = MkTimeReader defaultTimeFormat Nothing Nothing
