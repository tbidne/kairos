{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Types.TimeReader
  ( TimeReader (..),
    defaultTimeReader,
  )
where

import Control.DeepSeq (NFData (rnf), deepseq)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kairos.Types.Date (Date)
import Kairos.Types.TZInput (TZInput)
import Kairos.Types.TimeFormat (TimeFormat, defaultTimeFormats)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)

-- | Determines how to read a time string.
--
-- @since 0.1
data TimeReader = MkTimeReader
  { -- | Format(s) used when parsing the time string. This should __not__
    -- include timezone formatting e.g. @%Z@. Use 'srcTZ' instead. It should
    -- also not include date information. Use 'date' instead.
    --
    -- The formats are tried in order.
    --
    -- @since 0.1
    formats :: NonEmpty TimeFormat,
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
  (k ~ A_Lens, a ~ NonEmpty TimeFormat, b ~ NonEmpty TimeFormat) =>
  LabelOptic "formats" k TimeReader TimeReader a b
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
-- @since 0.1
defaultTimeReader :: Text -> TimeReader
defaultTimeReader = MkTimeReader defaultTimeFormats Nothing Nothing
