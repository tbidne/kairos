-- | @since 0.1
module Kairos.Types.TZInput
  ( TZInput (..),
    parseTZInput,
    locale,
    _TZDatabase,
    _TZActual,
  )
where

import Control.Applicative (asum)
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (TimeZone, parseTimeM)
import Data.Time.Format (TimeLocale)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime qualified as LT
import Data.Time.Zones.All (TZLabel)
import GHC.Generics (Generic)
import Kairos.Internal qualified as Internal
import Optics.Core (Prism', prism)

-- | Timezone input.
data TZInput
  = -- | TZ database label like America/New_York.
    TZDatabase TZLabel
  | -- | Actual timezone.
    TZActual TimeZone
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | Attempts to parse 'TZInput'.
--
-- @since 0.1
parseTZInput :: Text -> Maybe TZInput
parseTZInput txt = asum parsers
  where
    parsers =
      ($ txt)
        <$> [ parseTZLabel,
              parseTZOffset "%z",
              parseTZOffset "%:z",
              parseTZOffsetH,
              utcAbbrev
            ]

parseTZLabel :: Text -> Maybe TZInput
parseTZLabel = fmap TZDatabase . Internal.tzNameToLabel

parseTZOffset :: String -> Text -> Maybe TZInput
parseTZOffset fmt = fmap TZActual . parseTimeM False locale fmt . T.unpack

parseTZOffsetH :: Text -> Maybe TZInput
parseTZOffsetH txt =
  -- +/- and HH e.g. +13
  if T.length txt == 3
    then parseTZOffset "%z" (txt <> "00")
    else Nothing

utcAbbrev :: Text -> Maybe TZInput
utcAbbrev "Z" = Just $ TZActual LT.utc
utcAbbrev _ = Nothing

-- | Default locale.
--
-- @since 0.1
locale :: TimeLocale
locale = Format.defaultTimeLocale

-- | @since 0.1
_TZDatabase :: Prism' TZInput TZLabel
_TZDatabase =
  prism
    TZDatabase
    ( \case
        TZDatabase lbl -> Right lbl
        other -> Left other
    )
{-# INLINE _TZDatabase #-}

-- | @since 0.1
_TZActual :: Prism' TZInput TimeZone
_TZActual =
  prism
    TZActual
    ( \case
        TZActual t -> Right t
        other -> Left other
    )
{-# INLINE _TZActual #-}
