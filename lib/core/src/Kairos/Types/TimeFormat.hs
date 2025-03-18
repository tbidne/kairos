{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Types.TimeFormat
  ( TimeFormat (..),
    defaultTimeFormats,
    hm,
    hm12h,
    hmTZ,
    hmTZ12h,
    rfc822,
  )
where

import Control.DeepSeq (NFData)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Optics.Core (An_Iso, LabelOptic (labelOptic), iso)

-- | Time formatting string. The 'Monoid' instance behaves like 'Text'.
--
-- @since 0.1
newtype TimeFormat = MkTimeFormat
  {unTimeFormat :: Text}
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
  deriving
    ( -- | @since 0.1
      Monoid,
      -- | @since 0.1
      IsString,
      -- | @since 0.1
      Semigroup
    )
    via Text

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Text, b ~ Text) =>
  LabelOptic "unTimeFormat" k TimeFormat TimeFormat a b
  where
  labelOptic = iso (\(MkTimeFormat f) -> f) MkTimeFormat
  {-# INLINE labelOptic #-}

-- | Parses 24 hour time w/ and w/o colon (13:00, 1300) and 12 hour
-- time w/ and w/o passing (1:30 pm, 1 pm, 10:15 am).
--
-- @since 0.1
defaultTimeFormats :: NonEmpty TimeFormat
defaultTimeFormats =
  hm
    :| [ hmNoColon, -- 0300
         "%-I:%M %P", -- 1:30 pm,
         "%-I:%M%P", -- 1:30pm
         "%-I %P", -- 1pm
         "%-I%P" -- 1 pm
       ]

-- | Format for 24-hour @hours:minutes@.
--
-- @since 0.1
hm :: TimeFormat
hm = "%H:%M"
{-# INLINE hm #-}

-- | Format for 24-hour @hoursminutes@ (no colon).
--
-- @since 0.1
hmNoColon :: TimeFormat
hmNoColon = "%H%M"
{-# INLINE hmNoColon #-}

-- | Format for 12-hour @hours:minutes am/pm@.
--
-- @since 0.1
hm12h :: TimeFormat
hm12h = "%I:%M %P"
{-# INLINE hm12h #-}

-- | Format for 24-hour @hours:minutes TZ@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'Kairos.Types.TimeReader.format'.
--
-- @since 0.1
hmTZ :: TimeFormat
hmTZ = "%H:%M %Z"
{-# INLINE hmTZ #-}

-- | Format for 12-hour @hours:minutes am/pm TZ@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'Kairos.Types.TimeReader.format'.
--
-- @since 0.1
hmTZ12h :: TimeFormat
hmTZ12h = "%I:%M %P %Z"
{-# INLINE hmTZ12h #-}

-- | Format for RFC822: @%a, %_d %b %Y %H:%M:%S %Z@. As this contains a timezone
-- flag, it should be used for formatting output only. In particular, it should
-- __not__ be used with 'Kairos.Types.TimeReader.format'.
--
-- @since 0.1
rfc822 :: TimeFormat
rfc822 = "%a, %_d %b %Y %H:%M:%S %Z"
{-# INLINE rfc822 #-}
