-- | @since 0.1
module Kairos.Types.Date
  ( -- * Date
    Internal.Date (MkDate, MkDateString),

    -- * Creation
    Internal.parseDateString,

    -- * Elimination
    Internal.unDateString,
    Internal.unDate,
    Internal.year,
    Internal.month,
    Internal.day,

    -- * Optics
    _DateString,
  )
where

import Data.Text (Text)
import Kairos.Types.Date.Internal (Date (MkDate, MkDateString))
import Kairos.Types.Date.Internal qualified as Internal
import Optics.Core (ReversedPrism', prism, re)

-- | @since 0.1
_DateString :: ReversedPrism' Date Text
_DateString = re $ prism Internal.unDateString setter
  where
    setter t = case Internal.parseDateString t of
      Nothing -> Left t
      Just d -> Right d
{-# INLINE _DateString #-}
