{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Types.Date
  ( -- * Date
    Date (..),
    parseDate,
    _DateToday,
    _DateLiteral,

    -- * Date String
    Internal.DateString,

    -- ** Creation
    Internal.parseDateString,

    -- ** Elimination
    Internal.unDateString,
    Internal.year,
    Internal.month,
    Internal.day,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)
import Kairos.Types.Date.Internal (DateString)
import Kairos.Types.Date.Internal qualified as Internal
import Optics.Core (Prism', prism)

-- | Date to use when reading a time string.
--
-- @since 0.1
data Date
  = -- | Corresponds to today.
    --
    -- @since 0.1
    DateToday
  | -- | Manual date string.
    --
    -- @since 0.1
    DateLiteral DateString
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

-- | Parses a date.
--
-- @since 0.1
parseDate :: (MonadFail f) => Text -> f Date
parseDate "today" = pure DateToday
parseDate txt = DateLiteral <$> Internal.parseDateString txt

-- | @since 0.1
_DateToday :: Prism' Date ()
_DateToday =
  prism
    (const DateToday)
    ( \case
        DateToday -> Right ()
        other -> Left other
    )
{-# INLINE _DateToday #-}

-- | @since 0.1
_DateLiteral :: Prism' Date DateString
_DateLiteral =
  prism
    DateLiteral
    ( \case
        DateLiteral s -> Right s
        other -> Left other
    )
{-# INLINE _DateLiteral #-}
