module Unit.Utils
  ( -- * MonadFail (Either String)
    EString (..),
    unEString,

    -- * Misc
    runParseDate,
    runParseDateString,
  )
where

import Data.Text (Text)
import Kairos.Types.Date (Date, DateString)
import Kairos.Types.Date qualified as Date

-- | Pure 'Date.parseDate' that returns a string error via 'EString'.
runParseDate :: Text -> Either String Date
runParseDate = unEString . Date.parseDate

-- | Pure 'Date.parseDateString' that returns a string error via 'EString'.
runParseDateString :: Text -> Either String DateString
runParseDateString = unEString . Date.parseDateString

-- | Either fixed to 'Either String' for the purpose of its 'MonadFail'
-- instance.
newtype EString a = MkEString (Either String a)
  deriving stock (Eq, Show)
  deriving (Applicative, Functor, Monad) via (Either String)

instance MonadFail EString where
  fail = MkEString . Left

unEString :: EString a -> Either String a
unEString (MkEString x) = x
