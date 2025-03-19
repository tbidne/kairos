{-# LANGUAGE CPP #-}

module Unit.Utils
  ( -- * MonadFail (Either String)
    EString (..),
    unEString,

    -- * Misc
    runParseDate,
    testProp,
    testPropertyCompat,
  )
where

import Data.Text (Text)
import Hedgehog (Property, PropertyName, PropertyT, property)
import Kairos.Types.Date (Date)
import Kairos.Types.Date qualified as Date
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog qualified as THH

-- | Pure 'Date.parseDate' that returns a string error via 'EString'.
runParseDate :: Text -> Either String Date
runParseDate = unEString . Date.parseDateString

-- | Either fixed to 'Either String' for the purpose of its 'MonadFail'
-- instance.
newtype EString a = MkEString (Either String a)
  deriving stock (Eq, Show)
  deriving (Applicative, Functor, Monad) via (Either String)

instance MonadFail EString where
  fail = MkEString . Left

unEString :: EString a -> Either String a
unEString (MkEString x) = x

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = THH.testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = THH.testProperty tn
#endif

testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp tn pn = testPropertyCompat tn pn . property
