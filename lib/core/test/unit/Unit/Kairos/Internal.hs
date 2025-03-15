{-# LANGUAGE CPP #-}

module Unit.Kairos.Internal (tests) where

import Control.DeepSeq (force)
import Control.Exception
  ( evaluate,
  )
import Control.Monad (void)
import Data.Text.Encoding.Error qualified as TError
import Hedgehog
  ( Property,
    PropertyName,
    failure,
    forAll,
    property,
  )
import Kairos.Internal qualified as Internal
import Props.Generators qualified as G
import Test.Tasty (TestName, TestTree, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests =
  testGroup
    "Kairos.Internal"
    [ tzTests
    ]

tzTests :: TestTree
tzTests =
  testGroup
    "TZ database tests"
    [ tzNameMapParses,
      tzNameRandomCase
    ]

tzNameMapParses :: TestTree
tzNameMapParses =
  testCase "tzNameMap parses without errors" $
    void $
      evaluate (force mp)
  where
    mp = Internal.tzLowerNameLabelMapWith TError.strictDecode

tzNameRandomCase :: TestTree
tzNameRandomCase =
  testPropertyCompat "tzNameToTZLabel handles random case" "tzNameRandomCase" $
    property $ do
      txt <- forAll G.tzText
      case Internal.tzNameToTZLabel txt of
        Just _ -> pure ()
        Nothing -> failure

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif
