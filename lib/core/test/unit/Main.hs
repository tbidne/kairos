module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Kairos qualified as C
import Unit.Kairos.Internal qualified as CInternal
import Unit.Kairos.Types.Date qualified as CTypes.Date

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ C.tests,
        CInternal.tests,
        CTypes.Date.tests
      ]
