module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Unit.Kairos qualified
import Unit.Kairos.Internal qualified
import Unit.Kairos.Types.Date qualified
import Unit.Kairos.Types.TZInput qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit tests"
      [ Unit.Kairos.tests,
        Unit.Kairos.Internal.tests,
        Unit.Kairos.Types.Date.tests,
        Unit.Kairos.Types.TZInput.tests
      ]
