{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion.Types.Date (tests) where

import Control.Monad ((>=>))
import Data.Bifunctor (bimap)
import Data.Time.Conversion.Types.Date (Date (DateToday), _DateLiteral)
import Data.Time.Conversion.Types.Date.Internal qualified as Date
import Optics.Core (matching)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion.Types.Date"
    [ testParseDateTodaySuccess,
      testParseDateLiteralSuccess,
      testParseDateLiteralBadYear,
      testParseDateLiteralBadMonth,
      testParseDateLiteralBadDay,
      testParseDateLiteralBadFormat
    ]

testParseDateTodaySuccess :: TestTree
testParseDateTodaySuccess = testCase "parseDateString today success" $ do
  Right DateToday @=? Utils.runParseDate "today"

testParseDateLiteralSuccess :: TestTree
testParseDateLiteralSuccess = testCase "parseDateString literal success" $ do
  Right "2022-03-12" @=? runParseDate "2022-03-12"
  Right "2010-01-31" @=? runParseDate "2010-01-31"
  Right "1900-02-29" @=? runParseDate "1900-02-29"
  Right "2099-08-25" @=? runParseDate "2099-08-25"
  where
    runParseDate = Utils.runParseDate >=> toDateStr
    toDateStr =
      bimap (const "Could not match _DateLiteral") Date.unDateString
        . matching _DateLiteral

testParseDateLiteralBadYear :: TestTree
testParseDateLiteralBadYear = testCase "parseDateString bad year fails" $ do
  Left "Year should be an integer between 1900 and 3000, received '1899'" @=? Utils.runParseDate "1899-03-12"
  Left "Year should be an integer between 1900 and 3000, received '3001'" @=? Utils.runParseDate "3001-03-12"

testParseDateLiteralBadMonth :: TestTree
testParseDateLiteralBadMonth = testCase "parseDateString bad month fails" $ do
  Left "Month should be an integer between 1 and 12, received '00'" @=? Utils.runParseDate "2000-00-12"
  Left "Month should be an integer between 1 and 12, received '13'" @=? Utils.runParseDate "2000-13-12"
  Left "Month should be an integer between 1 and 12, received 'x'" @=? Utils.runParseDate "2000-x-12"

testParseDateLiteralBadDay :: TestTree
testParseDateLiteralBadDay = testCase "parseDateString bad day fails" $ do
  Left "Day should be an integer between 1 and 31, received '00'" @=? Utils.runParseDate "2000-05-00"
  Left "Day should be an integer between 1 and 31, received '32'" @=? Utils.runParseDate "2000-05-32"
  Left "Day should be an integer between 1 and 31, received 'y'" @=? Utils.runParseDate "2000-05-y"

testParseDateLiteralBadFormat :: TestTree
testParseDateLiteralBadFormat = testCase "parseDateString bad format fails" $ do
  Left "Date has the form YYYY-MM-DD, received '-05-00'" @=? Utils.runParseDate "-05-00"
  Left "Date has the form YYYY-MM-DD, received '2000-05-'" @=? Utils.runParseDate "2000-05-"
  Left "Date has the form YYYY-MM-DD, received '7'" @=? Utils.runParseDate "7"
  Left "Date has the form YYYY-MM-DD, received 'cat'" @=? Utils.runParseDate "cat"
  Left "Date has the form YYYY-MM-DD, received ''" @=? Utils.runParseDate ""
