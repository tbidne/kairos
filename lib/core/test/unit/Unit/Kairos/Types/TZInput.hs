module Unit.Kairos.Types.TZInput (tests) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Time.LocalTime (TimeZone (TimeZone))
import Data.Time.Zones.All
  ( TZLabel
      ( America__New_York,
        Etc__UTC,
        Pacific__Auckland,
        UTC
      ),
  )
import Data.Time.Zones.All qualified as All
import Hedgehog ((===))
import Hedgehog qualified as H
import Kairos.Types.TZInput (TZInput (TZActual, TZDatabase))
import Kairos.Types.TZInput qualified as TZInput
import Props.Generators qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Kairos.Types.TZInput"
    [ testTZLabelRoundtrips,
      testParseTZLabelCases,
      testParseTZLabelCasesFail,
      testParseTZOffsetProps,
      testParseTZOffsetCases,
      testParseTZOffsetCasesFail
    ]

testTZLabelRoundtrips :: TestTree
testTZLabelRoundtrips = Utils.testProp desc "testTZLabelRoundtrips" $ do
  lbl <- H.forAll G.tzLabel

  let name = All.toTZName lbl
      txt = TEnc.decodeUtf8 name
      mResult = TZInput.parseTZInput txt

  H.annotateShow name
  H.annotate (T.unpack txt)

  case mResult of
    Nothing -> H.failure
    Just (TZActual (TimeZone m s n)) -> do
      H.annotateShow m
      H.annotateShow s
      H.annotate n
      H.failure
    Just (TZDatabase lblResult) -> lbl === lblResult
  where
    desc = "parseTZInput . toTZName roundtrips"

testParseTZLabelCases :: TestTree
testParseTZLabelCases = testCase "Parses tz_database labels" $ do
  Just (TZDatabase America__New_York) @=? TZInput.parseTZInput "america/new_york"
  Just (TZDatabase America__New_York) @=? TZInput.parseTZInput "aMeRiCa/NeW_YoRk"
  Just (TZDatabase Etc__UTC) @=? TZInput.parseTZInput "etc/utc"
  Just (TZDatabase UTC) @=? TZInput.parseTZInput "utc"
  Just (TZDatabase Pacific__Auckland) @=? TZInput.parseTZInput "Pacific/Auckland"

testParseTZLabelCasesFail :: TestTree
testParseTZLabelCasesFail = testCase "Parses tz_database label failures" $ do
  Nothing @=? TZInput.parseTZInput "america"
  Nothing @=? TZInput.parseTZInput "America/New"
  Nothing @=? TZInput.parseTZInput "Pacific"

testParseTZOffsetProps :: TestTree
testParseTZOffsetProps = Utils.testProp desc "testParseTZOffsetProps" $ do
  offset <- H.forAll G.offsetTxt
  case TZInput.parseTZInput offset of
    Nothing -> H.failure
    Just (TZActual _) -> pure ()
    Just (TZDatabase lblResult) -> do
      H.annotateShow lblResult
      H.failure
  where
    desc = "Parses generated timezone offsets"

testParseTZOffsetCases :: TestTree
testParseTZOffsetCases = testCase "Parses timezone offsets" $ do
  Just (TZActual e1) `compMTZ` TZInput.parseTZInput "+0300"
  Just (TZActual e1) `compMTZ` TZInput.parseTZInput "+03"
  Just (TZActual e1) `compMTZ` TZInput.parseTZInput "+03:00"
  Just (TZActual e2) `compMTZ` TZInput.parseTZInput "Z"
  Just (TZActual e3) `compMTZ` TZInput.parseTZInput "-1700"
  Just (TZActual e3) `compMTZ` TZInput.parseTZInput "-17"
  Just (TZActual e3) `compMTZ` TZInput.parseTZInput "-17:00"
  where
    e1 = TimeZone 180 False ""
    e2 = TimeZone 0 False "UTC"
    e3 = TimeZone (-1020) False ""

    compMTZ (Just (TZActual tz1)) (Just (TZActual tz2)) = compTZ tz1 tz2
    compMTZ x1 x2 = x1 @=? x2

    compTZ (TimeZone min1 summer1 name1) (TimeZone min2 summer2 name2) = do
      min1 @=? min2
      summer1 @=? summer2
      name1 @=? name2

testParseTZOffsetCasesFail :: TestTree
testParseTZOffsetCasesFail = testCase "Parses timezone offset failures" $ do
  Nothing @=? TZInput.parseTZInput "13"
  Nothing @=? TZInput.parseTZInput "+1"
  Nothing @=? TZInput.parseTZInput "-1"
  Nothing @=? TZInput.parseTZInput "+1a"
