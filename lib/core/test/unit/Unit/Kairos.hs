{-# LANGUAGE CPP #-}

module Unit.Kairos (tests) where

import Control.Exception.Utils (catchSync)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (ZonedTime (ZonedTime))
import Data.Time.LocalTime qualified as Time
import Hedgehog (Property, PropertyName)
import Hedgehog qualified as H
import Hedgehog.Internal.Property ((===))
import Kairos qualified
import Kairos.Types.Date (Date (DateLiteral, DateToday))
import Kairos.Types.TZInput (TZInput (TZDatabase))
import Kairos.Types.TimeFormat qualified as TimeFmt
import Kairos.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        formats,
        srcTZ,
        timeString
      ),
  )
import Props.Generators qualified as G
import Test.Tasty (TestName, TestTree, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Kairos"
    [ testDestSrcRoundtrips,
      testDestSrcDateRoundtrips,
      defaultFormatTests
    ]

testDestSrcRoundtrips :: TestTree
testDestSrcRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date today)" "testDestSrcRoundtrips" $
    H.property $ do
      tzdb <- TZDatabase <$> H.forAll G.tzLabel

      currTime <- liftIO $ Kairos.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Kairos.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTimeDest
      let currTimeDestStr = fmt currTimeDest
          timeReader =
            MkTimeReader
              { formats = NE.singleton TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just DateToday,
                timeString = T.pack currTimeDestStr
              }

      currTime' <-
        liftIO (Kairos.readConvertTime (Just timeReader) Nothing)
          `catchSync` \ex -> do
            H.annotateShow ex
            H.failure
      H.annotateShow currTime'

      compareTime fmtOut currTime currTime'
  where
    fmt = Format.formatTime locale fmtOut
    fmtOut = "%H:%M"
    locale = Format.defaultTimeLocale

testDestSrcDateRoundtrips :: TestTree
testDestSrcDateRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date literal)" "testDestSrcDateRoundtrips" $
    H.property $ do
      tzdb <- TZDatabase <$> H.forAll G.tzLabel

      currTime <- liftIO $ Kairos.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Kairos.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTime
      (currDateDestStr, currTimeDestStr) <-
        case T.split (== ' ') (T.pack $ fmt currTimeDest) of
          [y, d] -> pure (y, d)
          _ -> do
            let err =
                  mconcat
                    [ "Unit.Kairos: date should have format ",
                      "YYYY-MM-DD HH:MM, received: '",
                      fmt currTimeDest,
                      "'"
                    ]
            H.annotate err
            H.failure
      H.annotate $ T.unpack currDateDestStr
      H.annotate $ T.unpack currTimeDestStr
      currDateDestStr' <- case Utils.runParseDateString currDateDestStr of
        Right s -> pure s
        Left err -> do
          H.annotate err
          H.failure

      H.annotateShow currDateDestStr'
      let timeReader =
            MkTimeReader
              { formats = NE.singleton TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just $ DateLiteral currDateDestStr',
                timeString = currTimeDestStr
              }

      currTime' <-
        liftIO (Kairos.readConvertTime (Just timeReader) Nothing)
          `catchSync` \ex -> do
            H.annotateShow ex
            H.failure

      H.annotateShow currTime'

      compareTime fmtOut currTime currTime'
  where
    fmt = Format.formatTime locale fmtOut
    fmtOut = "%Y-%m-%d %H:%M"
    locale = Format.defaultTimeLocale

compareTime :: String -> ZonedTime -> ZonedTime -> H.PropertyT IO ()
compareTime fmtOut currTime currTime' = do
  -- Normally these two are equal, but unfortunately we can have
  -- currTime + 1 == currTime' because the second readConvertTime crosses
  -- the minute mark. In these cases we need to relax the test.
  let exactEq = fmt currTime == fmt currTime'
  -- FIXME: This will eventually fail, as the whole problem is that some
  -- portion of the time, CI will generate currTime /= currTime'. We will
  -- probably want to set this to something like 90 instead, but first
  -- we want to see what a typical failure looks like.
  H.cover 100 "Exact" exactEq

  unless exactEq $
    fmt (addSecond currTime) === fmt currTime'
  where
    fmt = Format.formatTime locale fmtOut
    locale = Format.defaultTimeLocale

addSecond :: ZonedTime -> ZonedTime
addSecond (ZonedTime lt tz) = ZonedTime (Time.addLocalTime nominalSecond lt) tz
  where
    nominalSecond :: NominalDiffTime
    nominalSecond = 1

defaultFormatTests :: TestTree
defaultFormatTests =
  testGroup
    "Parses local time with default format"
    $ mkParseTest
      <$> [ ("17:00", "17:00"),
            ("09:00", "09:00"),
            ("17:00", "1700"),
            ("09:00", "0900"),
            ("14:30", "02:30 pm"),
            ("14:30", "2:30 pm"),
            ("14:30", "02:30pm"),
            ("14:30", "2:30pm"),
            ("09:10", "09:10 am"),
            ("09:10", "9:10 am"),
            ("14:00", "2 pm"),
            ("09:00", "9 am"),
            ("14:00", "2pm"),
            ("09:00", "9am")
          ]

mkParseTest :: (String, String) -> TestTree
mkParseTest (expected, s) = testCase ("Parses " ++ s) (parsesDefault expected s)

parsesDefault :: String -> String -> IO ()
parsesDefault expected s = case Kairos.readTimeFormatLocal locale fmts (T.pack s) of
  Nothing -> assertFailure $ "Failed to parse local time: " ++ s
  Just result -> expected @=? format result
  where
    fmts = TimeFmt.defaultTimeFormats
    locale = Format.defaultTimeLocale

    format = Format.formatTime locale "%H:%M"

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif
