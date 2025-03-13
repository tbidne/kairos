{-# LANGUAGE CPP #-}

module Unit.Data.Time.Conversion (tests) where

import Control.Exception.Utils (catchSync)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Conversion qualified as Conversion
import Data.Time.Conversion.Types.Date (Date (DateLiteral, DateToday))
import Data.Time.Conversion.Types.TZInput (TZInput (TZDatabase))
import Data.Time.Conversion.Types.TimeFormat qualified as TimeFmt
import Data.Time.Conversion.Types.TimeReader
  ( TimeReader
      ( MkTimeReader,
        date,
        format,
        srcTZ,
        timeString
      ),
  )
import Data.Time.Format qualified as Format
import Data.Time.LocalTime (ZonedTime (ZonedTime))
import Data.Time.LocalTime qualified as Time
import Hedgehog (Property, PropertyName)
import Hedgehog qualified as H
import Hedgehog.Internal.Property ((===))
import Props.Generators qualified as G
import Test.Tasty (TestName, TestTree, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif
import Unit.Utils qualified as Utils

tests :: TestTree
tests =
  testGroup
    "Data.Time.Conversion"
    [ testDestSrcRoundtrips,
      testDestSrcDateRoundtrips
    ]

testDestSrcRoundtrips :: TestTree
testDestSrcRoundtrips =
  testPropertyCompat "currTime == fromSource . toDest (date today)" "testDestSrcRoundtrips" $
    H.property $ do
      tzdb <- TZDatabase <$> H.forAll G.tzLabel

      currTime <- liftIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Conversion.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTimeDest
      let currTimeDestStr = fmt currTimeDest
          timeReader =
            MkTimeReader
              { format = TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just DateToday,
                timeString = T.pack currTimeDestStr
              }

      currTime' <-
        liftIO (Conversion.readConvertTime (Just timeReader) Nothing)
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

      currTime <- liftIO $ Conversion.readConvertTime Nothing Nothing
      H.annotateShow currTime
      currTimeDest <- liftIO $ Conversion.readConvertTime Nothing (Just tzdb)
      H.annotateShow currTime
      (currDateDestStr, currTimeDestStr) <-
        case T.split (== ' ') (T.pack $ fmt currTimeDest) of
          [y, d] -> pure (y, d)
          _ -> do
            let err =
                  mconcat
                    [ "Unit.Data.Time.Conversion: date should have format ",
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
              { format = TimeFmt.hm,
                srcTZ = Just tzdb,
                date = Just $ DateLiteral currDateDestStr',
                timeString = currTimeDestStr
              }

      currTime' <-
        liftIO (Conversion.readConvertTime (Just timeReader) Nothing)
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

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif
