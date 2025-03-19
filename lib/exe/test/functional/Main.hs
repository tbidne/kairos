{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- | Functional test suite
--
-- @since 0.1
module Main (main) where

import Control.Exception (Exception (displayException), bracket)
import Control.Monad.Catch (MonadCatch, MonadThrow, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Effects.FileSystem.FileReader (MonadFileReader)
import Effects.FileSystem.PathReader (MonadPathReader)
import Effects.IORef (IORef, MonadIORef, modifyIORef', newIORef, readIORef)
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv)
import Effects.System.Environment qualified as SysEnv
import Effects.System.Terminal (MonadTerminal (putStrLn))
import Effects.Time
  ( MonadTime
      ( getMonotonicTime,
        getSystemZonedTime,
        getTimeZone
      ),
    ZonedTime (ZonedTime),
  )
import FileSystem.OsPath (ospPathSep, unsafeDecode)
import GHC.Exts (IsList (toList))
import Kairos.Runner (runKairos)
import Kairos.Types.Exception
  ( DateNoTimeStringException,
    ParseTZInputException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )
import Optics.Core (over', set')
import Params
  ( CliArgs,
    TestParams (MkTestParams, cliArgs, configEnabled, mCurrentTime),
  )
import Params qualified
import System.Environment qualified as Env
import System.Environment.Guard (ExpectEnv (ExpectEnvEquals))
import System.Environment.Guard qualified as Guard
import Test.Tasty (TestTree, testGroup)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@=?))

-- | Runs functional tests.
--
-- @since 0.1
main :: IO ()
main = do
  extra <-
    Guard.guardOrElse'
      "FUNC_EXTRA"
      (ExpectEnvEquals "1")
      (pure extraTests)
      (pure $ testGroup "Empty" [])

  Tasty.defaultMain $
    testGroup
      "Functional tests"
      [ formatTests,
        formatOutputTests,
        srcTzTests,
        destTzTests,
        tzOffsetTests,
        dateTests,
        tomlTests,
        miscTests,
        extra
      ]

formatTests :: TestTree
formatTests =
  testGroup
    "Input Format"
    [ testFormatDefault,
      testFormatCustom,
      testFormatFails
    ]

testFormatDefault :: TestTree
testFormatDefault = testCase "Uses default parsing" $ do
  result1 <- captureKairosIO ["08:30", "-o", "%H:%M"]
  "08:30" @=? result1

  result2 <- captureKairosIO ["0830", "-o", "%H:%M"]
  "08:30" @=? result2

  result3 <- captureKairosIO ["4:15 pm", "-o", "%H:%M"]
  "16:15" @=? result3

  result4 <- captureKairosIO ["4:15pm", "-o", "%H:%M"]
  "16:15" @=? result4

  result5 <- captureKairosIO ["4 pm", "-o", "%H:%M"]
  "16:00" @=? result5

  result6 <- captureKairosIO ["7 am", "-o", "%H:%M"]
  "07:00" @=? result6

  result7 <- captureKairosIO ["7am", "-o", "%H:%M"]
  "07:00" @=? result7

testFormatCustom :: TestTree
testFormatCustom = testCase "Uses custom parsing" $ do
  result <-
    captureKairosIO
      ["-f", "%Y-%m-%d %H:%M", "-o", "%H:%M", "2022-06-15 08:30"]
  "08:30" @=? result

testFormatFails :: TestTree
testFormatFails = testCase "Bad format fails" $ do
  assertException @ParseTimeException expected $ captureKairosIO args
  where
    args = pureTZ ["-f", "%Y %H:%M", "08:30"]
    expected =
      mconcat
        [ "Could not parse time string '08:30' with format(s): ",
          "\n - %Y %H:%M"
        ]

formatOutputTests :: TestTree
formatOutputTests =
  testGroup
    "Output Format"
    [ testFormatOutputCustom,
      testFormatOutputCustomTZOffset,
      testFormatOutputRfc822
    ]

testFormatOutputCustom :: TestTree
testFormatOutputCustom = testCase "Overrides input formatting" $ do
  result <- captureKairosIO $ pureTZ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputCustomTZOffset :: TestTree
testFormatOutputCustomTZOffset = testCase desc $ do
  result <- captureKairosIO $ pureTZ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result
  where
    desc = "Overrides input formatting tz offset"

testFormatOutputRfc822 :: TestTree
testFormatOutputRfc822 = testCase "Uses rfc822 output" $ do
  result <-
    captureKairosIO $ pureTZDate ["-o", "rfc822", "08:30"]
  "Sat,  5 Jan 1980 08:30:00 UTC" @=? result

srcTzTests :: TestTree
srcTzTests =
  testGroup
    "Source Timezone"
    [ testSrcTzDatabase,
      testSrcTzDatabaseCase,
      testSrcTzFails,
      testSrcTzDST,
      testSrcTzNoDate
    ]

testSrcTzDatabase :: TestTree
testSrcTzDatabase = testCase "Uses source timezone from tz database" $ do
  result <-
    captureKairosIO $ pureDestTZDate ["-s", "Europe/Paris", "08:30"]
  "Sat,  5 Jan 1980 07:30:00 UTC" @=? result

testSrcTzDatabaseCase :: TestTree
testSrcTzDatabaseCase = testCase desc $ do
  result <-
    captureKairosIO $ pureDestTZDate ["-s", "aMeRiCa/new_yoRk", "08:30"]
  "Sat,  5 Jan 1980 13:30:00 UTC" @=? result

  result2 <-
    captureKairosIO $ pureDestTZDate ["-s", "etc/utc", "08:30"]
  "Sat,  5 Jan 1980 08:30:00 UTC" @=? result2
  where
    desc = "Uses source timezone from tz database with 'wrong' case"

testSrcTzFails :: TestTree
testSrcTzFails = testCase "Bad source timezone fails" $ do
  assertException @ParseTZInputException expected $ captureKairosIO args
  where
    args = pureDestTZ ["-s", "Europe/Pariss", "08:30"]
    expected =
      mconcat
        [ "Could not parse timezone from 'Europe/Pariss'. Wanted a name or offset ",
          "e.g. 'America/New_York', '+0800'."
        ]

testSrcTzDST :: TestTree
testSrcTzDST = testCase "Correctly converts src w/ DST" $ do
  result <- captureKairosIO $ pureDestTZ argsDST
  "Mon, 10 Apr 2023 12:30:00 UTC" @=? result

  result2 <- captureKairosIO $ pureDestTZ argsNoDST
  "Tue, 10 Jan 2023 13:30:00 UTC" @=? result2
  where
    argsDST = set' #date "2023-04-10" args
    argsNoDST = set' #date "2023-01-10" args
    args =
      [ "-s",
        "America/New_York",
        "08:30"
      ]

testSrcTzNoDate :: TestTree
testSrcTzNoDate = testCase "Correctly converts src w/o --date" $ do
  resultUtcSrcDst <- captureKairosParamsIO $ mkSrcParams pureDestTZ
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultUtcSrcDst

  resultNzstSrcDst <-
    captureKairosParamsIO $
      mkSrcParams (set' #destTZ "Pacific/Auckland")
  "Wed, 19 Apr 2023 11:30:00 NZST" @=? resultNzstSrcDst

  resultUtcDestDst <- captureKairosParamsIO $ mkDestParams pureDestTZ
  "Sun, 19 Feb 2023 00:30:00 UTC" @=? resultUtcDestDst

  resultNzstDestDst <-
    captureKairosParamsIO $
      mkDestParams (set' #destTZ "Pacific/Auckland")
  "Sun, 19 Feb 2023 13:30:00 NZDT" @=? resultNzstDestDst
  where
    mkSrcParams :: (CliArgs -> CliArgs) -> TestParams
    mkSrcParams f = over' #cliArgs f $ mkParams currTimeSrcDst

    mkDestParams :: (CliArgs -> CliArgs) -> TestParams
    mkDestParams f = over' #cliArgs f $ mkParams currTimeDestDst

    mkParams :: String -> TestParams
    mkParams currTime =
      MkTestParams
        { cliArgs =
            [ "-s",
              "America/New_York",
              "19:30"
            ],
          configEnabled = False,
          mCurrentTime = Just currTime
        }

    currTimeSrcDst = "2023-04-18 19:30 -0400"
    currTimeDestDst = "2023-02-18 19:30 -0500"

destTzTests :: TestTree
destTzTests =
  testGroup
    "Dest Timezone"
    [ testDestTzDatabase,
      testSrcDestTzDatabase,
      testDestTzFails
    ]

testDestTzDatabase :: TestTree
testDestTzDatabase = testCase "Uses dest timezone from tz database" $ do
  result <-
    captureKairosIO $ pureSrcTZDate ["-d", "Europe/Paris", "08:30"]
  "Sat,  5 Jan 1980 09:30:00 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <-
    captureKairosIO $
      fixedDate ["-s", "America/New_York", "-d", "Europe/Paris", "08:30"]
  "Sat,  5 Jan 1980 14:30:00 CET" @=? result

testDestTzFails :: TestTree
testDestTzFails = testCase "Bad dest timezone fails" $ do
  assertException @ParseTZInputException expected $ captureKairosIO args
  where
    args = pureSrcTZ ["-d", "Europe/Pariss", "08:30"]
    expected =
      mconcat
        [ "Could not parse timezone from 'Europe/Pariss'. Wanted a name or offset ",
          "e.g. 'America/New_York', '+0800'."
        ]

tzOffsetTests :: TestTree
tzOffsetTests =
  testGroup
    "Parses TZ offsets"
    [ testTzOffsetColon,
      testTzOffsetNoColon,
      testTzOffsetHours,
      testTzOffsetUtc
    ]

testTzOffsetColon :: TestTree
testTzOffsetColon = testCase "Uses tz offsets with colon" $ do
  result <-
    captureKairosIO $
      fixedDate ["-s", "+13:00", "08:30", "-d", "-08:00"]
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetNoColon :: TestTree
testTzOffsetNoColon = testCase "Uses tz offsets without colon" $ do
  result <-
    captureKairosIO $
      fixedDate ["-s", "+1300", "08:30", "-d", "-0800"]
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetHours :: TestTree
testTzOffsetHours = testCase "Uses tz offsets with hours only" $ do
  result <-
    captureKairosIO $
      fixedDate ["-s", "+13", "08:30", "-d", "-08"]
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetUtc :: TestTree
testTzOffsetUtc = testCase "Uses tz offsets without colon" $ do
  result <-
    captureKairosIO $
      fixedDate ["-s", "Z", "08:30", "-d", "-0800"]
  "Sat,  5 Jan 1980 00:30:00 -0800" @=? result

dateTests :: TestTree
dateTests =
  testGroup
    "Handles dates correctly"
    [ testConvertExplicit1,
      testConvertExplicit2,
      testConvertExplicit3,
      testConvertExplicit4
    ]

testConvertExplicit1 :: TestTree
testConvertExplicit1 = testCase desc $ do
  result1 <- captureKairosIO args1
  expected1 @=? result1

  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 1"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Thu, 10 Mar 2022 03:30:00 EST"

    date2 = "2022-06-10"
    args2 = mkArgs date2
    expected2 = "Fri, 10 Jun 2022 03:30:00 EDT"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "europe/london",
        "-d",
        "america/new_york",
        "08:30"
      ]

testConvertExplicit2 :: TestTree
testConvertExplicit2 = testCase desc $ do
  result1 <- captureKairosIO args1
  expected1 @=? result1

  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 2"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Thu, 10 Mar 2022 13:30:00 GMT"

    date2 = "2022-06-10"
    args2 = mkArgs date2
    expected2 = "Fri, 10 Jun 2022 13:30:00 BST"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "America/New_York",
        "-d",
        "europe/london",
        "08:30"
      ]

testConvertExplicit3 :: TestTree
testConvertExplicit3 = testCase desc $ do
  result1 <- captureKairosIO args1
  expected1 @=? result1

  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 3"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Fri, 11 Mar 2022 02:30:00 NZDT"

    date2 = "2022-05-10"
    args2 = mkArgs date2
    expected2 = "Wed, 11 May 2022 00:30:00 NZST"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "america/new_york",
        "-d",
        "Pacific/Auckland",
        "08:30"
      ]

testConvertExplicit4 :: TestTree
testConvertExplicit4 = testCase desc $ do
  -- Date 1
  result1 <- captureKairosIO args1
  expected1 @=? result1

  -- Date 2
  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 4"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Wed,  9 Mar 2022 14:30:00 EST"

    date2 = "2022-05-10"
    args2 = mkArgs date2
    expected2 = "Mon,  9 May 2022 16:30:00 EDT"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "Pacific/Auckland",
        "-d",
        "america/new_york",
        "08:30"
      ]

-- These are tests that are useful to run at least locally but should not be
-- run by default because e.g. they are not reliable on arbitrary systems.
extraTests :: TestTree
extraTests =
  testGroup
    "Extra"
    [ -- These are essentially copies of the above dateTests, except we rely
      -- on implicitly reading system timezone/tz for one of src/dest.
      -- This does not work on windows CI, hence it is guarded behind an env
      -- var.
      testCurrTZFromSrcTime1,
      testCurrTZToDestTime1,
      testCurrTZFromSrcTime2,
      testCurrTZToDestTime2
    ]

testCurrTZFromSrcTime1 :: TestTree
testCurrTZFromSrcTime1 = testCase desc $ do
  -- For the first date -- 2022-03-10 -- the source timezone is GMT (+0000)
  -- and the dest is EST (-0500), hence the total offset is -0500.
  --
  -- The second date -- 2022-06-10 -- the source timezone is BST (+0100)
  -- and the dest is EDT (-0400), hence hte total offset is -0500.
  withTZ "America/New_York" $ do
    result1 <- captureKairosIO args1
    expected1 @=? result1

    result2 <- captureKairosIO args2
    expected2 @=? result2
  where
    desc = "Derives current timezone from source time 1"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Thu, 10 Mar 2022 03:30:00 EST"

    date2 = "2022-06-10"
    args2 = mkArgs date2
    expected2 = "Fri, 10 Jun 2022 03:30:00 EDT"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "europe/london",
        "08:30"
      ]

testCurrTZToDestTime1 :: TestTree
testCurrTZToDestTime1 = testCase desc $ do
  -- For the first date -- 2022-03-10 -- the source timezone is EST (-0500)
  -- and the dest is GMT (+0000), hence the total offset is +0500.
  --
  -- The second date -- 2022-06-10 -- the source timezone is EDT (-0400)
  -- and the dest is BST (+0100), hence hte total offset is +0500.
  withTZ "America/New_York" $ do
    result1 <- captureKairosIO args1
    expected1 @=? result1

    result2 <- captureKairosIO args2
    expected2 @=? result2
  where
    desc = "Derives current timezone from dest time 1"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Thu, 10 Mar 2022 13:30:00 GMT"

    date2 = "2022-06-10"
    args2 = mkArgs date2
    expected2 = "Fri, 10 Jun 2022 13:30:00 BST"

    mkArgs d =
      [ "--date",
        d,
        "-d",
        "europe/london",
        "08:30"
      ]

-- Evidently CI on windows does not have Pacific/Auckland, so this fails.
-- But it is still a useful test, so we include it here.
testCurrTZFromSrcTime2 :: TestTree
testCurrTZFromSrcTime2 = testCase desc $ do
  -- NOTE: [Testing current timezone]
  --
  -- Notice that these two tests are identical _except_ for the year. We
  -- deliberately choose two dates where the "current timezone" differs
  -- dependening on the date. Here is why.
  --
  -- For the first date -- 2022-03-10 -- the source timezone is EST (-0500)
  -- and the dest is NZDT (+1300), hence the total offset is +1800.
  --
  -- The second date -- 2022-05-10 -- the source timezone is EDT (-0400)
  -- and the dest is NZST (+1200), hence hte total offset is +1600.
  --
  -- These conversions should _NOT_ depend on the current system timezone i.e.
  -- the system timezone at the current time. Previously they did, which
  -- would mean one of these tests would fail.
  --
  -- For instance, if we were to run this when it is currently NZST, the
  -- first test would fail. If instead it was NZDT, the second test would fail.
  withTZ "Pacific/Auckland" $ do
    result1 <- captureKairosIO args1
    expected1 @=? result1

    result2 <- captureKairosIO args2
    expected2 @=? result2
  where
    desc = "Derives current timezone from source time 2"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Fri, 11 Mar 2022 02:30:00 NZDT"

    date2 = "2022-05-10"
    args2 = mkArgs date2
    expected2 = "Wed, 11 May 2022 00:30:00 NZST"

    mkArgs d =
      [ "--date",
        d,
        "-s",
        "america/new_york",
        "08:30"
      ]

testCurrTZToDestTime2 :: TestTree
testCurrTZToDestTime2 = testCase desc $ do
  -- See NOTE: [Testing current timezone] for motivation. The breakdown here
  -- is:
  --
  -- For the first date -- 2022-03-10 -- the source timezone is NZDT (+1300)
  -- and the dest is EST (-0500), hence the total offset is +1800.
  --
  -- The second date -- 2022-05-10 -- the source timezone is NZST (+1200)
  -- and the dest is EDT (-0400), hence hte total offset is +1600.
  withTZ "Pacific/Auckland" $ do
    -- Date 1
    result1 <- captureKairosIO args1
    expected1 @=? result1

    -- Date 2
    result2 <- captureKairosIO args2
    expected2 @=? result2
  where
    desc = "Derives current timezone from dest time 2"

    date1 = "2022-03-10"
    args1 = mkArgs date1
    expected1 = "Wed,  9 Mar 2022 14:30:00 EST"

    date2 = "2022-05-10"
    args2 = mkArgs date2
    expected2 = "Mon,  9 May 2022 16:30:00 EDT"

    mkArgs d =
      [ "--date",
        d,
        "-d",
        "america/new_york",
        "08:30"
      ]

tomlTests :: TestTree
tomlTests =
  testGroup
    "Toml"
    [ testTomlAliases
    ]

testTomlAliases :: TestTree
testTomlAliases = testCase "Config aliases succeed" $ do
  resultsLA <- captureKairosParamsIO (withDest "la")
  "Tue, 12 Jul 2022 01:30:00 PDT" @=? resultsLA

  resultZagreb <- captureKairosParamsIO (withDest "zagreb")
  "Tue, 12 Jul 2022 10:30:00 CEST" @=? resultZagreb

  resultOffset <- captureKairosParamsIO (withDest "some_offset")
  "Tue, 12 Jul 2022 15:30:00 +0700" @=? resultOffset
  where
    withDest d =
      MkTestParams
        { cliArgs =
            [ "-c",
              configFp,
              "-s",
              "Etc/Utc",
              "-d",
              d,
              "--date",
              "2022-07-12",
              "08:30"
            ],
          configEnabled = True,
          mCurrentTime = Nothing
        }

miscTests :: TestTree
miscTests =
  testGroup
    "Misc"
    [ testNoArgs,
      testNoTimeString,
      testSrcTzNoTimeStr,
      testDateNoTimeStr
    ]

testNoArgs :: TestTree
testNoArgs = testCase "No args succeeds" $ do
  result <- captureKairosIO []
  assertBool ("Should be non-empty: " <> T.unpack result) $ (not . T.null) result

testNoTimeString :: TestTree
testNoTimeString = testCase "No time string gets current time" $ do
  resultsLocal <- captureKairosParamsIO $ mkParams []
  "Tue, 18 Apr 2023 19:30:00 -0400" @=? resultsLocal

  resultsUtc <- captureKairosParamsIO $ mkParams ["-d", "etc/utc"]
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultsUtc

  resultsParis <- captureKairosParamsIO $ mkParams ["-d", "europe/paris"]
  "Wed, 19 Apr 2023 01:30:00 CEST" @=? resultsParis
  where
    mkParams args = set' #mCurrentTime (Just currTime) (Params.fromArgs args)

    currTime = "2023-04-18 19:30 -0400"

testSrcTzNoTimeStr :: TestTree
testSrcTzNoTimeStr = testCase "Src w/o time string fails" $ do
  assertException @SrcTZNoTimeStringException expected $ captureKairosIO args
  where
    expected = "The --src-tz option was specified without required time string"
    args =
      [ "-s",
        "Etc/Utc"
      ]

testDateNoTimeStr :: TestTree
testDateNoTimeStr = testCase "Date w/o time string fails" $ do
  assertException @DateNoTimeStringException expected $ captureKairosIO args
  where
    expected = "The --date option was specified without required time string"
    args =
      [ "--date",
        "2024-10-08"
      ]

assertException :: forall e a. (Exception e) => String -> IO a -> Assertion
assertException expected io = do
  try @_ @e io >>= \case
    Right _ -> assertFailure "Expected exception, received none"
    Left result -> do
      let result' = displayException result
      assertBool
        ("Encountered exception: " <> expected <> "\nReceived: " <> result')
        (startsWith expected result')

-- | Runs kairos with default TestParams i.e.
--
-- - The given CLI args.
-- - Toml config disabled.
-- - No mocked time string.
captureKairosIO :: CliArgs -> IO Text
captureKairosIO = captureKairosParamsIO . Params.fromArgs

-- | General function for capturing kairos output given TestParams.
captureKairosParamsIO :: TestParams -> IO Text
captureKairosParamsIO params = case params.mCurrentTime of
  Nothing -> captureKairosConfigM args'
  Just timeString -> usingMockTimeIO timeString (captureKairosConfigM args')
  where
    args' =
      if params.configEnabled
        then toList $ params.cliArgs
        else "--no-config" : toList params.cliArgs

    -- Runs kairos with the args, capturing terminal output.
    -- Toml configuration is not disabled, so take care that one of the following
    -- situations applies:
    --
    -- 1. Args includes @--config <path>@.
    -- 2. Args includes @--no-config@.
    -- 3. The XDG config dir is overridden to an expected path.
    --
    -- Otherwise, we may end up picking up a toml configuration at the real,
    -- expected XDG location, possibly inteferring with tests.
    --
    -- The function is polymorphic so that we can run it both without mocked
    -- time (IO) and with mocked time (MockTimeIO).
    --
    -- This function is intended to be used by captureKairosParamsIO only,
    -- hence the @where@ declaration.
    captureKairosConfigM ::
      ( MonadEnv m,
        MonadCatch m,
        MonadFileReader m,
        MonadIORef m,
        MonadOptparse m,
        MonadPathReader m,
        MonadTime m
      ) =>
      -- Args.
      [String] ->
      m Text
    captureKairosConfigM argList = SysEnv.withArgs argList $ runTermT runKairos

newtype MockTimeIO a = MkMockTimeM (ReaderT String IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadIO,
      MonadIORef,
      MonadOptparse,
      MonadPathReader,
      MonadThrow
    )
    via (ReaderT String IO)
  deriving (MonadReader String) via (ReaderT String IO)

runMockTimeIO :: MockTimeIO a -> String -> IO a
runMockTimeIO (MkMockTimeM rdr) = runReaderT rdr

usingMockTimeIO :: String -> MockTimeIO a -> IO a
usingMockTimeIO = flip runMockTimeIO

instance MonadTime MockTimeIO where
  getSystemZonedTime = do
    str <- ask
    liftIO $
      Format.parseTimeM
        True
        Format.defaultTimeLocale
        "%Y-%m-%d %H:%M %z"
        str

  getTimeZone _ = do
    ZonedTime _ tz <- getSystemZonedTime

    pure tz

  getMonotonicTime = pure 0

-- when we want to ensure that nothing depends on local time.
pureTZ :: CliArgs -> CliArgs
pureTZ = pureDestTZ . pureSrcTZ

pureSrcTZ :: CliArgs -> CliArgs
pureSrcTZ = set' #srcTZ "utc"

pureDestTZ :: CliArgs -> CliArgs
pureDestTZ = set' #destTZ "utc"

fixedDate :: CliArgs -> CliArgs
fixedDate = set' #date "1980-01-05"

pureSrcTZDate :: CliArgs -> CliArgs
pureSrcTZDate = pureSrcTZ . fixedDate

pureDestTZDate :: CliArgs -> CliArgs
pureDestTZDate = pureDestTZ . fixedDate

pureTZDate :: CliArgs -> CliArgs
pureTZDate = pureTZ . fixedDate

startsWith :: (Eq a) => [a] -> [a] -> Bool
startsWith [] _ = True
startsWith (_ : _) [] = False
startsWith (x : xs) (y : ys)
  | x == y = startsWith xs ys
  | otherwise = False

-- Adds a MonadTerminal instance that reads putStrLn into an IORef. Intended
-- to be added "on top" of some Monad that implements the rest of Kairos's
-- dependencies e.g. IO or MockTimeIO.
newtype TermT m a = MkTermT (ReaderT (IORef Text) m a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadIORef,
      MonadOptparse,
      MonadPathReader,
      MonadThrow,
      MonadTime
    )
    via (ReaderT (IORef Text) m)
  deriving (MonadReader (IORef Text)) via (ReaderT (IORef Text) m)

instance (MonadIORef m) => MonadTerminal (TermT m) where
  putStrLn s = ask >>= \ref -> modifyIORef' ref (T.pack s <>)

runTermT :: (MonadIORef m) => TermT m a -> m Text
runTermT (MkTermT m) = do
  outputRef <- newIORef ""
  _ <- runReaderT m outputRef
  readIORef outputRef

configFp :: FilePath
configFp = unsafeDecode [ospPathSep|examples/config.toml|]

withTZ :: String -> IO a -> IO a
withTZ tz m = bracket setTZ resetTZ (const m)
  where
    setTZ = Env.lookupEnv "TZ" <* Env.setEnv "TZ" tz
    resetTZ = Env.setEnv "TZ" . fromMaybe ""
