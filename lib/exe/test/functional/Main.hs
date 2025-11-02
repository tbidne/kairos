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
import Control.Monad.Reader
  ( MonadReader,
    MonadTrans,
    ReaderT (runReaderT),
    ask,
    asks,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Format qualified as Format
import Effects.FileSystem.FileReader (MonadFileReader)
import Effects.FileSystem.FileReader qualified as FR
import Effects.FileSystem.FileWriter (MonadFileWriter)
import Effects.FileSystem.PathReader
  ( MonadPathReader
      ( doesFileExist,
        getXdgDirectory
      ),
    XdgDirectory (XdgConfig, XdgState),
  )
import Effects.FileSystem.PathReader qualified as PR
import Effects.FileSystem.PathWriter (MonadPathWriter)
import Effects.FileSystem.PathWriter qualified as PW
import Effects.IORef (IORef, MonadIORef, modifyIORef', newIORef, readIORef)
import Effects.Optparse (MonadOptparse)
import Effects.System.Environment (MonadEnv)
import Effects.System.Environment qualified as SysEnv
import Effects.System.Terminal (MonadTerminal (putStrLn))
import Effects.System.Terminal qualified as Term
import Effects.Time
  ( MonadTime
      ( getMonotonicTime,
        getSystemZonedTime,
        getTimeZone
      ),
    ZonedTime (ZonedTime),
  )
import FileSystem.OsPath (OsPath, ospPathSep, unsafeDecode, (</>))
import GHC.Exts (IsList (toList))
import Kairos.Runner (PrintAliasesE, runKairos)
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
import System.Environment.Guard (ExpectEnv (ExpectEnvEquals, ExpectEnvSet))
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
    Tasty.withResource setup teardown $ \getTestDir ->
      testGroup
        "Functional tests"
        [ formatTests,
          formatOutputTests,
          srcTzTests,
          destTzTests,
          tzOffsetTests,
          dateTests,
          tomlTests getTestDir,
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
      testFormatOutputTimeZone,
      testFormatOutputTimeZoneOffset,
      testFormatOutputRfc822
    ]

testFormatOutputCustom :: TestTree
testFormatOutputCustom = testCase "Overrides output formatting" $ do
  result <- captureKairosIO $ pureTZ ["-o", "%H:%M %Z", "08:30"]
  "08:30 UTC" @=? result

testFormatOutputTimeZone :: TestTree
testFormatOutputTimeZone = testCase desc $ do
  result <- captureKairosIO $ pureTZ ["-o", "%Z", "08:30"]
  "UTC" @=? result
  where
    desc = "Overrides output formatting timezone"

testFormatOutputTimeZoneOffset :: TestTree
testFormatOutputTimeZoneOffset = testCase desc $ do
  result <- captureKairosIO $ pureTZ ["-o", "%z", "08:30"]
  "+0000" @=? result
  where
    desc = "Overrides input formatting timezone offset"

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
    captureKairosIO $ pureDestTZDate $ set' #srcTZ "Europe/Paris" "08:30"
  "Sat,  5 Jan 1980 07:30:00 UTC" @=? result

testSrcTzDatabaseCase :: TestTree
testSrcTzDatabaseCase = testCase desc $ do
  result <-
    captureKairosIO $ pureDestTZDate $ set' #srcTZ "aMeRiCa/new_yoRk" "08:30"
  "Sat,  5 Jan 1980 13:30:00 UTC" @=? result

  result2 <-
    captureKairosIO $ pureDestTZDate $ set' #srcTZ "etc/utc" "08:30"
  "Sat,  5 Jan 1980 08:30:00 UTC" @=? result2
  where
    desc = "Uses source timezone from tz database with 'wrong' case"

testSrcTzFails :: TestTree
testSrcTzFails = testCase "Bad source timezone fails" $ do
  assertException @ParseTZInputException expected $ captureKairosIO args
  where
    args = pureDestTZ $ set' #srcTZ "Europe/Pariss" "08:30"
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
    args = set' #srcTZ "America/New_York" "08:30"

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
        { cliArgs = set' #srcTZ "America/New_York" "19:30",
          configEnabled = False,
          mCurrentTime = Just currTime,
          mXdg = Nothing
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
    captureKairosIO $ pureSrcTZDate $ set' #destTZ "Europe/Paris" "08:30"
  "Sat,  5 Jan 1980 09:30:00 CET" @=? result

testSrcDestTzDatabase :: TestTree
testSrcDestTzDatabase = testCase "Uses src to dest" $ do
  result <-
    captureKairosIO
      . fixedDate
      . set' #srcTZ "America/New_York"
      . set' #destTZ "Europe/Paris"
      $ "08:30"
  "Sat,  5 Jan 1980 14:30:00 CET" @=? result

testDestTzFails :: TestTree
testDestTzFails = testCase "Bad dest timezone fails" $ do
  assertException @ParseTZInputException expected $ captureKairosIO args
  where
    args = pureSrcTZ $ set' #destTZ "Europe/Pariss" "08:30"
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
    captureKairosIO
      . fixedDate
      . set' #srcTZ "+13:00"
      . set' #destTZ "-08:00"
      $ "08:30"
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetNoColon :: TestTree
testTzOffsetNoColon = testCase "Uses tz offsets without colon" $ do
  result <-
    captureKairosIO
      . fixedDate
      . set' #srcTZ "+1300"
      . set' #destTZ "-0800"
      $ "08:30"
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetHours :: TestTree
testTzOffsetHours = testCase "Uses tz offsets with hours only" $ do
  result <-
    captureKairosIO
      . fixedDate
      . set' #srcTZ "+13"
      . set' #destTZ "-08"
      $ "08:30"
  "Fri,  4 Jan 1980 11:30:00 -0800" @=? result

testTzOffsetUtc :: TestTree
testTzOffsetUtc = testCase "Uses tz offsets utc" $ do
  result <-
    captureKairosIO
      . fixedDate
      . set' #srcTZ "Z"
      . set' #destTZ "-0800"
      $ "08:30"
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

    args1 = set' #date "2022-03-10" args
    expected1 = "Thu, 10 Mar 2022 03:30:00 EST"

    args2 = set' #date "2022-06-10" args
    expected2 = "Fri, 10 Jun 2022 03:30:00 EDT"

    args =
      set' #srcTZ "europe/london"
        . set' #destTZ "america/new_york"
        $ "08:30"

testConvertExplicit2 :: TestTree
testConvertExplicit2 = testCase desc $ do
  result1 <- captureKairosIO args1
  expected1 @=? result1

  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 2"

    args1 = set' #date "2022-03-10" args
    expected1 = "Thu, 10 Mar 2022 13:30:00 GMT"

    args2 = set' #date "2022-06-10" args
    expected2 = "Fri, 10 Jun 2022 13:30:00 BST"

    args =
      set' #srcTZ "America/New_York"
        . set' #destTZ "europe/london"
        $ "08:30"

testConvertExplicit3 :: TestTree
testConvertExplicit3 = testCase desc $ do
  result1 <- captureKairosIO args1
  expected1 @=? result1

  result2 <- captureKairosIO args2
  expected2 @=? result2
  where
    desc = "Converts explicit src -> dest 3"

    args1 = set' #date "2022-03-10" args
    expected1 = "Fri, 11 Mar 2022 02:30:00 NZDT"

    args2 = set' #date "2022-05-10" args
    expected2 = "Wed, 11 May 2022 00:30:00 NZST"

    args =
      set' #srcTZ "america/new_york"
        . set' #destTZ "Pacific/Auckland"
        $ "08:30"

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

    args1 = set' #date "2022-03-10" args
    expected1 = "Wed,  9 Mar 2022 14:30:00 EST"

    args2 = set' #date "2022-05-10" args
    expected2 = "Mon,  9 May 2022 16:30:00 EDT"

    args =
      set' #srcTZ "Pacific/Auckland"
        . set' #destTZ "america/new_york"
        $ "08:30"

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

    args1 = set' #date "2022-03-10" args
    expected1 = "Thu, 10 Mar 2022 03:30:00 EST"

    args2 = set' #date "2022-06-10" args
    expected2 = "Fri, 10 Jun 2022 03:30:00 EDT"

    args = set' #srcTZ "europe/london" "08:30"

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

    args1 = set' #date "2022-03-10" args
    expected1 = "Thu, 10 Mar 2022 13:30:00 GMT"

    args2 = set' #date "2022-06-10" args
    expected2 = "Fri, 10 Jun 2022 13:30:00 BST"

    args = set' #destTZ "europe/london" "08:30"

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

    args1 = set' #date "2022-03-10" args
    expected1 = "Fri, 11 Mar 2022 02:30:00 NZDT"

    args2 = set' #date "2022-05-10" args
    expected2 = "Wed, 11 May 2022 00:30:00 NZST"

    args = set' #srcTZ "america/new_york" "08:30"

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

    args1 = set' #date "2022-03-10" args
    expected1 = "Wed,  9 Mar 2022 14:30:00 EST"

    args2 = set' #date "2022-05-10" args
    expected2 = "Mon,  9 May 2022 16:30:00 EDT"

    args = set' #destTZ "america/new_york" "08:30"

tomlTests :: IO OsPath -> TestTree
tomlTests getTestDir =
  testGroup
    "Toml"
    [ testTomlAliases getTestDir,
      testTomlPrintAliases,
      testTomlPrintAliasesNoColor,
      testTomlPrintAliasesNoAliases,
      testTomlPrintAliasesEmptyAliases,
      testTomlPrintAliasesNoConfig,
      testTomlPrintAliasesMissingConfig
    ]

testTomlAliases :: IO OsPath -> TestTree
testTomlAliases getTestDir = testCase "Config aliases succeed" $ do
  testDir <- (</> [ospPathSep|testTomlAliases|]) <$> getTestDir

  let withDest = withDest' testDir
      readSavedAliases = readSavedAliases' testDir

  Nothing <@=?> readSavedAliases

  resultsLA <- captureKairosParamsIO (withDest "la")
  "Tue, 12 Jul 2022 01:30:00 PDT" @=? resultsLA
  Just expectedAliases <@=?> readSavedAliases

  resultZagreb <- captureKairosParamsIO (withDest "zagreb")
  "Tue, 12 Jul 2022 10:30:00 CEST" @=? resultZagreb
  Just expectedAliases <@=?> readSavedAliases

  resultOffset <- captureKairosParamsIO (withDest "some_offset")
  "Tue, 12 Jul 2022 15:30:00 +0700" @=? resultOffset
  Just expectedAliases <@=?> readSavedAliases
  where
    withDest' xdg d =
      MkTestParams
        { cliArgs =
            set' #date "2022-07-12"
              . set' #srcTZ "Etc/Utc"
              . set' #destTZ d
              $ [ "-c",
                  configFp,
                  "08:30"
                ],
          configEnabled = True,
          mCurrentTime = Nothing,
          mXdg = Just xdg
        }

    expectedAliases =
      [ "la",
        "ny",
        "paris",
        "some_offset",
        "uk",
        "zagreb"
      ]

    readSavedAliases' xdg = do
      let path = xdg </> [ospPathSep|state/kairos/aliases.txt|]
      exists <- PR.doesFileExist path
      if exists
        then Just . T.lines <$> FR.readFileUtf8ThrowM path
        else pure Nothing

testTomlPrintAliases :: TestTree
testTomlPrintAliases = testCase desc $ do
  results <- captureKairosParamsIO params
  expected @=? results
  where
    desc = "Prints example alises"

    params =
      set' #configEnabled True
        . Params.fromArgs
        $ ["-c", configFp, "--color", "on", "--print-aliases"]

    expected =
      unlinesNoTrailingNL
        [ c1 <> "- la:          america/los_angeles" <> endColor,
          c2 <> "- ny:          america/new_york" <> endColor,
          c1 <> "- paris:       europe/paris" <> endColor,
          c2 <> "- some_offset: +0700" <> endColor,
          c1 <> "- uk:          europe/london" <> endColor,
          c2 <> "- zagreb:      europe/zagreb" <> endColor
        ]

    c1 = "\ESC[34m"
    c2 = "\ESC[32m"
    endColor = "\ESC[0m"

testTomlPrintAliasesNoColor :: TestTree
testTomlPrintAliasesNoColor = testCase desc $ do
  results <- captureKairosParamsIO params
  expected @=? results
  where
    desc = "Prints example aliases without coloring"
    params =
      -- color = false is set in the toml file, so no need here.
      set' #configEnabled True
        . Params.fromArgs
        $ ["-c", configFp, "--print-aliases"]

    expected =
      unlinesNoTrailingNL
        [ "- la:          america/los_angeles",
          "- ny:          america/new_york",
          "- paris:       europe/paris",
          "- some_offset: +0700",
          "- uk:          europe/london",
          "- zagreb:      europe/zagreb"
        ]

-- avoid T.unlines to skip trailing newline.
unlinesNoTrailingNL :: [Text] -> Text
unlinesNoTrailingNL = T.intercalate "\n"

testTomlPrintAliasesNoAliases :: TestTree
testTomlPrintAliasesNoAliases = testCase desc $ do
  results <- captureKairosParamsIO params
  expected @=? results
  where
    desc = "--print-aliases with no aliases"
    params =
      set' #configEnabled True
        . Params.fromArgs
        $ ["-c", cfg, "--print-aliases"]

    cfg = unsafeDecode [ospPathSep|test/functional/toml/empty.toml|]
    expected =
      mconcat
        [ "No aliases found in toml: ",
          T.pack cfg
        ]

testTomlPrintAliasesEmptyAliases :: TestTree
testTomlPrintAliasesEmptyAliases = testCase desc $ do
  results <- captureKairosParamsIO params
  expected @=? results
  where
    desc = "--print-aliases with empty aliases"
    params =
      set' #configEnabled True
        . Params.fromArgs
        $ ["-c", cfg, "--print-aliases"]

    cfg = unsafeDecode [ospPathSep|test/functional/toml/empty2.toml|]
    expected =
      mconcat
        [ "No aliases found in toml: ",
          T.pack cfg
        ]

testTomlPrintAliasesNoConfig :: TestTree
testTomlPrintAliasesNoConfig = testCase desc $ do
  assertException @PrintAliasesE expected $ captureKairosIO args
  where
    desc = "--print-aliases fails with --config off"
    -- --config off defaults to true in our tests via default
    -- configEnabled = False
    args = ["--print-aliases"]
    expected = "--print-aliases was specified with --config off."

testTomlPrintAliasesMissingConfig :: TestTree
testTomlPrintAliasesMissingConfig = testCase desc $ do
  assertException @PrintAliasesE expected $ captureKairosParamsIO params
  where
    desc = "--print-aliases fails with missing config"
    params =
      set' #configEnabled True
        -- Setting the xdg dir to an extant dir w/ no kairos/config.toml, so
        -- that lookup succeeds but with none found.
        . set' #mXdg (Just [ospPathSep|test/functional|])
        . Params.fromArgs
        $ "--print-aliases"
    expected = "--print-aliases was specified, but no config file was found."

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

  resultsUtc <- captureKairosParamsIO $ mkParams $ set' #destTZ "etc/utc" []
  "Tue, 18 Apr 2023 23:30:00 UTC" @=? resultsUtc

  resultsParis <- captureKairosParamsIO $ mkParams $ set' #destTZ "europe/paris" []
  "Wed, 19 Apr 2023 01:30:00 CEST" @=? resultsParis
  where
    mkParams args = set' #mCurrentTime (Just currTime) (Params.fromArgs args)

    currTime = "2023-04-18 19:30 -0400"

testSrcTzNoTimeStr :: TestTree
testSrcTzNoTimeStr = testCase "Src w/o time string fails" $ do
  assertException @SrcTZNoTimeStringException expected $ captureKairosIO args
  where
    expected = "The --src-tz option was specified without required time string"
    args = set' #srcTZ "Etc/Utc" []

testDateNoTimeStr :: TestTree
testDateNoTimeStr = testCase "Date w/o time string fails" $ do
  assertException @DateNoTimeStringException expected $ captureKairosIO args
  where
    expected = "The --date option was specified without required time string"
    args = set' #date "2024-10-08" []

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
        else "--config" : "off" : toList params.cliArgs

    -- Runs kairos with the args, capturing terminal output.
    -- Toml configuration is not disabled, so take care that one of the following
    -- situations applies:
    --
    -- 1. Args includes @--config <path>@.
    -- 2. Args includes @--config off@.
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
        MonadFileWriter m,
        MonadIO m,
        MonadIORef m,
        MonadOptparse m,
        MonadPathWriter m,
        MonadTime m
      ) =>
      -- Args.
      [String] ->
      m Text
    captureKairosConfigM argList =
      SysEnv.withArgs argList $
        runTermT params.mXdg runKairos

-- | The purpose of 'MockTimeIO' is to mock the current time / timezone.
-- So why do we derive a bunch of IO instances, when TermT is ultimately what
-- runs? To make deriving easier.
--
-- Most of the IO instances are "real", so it is easier to derive them here
-- via IO, and then derive them on TermT via the underlying IO/MockTimeIO.
-- This is fine when we do not care about mocking the instance.
--
-- But consider e.g. MonadPathReader, which we _do_ want to potentially mock.
-- We want to provide the custom impl in TermT. When we mock the xgg dir,
-- impl is easy; just return the mocked value. When we do _not_ mock xdg i.e.
-- implement the real function, we have two choices: write the TermT instance
-- in terms of MonadIO or MonadPathReader.
--
-- We choose the former, as it means we do not have to provide an instance
-- for MonadPathReader MockTimeIO, which is arguably clearer about what is
-- happening. We could actually go farther here, and eliminate all of these
-- MockTimeIO file IO instances, and just explicitly implement the ones on
-- TermT in terms of MonadIO. But for now, this is easier.
newtype MockTimeIO a = MkMockTimeM (ReaderT String IO a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadFileWriter,
      MonadIO,
      MonadIORef,
      MonadOptparse,
      MonadPathWriter,
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

data TermEnv
  = MkTermEnv
  { output :: IORef Text,
    xdg :: Maybe OsPath
  }

-- Adds a MonadTerminal instance that reads putStrLn into an IORef. Intended
-- to be added "on top" of some Monad that implements the rest of Kairos's
-- dependencies e.g. IO or MockTimeIO.
--
-- This also potentially mocks xdg.
newtype TermT m a = MkTermT (ReaderT TermEnv m a)
  deriving
    ( Applicative,
      Functor,
      Monad,
      MonadCatch,
      MonadEnv,
      MonadFileReader,
      MonadFileWriter,
      MonadIO,
      MonadIORef,
      MonadOptparse,
      MonadPathWriter,
      MonadThrow,
      MonadTime
    )
    via (ReaderT TermEnv m)
  deriving (MonadReader TermEnv) via (ReaderT TermEnv m)
  deriving (MonadTrans) via (ReaderT TermEnv)

instance (MonadIORef m) => MonadTerminal (TermT m) where
  putStrLn s = asks (.output) >>= \ref -> modifyIORef' ref (T.pack s <>)

instance (MonadIO m) => MonadPathReader (TermT m) where
  doesFileExist = liftIO . doesFileExist

  getXdgDirectory t p = case t of
    XdgConfig ->
      asks (.xdg) >>= \case
        Just xdg -> pure $ xdg </> [ospPathSep|config|] </> p
        Nothing -> liftIO $ getXdgDirectory XdgConfig p
    XdgState ->
      asks (.xdg) >>= \case
        Just xdg -> pure $ xdg </> [ospPathSep|state|] </> p
        Nothing -> liftIO $ getXdgDirectory XdgState p
    other -> error $ "Unexpected xdg type: " ++ show other

runTermT :: (MonadIORef m) => Maybe OsPath -> TermT m a -> m Text
runTermT mXg (MkTermT m) = do
  outputRef <- newIORef ""
  _ <- runReaderT m (MkTermEnv outputRef mXg)
  readIORef outputRef

configFp :: FilePath
configFp = unsafeDecode [ospPathSep|examples/config.toml|]

withTZ :: String -> IO a -> IO a
withTZ tz m = bracket setTZ resetTZ (const m)
  where
    setTZ = Env.lookupEnv "TZ" <* Env.setEnv "TZ" tz
    resetTZ = Env.setEnv "TZ" . fromMaybe ""

setup :: IO OsPath
setup = do
  tmpDir <- PR.getTemporaryDirectory
  let testDir = tmpDir </> [ospPathSep|kairos/functional|]
  PW.createDirectoryIfMissing True testDir
  pure testDir

teardown :: OsPath -> IO ()
teardown p = Guard.guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = PW.removeDirectoryRecursiveIfExists_ p
    doNothing = Term.putStrLn $ "*** Not cleaning up tmp dir: " <> show p

(<@=?>) :: (Eq a, Show a) => a -> IO a -> Assertion
(<@=?>) expected m = do
  result <- m
  expected @=? result

infix 1 <@=?>
