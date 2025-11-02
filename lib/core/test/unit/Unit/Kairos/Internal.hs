{-# LANGUAGE CPP #-}

module Unit.Kairos.Internal (tests) where

import Control.DeepSeq (force)
import Control.Exception
  ( evaluate,
  )
import Control.Monad (void)
#if !MIN_VERSION_base(4, 20, 0)
import Data.Foldable (Foldable (foldl'))
#endif
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding.Error qualified as TError
import Data.Time.Zones.All (TZLabel)
import Hedgehog
  ( failure,
    forAll,
  )
import Kairos.Internal qualified as Internal
import Props.Generators qualified as G
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Unit.Utils qualified as Utils

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
      tzNameRandomCase,
      tzLabelNameRoundtrip
    ]

tzNameMapParses :: TestTree
tzNameMapParses =
  testCase "tzNameMap parses without errors" $
    void $
      evaluate (force mp)
  where
    mp = Internal.tzLowerNameLabelMapWith TError.strictDecode

tzNameRandomCase :: TestTree
tzNameRandomCase = Utils.testProp desc "tzNameRandomCase" $ do
  txt <- forAll G.tzText
  case Internal.tzNameToLabel txt of
    Just _ -> pure ()
    Nothing -> failure
  where
    desc = "tzNameToLabel handles random case"

data ParseResult
  = ParseFailure Text
  | ParseNotEqual Text TZLabel
  | ParseSuccess

tzLabelNameRoundtrip :: TestTree
tzLabelNameRoundtrip = testCase desc $ do
  -- Tests each label individually. We do this rather than hedgehog because:
  --
  --   - We can be exhaustive.
  --   - We can collect and report all errors, rather than just the first.
  case foldl' go ([], []) [minBound .. maxBound] of
    ([], []) -> pure ()
    (failures, []) -> assertFailure $ T.unpack $ renderFailures failures
    ([], notEquals) -> assertFailure $ T.unpack $ renderNotEquals notEquals
    (failures, notEquals) -> do
      let fstr = renderFailures failures
          nstr = renderNotEquals notEquals
      assertFailure $ T.unpack $ fstr <> "\n\n" <> nstr
  where
    desc = "tzNameToLabel . tzLabelToName roundtrips"

    go acc@(fs, ns) lbl = case tryLabel lbl of
      ParseSuccess -> acc
      ParseFailure txt -> ((lbl, txt) : fs, ns)
      ParseNotEqual txt lbl' -> (fs, (lbl, txt, lbl') : ns)

    tryLabel lbl =
      let txt = Internal.tzLabelToName lbl
       in case Internal.tzNameToLabel txt of
            Nothing -> ParseFailure txt
            Just lbl' ->
              if lbl == lbl'
                then ParseSuccess
                else ParseNotEqual txt lbl'

    renderFailures =
      renderFailure
        "The following labels with derived names failed to parse"

    renderNotEquals =
      renderFailure
        "The following labels parsed to a different label"

    renderFailure :: (Show a) => Text -> [a] -> Text
    renderFailure header xs =
      (header' <>)
        . mconcat
        . fmap (("\n - " <>) . showt)
        $ xs
      where
        len = showt $ length xs

        header' =
          mconcat
            [ header,
              " (",
              len,
              "):"
            ]

showt :: (Show a) => a -> Text
showt = T.pack . show
