{-# LANGUAGE CPP #-}

module Props.Generators
  ( tzText,
    tzLabel,
    offsetTxt,
  )
where

import Data.Char qualified as Ch
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TEnc
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLBuilder
import Data.Time.Zones.All (TZLabel)
import Data.Time.Zones.All qualified as All
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HG

-- | Generates a TZLabel.
--
-- @since 0.1
tzLabel :: Gen TZLabel
tzLabel = HG.enumBounded

-- | Generates text corresponding to TZLabel with random case.
--
-- @since 0.1
tzText :: Gen Text
tzText = tzLabel >>= textCase . toText
  where
    toText = TEnc.decodeUtf8 . All.toTZName

-- | Transforms each character's case.
--
-- @since 0.1
textCase :: Text -> Gen Text
textCase =
  fmap (TL.toStrict . TLBuilder.toLazyText)
    . tfoldr f (pure "")
  where
    f :: Char -> Gen Builder -> Gen Builder
    f c gacc =
      charCase c >>= \c' ->
        (TLBuilder.singleton c' <>) <$> gacc

-- | Generates offset text e.g. "+08:00"
offsetTxt :: Gen Text
offsetTxt = do
  s <- genSign
  h <- gen2Digits
  m <- HG.choice [genMin, pure ""]
  pure $ s <> h <> m
  where
    genSign :: Gen Text
    genSign = HG.element ["+", "-"]

    gen2Digits :: Gen Text
    gen2Digits = (\a b -> T.pack [a, b]) <$> HG.digit <*> HG.digit

    genMin :: Gen Text
    genMin = do
      mColon <- HG.element [":", ""]
      m <- gen2Digits
      pure $ mColon <> m

-- | Transforms the char's case.
--
-- @since 0.1
charCase :: Char -> Gen Char
charCase c = caseTransform <*> pure c

-- | Returns a function that affects the case.
--
-- @since 0.1
caseTransform :: Gen (Char -> Char)
caseTransform =
  HG.element
    [ Ch.toUpper,
      Ch.toLower,
      id
    ]

-- TODO: switch to unconditional foldr' once the oldest stack snapshot we
-- support has it (text 2.0.1)
tfoldr :: (Char -> a -> a) -> a -> Text -> a
#if MIN_VERSION_text(2,0,1)
tfoldr = T.foldr'
#else
tfoldr = T.foldr
#endif
