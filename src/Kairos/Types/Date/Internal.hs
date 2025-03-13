{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Types.Date.Internal
  ( -- * Type
    DateString (..),

    -- * Construction
    parseDateString,

    -- * Elimination
    unDateString,
    year,
    month,
    day,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import Optics.Core
  ( A_Getter,
    A_ReversedPrism,
    LabelOptic (labelOptic),
    ReversibleOptic (re),
    prism,
    to,
  )
import Text.Read qualified as TR

-- | Represents a date string in the format @YYYY-MM-DD@.
--
-- @since 0.1
data DateString = UnsafeDateString Word16 Word8 Word8
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Generic,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      NFData
    )

-- | @since 0.1
instance
  (k ~ A_ReversedPrism, a ~ Text, b ~ Text) =>
  LabelOptic "unDateString" k DateString DateString a b
  where
  labelOptic = re $ prism getter setter
    where
      setter t = case parseDateString t of
        Nothing -> Left t
        Just d -> Right d
      getter = unDateString
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word16, b ~ Word16) =>
  LabelOptic "year" k DateString DateString a b
  where
  labelOptic = to year
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word8, b ~ Word8) =>
  LabelOptic "month" k DateString DateString a b
  where
  labelOptic = to month
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word8, b ~ Word8) =>
  LabelOptic "day" k DateString DateString a b
  where
  labelOptic = to day
  {-# INLINE labelOptic #-}

-- | @since 0.1
unDateString :: DateString -> Text
unDateString (UnsafeDateString y m d) =
  mconcat
    [ showt y,
      "-",
      pad2 $ showt m,
      "-",
      pad2 $ showt d
    ]
  where
    showt :: (Show a) => a -> Text
    showt = T.pack . show
    pad2 x
      | T.length x == 1 = T.cons '0' x
      | otherwise = x

-- | Parses a date string in @YYYY-MM-DD@ form.
--
-- @since 0.1
parseDateString :: (MonadFail f) => Text -> f DateString
parseDateString txt = case T.split (== '-') txt of
  [y, m, d] | nonEmpty y && nonEmpty m && nonEmpty d ->
    case (parseYear y, parseMonth m, parseDay d) of
      (Just y', Just m', Just d') -> pure $ UnsafeDateString y' m' d'
      (Nothing, _, _) ->
        fail $
          "Year should be an integer between 1900 and 3000, received " <> squote y
      (_, Nothing, _) ->
        fail $
          "Month should be an integer between 1 and 12, received " <> squote m
      (_, _, Nothing) ->
        fail $
          "Day should be an integer between 1 and 31, received " <> squote d
  _ ->
    fail $ "Date has the form YYYY-MM-DD, received " <> squote txt

-- | @since 0.1
year :: DateString -> Word16
year (UnsafeDateString y _ _) = y

-- | @since 0.1
month :: DateString -> Word8
month (UnsafeDateString _ m _) = m

-- | @since 0.1
day :: DateString -> Word8
day (UnsafeDateString _ _ d) = d

parseYear :: Text -> Maybe Word16
parseYear = readDecimal @Word16 4 1900 3000

parseMonth :: Text -> Maybe Word8
parseMonth = readDecimal @Word8 2 1 12

parseDay :: Text -> Maybe Word8
parseDay = readDecimal @Word8 2 1 31

readDecimal :: (Ord a, Read a) => Int -> a -> a -> Text -> Maybe a
readDecimal len l u t = do
  tLen <- if T.length t == len then Just t else Nothing
  n <- TR.readMaybe (T.unpack tLen)
  if n >= l && n <= u
    then Just n
    else Nothing

nonEmpty :: Text -> Bool
nonEmpty = not . T.null . T.strip

squote :: Text -> String
squote t = T.unpack $ "'" <> t <> "'"
