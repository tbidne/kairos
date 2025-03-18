{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | @since 0.1
module Kairos.Types.Date.Internal
  ( -- * Type
    Date (.., MkDate, MkDateString),

    -- * Construction
    parseDateString,

    -- * Elimination
    unDateString,
    unDate,
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
import GHC.Records (HasField (getField))
import Optics.Core (A_Getter, LabelOptic (labelOptic), to)
import Text.Read qualified as TR

-- | Represents a date string in the format @YYYY-MM-DD@.
--
-- @since 0.1
data Date = UnsafeDate Word16 Word8 Word8
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
pattern MkDateString :: Text -> Date
pattern MkDateString t <- (unDateString -> t)

{-# COMPLETE MkDateString #-}

-- | @since 0.1
pattern MkDate :: Word16 -> Word8 -> Word8 -> Date
pattern MkDate y m d <- UnsafeDate y m d

{-# COMPLETE MkDate #-}

-- | @since 0.1
instance HasField "unDateString" Date Text where
  getField = unDateString
  {-# INLINE getField #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Text, b ~ Text) =>
  LabelOptic "unDateString" k Date Date a b
  where
  labelOptic = to unDateString
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance HasField "unDate" Date (Word16, Word8, Word8) where
  getField = unDate
  {-# INLINE getField #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ (Word16, Word8, Word8), b ~ (Word16, Word8, Word8)) =>
  LabelOptic "unDate" k Date Date a b
  where
  labelOptic = to unDate
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance HasField "year" Date Word16 where
  getField = year
  {-# INLINE getField #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word16, b ~ Word16) =>
  LabelOptic "year" k Date Date a b
  where
  labelOptic = to year
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance HasField "month" Date Word8 where
  getField = month
  {-# INLINE getField #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word8, b ~ Word8) =>
  LabelOptic "month" k Date Date a b
  where
  labelOptic = to month
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance HasField "day" Date Word8 where
  getField = day
  {-# INLINE getField #-}

-- | @since 0.1
instance
  (k ~ A_Getter, a ~ Word8, b ~ Word8) =>
  LabelOptic "day" k Date Date a b
  where
  labelOptic = to day
  {-# INLINE labelOptic #-}

-- | @since 0.1
unDateString :: Date -> Text
unDateString (UnsafeDate y m d) =
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

-- | @since 0.1
unDate :: Date -> (Word16, Word8, Word8)
unDate (UnsafeDate y m d) = (y, m, d)

-- | Parses a date string in @YYYY-MM-DD@ form.
--
-- @since 0.1
parseDateString :: (MonadFail f) => Text -> f Date
parseDateString txt = case T.split (== '-') txt of
  [y, m, d] | nonEmpty y && nonEmpty m && nonEmpty d ->
    case (parseYear y, parseMonth m, parseDay d) of
      (Just y', Just m', Just d') -> pure $ UnsafeDate y' m' d'
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
year :: Date -> Word16
year (UnsafeDate y _ _) = y

-- | @since 0.1
month :: Date -> Word8
month (UnsafeDate _ m _) = m

-- | @since 0.1
day :: Date -> Word8
day (UnsafeDate _ _ d) = d

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
