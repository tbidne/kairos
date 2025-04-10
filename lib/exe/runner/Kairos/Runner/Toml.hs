{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Runner.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL)
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
    getFieldOptWith,
  )

-- | @since 0.1
data Toml = MkToml
  { -- | @since 0.1
    aliases :: Maybe (Map Text Text),
    -- | @since 0.1
    color :: Maybe Bool
  }
  deriving stock (Eq, Show)

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe (Map Text Text), b ~ Maybe (Map Text Text)) =>
  LabelOptic "aliases" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml a1 a2) ->
    fmap (\b -> MkToml b a2) (f a1)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Bool, b ~ Maybe Bool) =>
  LabelOptic "color" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml a1 a2) ->
    fmap (\b -> MkToml a1 b) (f a2)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML Toml where
  tomlDecoder = MkToml <$> decodeAliases <*> decodeColor

-- | @since 0.1
decodeAliases :: Decoder (Maybe (Map Text Text))
decodeAliases = getFieldOptWith tomlDecoder "aliases"

-- | @since 0.1
decodeColor :: Decoder (Maybe Bool)
decodeColor = getFieldOptWith tomlDecoder "color"
