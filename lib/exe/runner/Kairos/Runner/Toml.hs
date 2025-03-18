{-# LANGUAGE UndecidableInstances #-}

-- | @since 0.1
module Kairos.Runner.Toml
  ( Toml (..),
  )
where

import Data.Map.Strict (Map)
import Data.Text (Text)
import Optics.Core (An_Iso, LabelOptic (labelOptic), iso)
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
    getFieldOptWith,
  )

-- | @since 0.1
newtype Toml = MkToml
  { -- | @since 0.1
    aliases :: Maybe (Map Text Text)
  }
  deriving stock (Eq, Show)

-- | @since 0.1
instance
  (k ~ An_Iso, a ~ Maybe (Map Text Text), b ~ Maybe (Map Text Text)) =>
  LabelOptic "aliases" k Toml Toml a b
  where
  labelOptic = iso (.aliases) MkToml
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML Toml where
  tomlDecoder = MkToml <$> decodeAliases

-- | @since 0.1
decodeAliases :: Decoder (Maybe (Map Text Text))
decodeAliases = getFieldOptWith tomlDecoder "aliases"
