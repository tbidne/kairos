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
    today :: Maybe Bool,
    -- | @since 0.1
    aliases :: Maybe (Map Text Text)
  }
  deriving stock (Eq, Show)

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe Bool, b ~ Maybe Bool) =>
  LabelOptic "today" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml _today _aliases) ->
    fmap (`MkToml` _aliases) (f _today)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Maybe (Map Text Text), b ~ Maybe (Map Text Text)) =>
  LabelOptic "aliases" k Toml Toml a b
  where
  labelOptic = lensVL $ \f (MkToml _today _aliases) ->
    fmap (MkToml _today) (f _aliases)
  {-# INLINE labelOptic #-}

-- | @since 0.1
instance DecodeTOML Toml where
  tomlDecoder = MkToml <$> decodeToday <*> decodeAliases

-- | @since 0.1
decodeToday :: Decoder (Maybe Bool)
decodeToday = getFieldOptWith tomlDecoder "today"

-- | @since 0.1
decodeAliases :: Decoder (Maybe (Map Text Text))
decodeAliases = getFieldOptWith tomlDecoder "aliases"
