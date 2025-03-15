{-# LANGUAGE UndecidableInstances #-}

module Params
  ( TestParams (..),
    defParams,
    fromArgs,
  )
where

import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL, set')

-- | Test params.
data TestParams = MkTestParams
  { -- | CLI args.
    args :: [String],
    -- | If false, prepends --no-config to 'args'.
    configEnabled :: Bool,
    -- | If given, represents the (mock) current time.
    mCurrentTime :: Maybe String
  }

instance
  (k ~ A_Lens, a ~ [String], b ~ [String]) =>
  LabelOptic "args" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3) ->
    fmap (\b -> MkTestParams b a2 a3) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "configEnabled" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3) ->
    fmap (\b -> MkTestParams a1 b a3) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe String, b ~ Maybe String) =>
  LabelOptic "mCurrentTime" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3) ->
    fmap (\b -> MkTestParams a1 a2 b) (f a3)
  {-# INLINE labelOptic #-}

-- | Default params.
defParams :: TestParams
defParams =
  MkTestParams
    { args = [],
      configEnabled = False,
      mCurrentTime = Nothing
    }

-- | Params given CLI args.
fromArgs :: [String] -> TestParams
fromArgs args = set' #args args defParams
