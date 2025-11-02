{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Params
  ( OptString (..),
    CliArgs (..),
    TestParams (..),
    defParams,
    fromArgs,
  )
where

import Data.String (IsString (fromString))
import FileSystem.OsPath (OsPath)
import GHC.Exts (IsList (Item, fromList, toList))
import Optics.Core (A_Lens, LabelOptic (labelOptic), lensVL, set')

-- | OptString is obviously isomorphic to Maybe String. It exists purely for
-- convenience, so we can use its IsString instance to write e.g.
-- "America/New_York" rather than (Just "America/New_York").
data OptString
  = OptNothing
  | OptJust String

instance Semigroup OptString where
  OptNothing <> r = r
  OptJust l <> _ = OptJust l

instance Monoid OptString where
  mempty = OptNothing

instance IsString OptString where
  fromString = OptJust

data CliArgs = MkCliArgs
  { -- | Actual args.
    args :: [String],
    -- | Optional --date argument.
    date :: OptString,
    -- | Optional --dest-tz argument.
    destTZ :: OptString,
    -- | Optional --src-tz argument.
    srcTZ :: OptString
  }

instance Semigroup CliArgs where
  x <> y =
    MkCliArgs
      (x.args <> y.args)
      (x.date <> y.date)
      (x.destTZ <> y.destTZ)
      (x.srcTZ <> y.srcTZ)

instance Monoid CliArgs where
  mempty = MkCliArgs [] OptNothing OptNothing OptNothing

instance IsList CliArgs where
  type Item CliArgs = String

  fromList = g
    where
      g :: [String] -> CliArgs
      g [] = mempty
      g [x] = MkCliArgs [x] OptNothing OptNothing OptNothing
      g (x : y : zs) = case x of
        "--date" -> MkCliArgs [] (OptJust y) OptNothing OptNothing <> g zs
        "-d" -> MkCliArgs [] OptNothing (OptJust y) OptNothing <> g zs
        "-s" -> MkCliArgs [] OptNothing OptNothing (OptJust y) <> g zs
        _ -> MkCliArgs [x, y] OptNothing OptNothing OptNothing <> g zs

  toList cliArgs =
    f "--date" cliArgs.date
      ++ f "-s" cliArgs.srcTZ
      ++ f "-d" cliArgs.destTZ
      ++ cliArgs.args
    where
      f _ OptNothing = []
      f k (OptJust s) = [k, s]

instance IsString CliArgs where
  fromString s = MkCliArgs [s] OptNothing OptNothing OptNothing

instance
  (k ~ A_Lens, a ~ [String], b ~ [String]) =>
  LabelOptic "args" k CliArgs CliArgs a b
  where
  labelOptic = lensVL $ \f (MkCliArgs a1 a2 a3 a4) ->
    fmap (\b -> MkCliArgs b a2 a3 a4) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OptString, b ~ OptString) =>
  LabelOptic "date" k CliArgs CliArgs a b
  where
  labelOptic = lensVL $ \f (MkCliArgs a1 a2 a3 a4) ->
    fmap (\b -> MkCliArgs a1 b a3 a4) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OptString, b ~ OptString) =>
  LabelOptic "destTZ" k CliArgs CliArgs a b
  where
  labelOptic = lensVL $ \f (MkCliArgs a1 a2 a3 a4) ->
    fmap (\b -> MkCliArgs a1 a2 b a4) (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ OptString, b ~ OptString) =>
  LabelOptic "srcTZ" k CliArgs CliArgs a b
  where
  labelOptic = lensVL $ \f (MkCliArgs a1 a2 a3 a4) ->
    fmap (\b -> MkCliArgs a1 a2 a3 b) (f a4)
  {-# INLINE labelOptic #-}

-- | Test params.
data TestParams = MkTestParams
  { -- | CLI args.
    cliArgs :: CliArgs,
    -- | If false, prepends --config off to 'args'.
    configEnabled :: Bool,
    -- | If given, represents the (mock) current time.
    mCurrentTime :: Maybe String,
    -- | Optional overridden xdg dir.
    mXdg :: Maybe OsPath
  }

instance
  (k ~ A_Lens, a ~ CliArgs, b ~ CliArgs) =>
  LabelOptic "cliArgs" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3 a4) ->
    fmap (\b -> MkTestParams b a2 a3 a4) (f a1)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "configEnabled" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3 a4) ->
    fmap (\b -> MkTestParams a1 b a3 a4) (f a2)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe String, b ~ Maybe String) =>
  LabelOptic "mCurrentTime" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3 a4) ->
    fmap (\b -> MkTestParams a1 a2 b a4) (f a3)
  {-# INLINE labelOptic #-}

instance
  (k ~ A_Lens, a ~ Maybe OsPath, b ~ Maybe OsPath) =>
  LabelOptic "mXdg" k TestParams TestParams a b
  where
  labelOptic = lensVL $ \f (MkTestParams a1 a2 a3 a4) ->
    fmap (\b -> MkTestParams a1 a2 a3 b) (f a4)
  {-# INLINE labelOptic #-}

-- | Default params.
defParams :: TestParams
defParams =
  MkTestParams
    { cliArgs = [],
      configEnabled = False,
      mCurrentTime = Nothing,
      mXdg = Nothing
    }

-- | Params given CLI args.
fromArgs :: CliArgs -> TestParams
fromArgs args = set' #cliArgs args defParams
