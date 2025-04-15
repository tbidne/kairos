{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}

module Kairos.Runner.Args.TH
  ( gitHash,
  )
where

import Development.GitRev.Typed.OsString qualified as GRT
import FileSystem.OsPath (decodeThrowM, osp)
import Language.Haskell.TH (Code, Q)

-- | Returns the hash or the result of an environment variable lookup on
-- @KAIROS_HASH@.
gitHash :: Code Q String
gitHash = toCode qs
  where
    toCode = GRT.qToCode . (>>= decodeThrowM) . GRT.projectError

    qs =
      GRT.firstSuccessQ
        [ GRT.embedGitError GRT.gitShortHashQ,
          GRT.embedEnvLookupError $ GRT.envValQ [osp|KAIROS_HASH|],
          GRT.runGitInEnvDirQ [osp|KAIROS_HOME|] GRT.gitShortHashQ
        ]
