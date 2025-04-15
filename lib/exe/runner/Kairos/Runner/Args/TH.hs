module Kairos.Runner.Args.TH
  ( gitHash,
  )
where

import Development.GitRev.Typed qualified as GRT
import Language.Haskell.TH (Code, Q)

-- | Returns the hash or the result of an environment variable lookup on
-- @KAIROS_HASH@.
gitHash :: Code Q String
gitHash = toCode qs
  where
    toCode = GRT.qToCode . GRT.liftError

    qs =
      GRT.firstRight
        (GRT.liftGitError GRT.gitShortHashQ)
        [ GRT.envValQ "KAIROS_HASH",
          GRT.runGitInEnvDirQ "KAIROS_HOME" GRT.gitShortHashQ
        ]
