-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Kairos.Runner (runKairos)
import Kairos.Types.Exception
  ( DateNoTimeStringException,
    LocalSystemTimeException,
    LocalTZException,
    LocalTimeZoneException,
    ParseTZInputException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  AnnUtils.setIgnoreKnownCallStackHandler proxies

  runKairos
  where
    proxies =
      [ MkExceptionProxy @DateNoTimeStringException,
        MkExceptionProxy @LocalSystemTimeException,
        MkExceptionProxy @LocalTimeZoneException,
        MkExceptionProxy @LocalTZException,
        MkExceptionProxy @ParseTimeException,
        MkExceptionProxy @ParseTZInputException,
        MkExceptionProxy @SrcTZNoTimeStringException
      ]
