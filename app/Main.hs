-- | Main module.
--
-- @since 0.1
module Main (main) where

import Control.Exception.Annotation.Utils (ExceptionProxy (MkExceptionProxy))
import Control.Exception.Annotation.Utils qualified as AnnUtils
import Data.Time.Conversion.Types.Exception
  ( DateNoTimeStringException,
    LocalSystemTimeException,
    LocalTimeZoneException,
    ParseTZInputException,
    ParseTimeException,
    SrcTZNoTimeStringException,
  )
import TimeConv.Runner (runTimeConv)

-- | Executable entry-point.
--
-- @since 0.1
main :: IO ()
main = do
  AnnUtils.setIgnoreKnownCallStackHandler proxies

  runTimeConv
  where
    proxies =
      [ MkExceptionProxy @DateNoTimeStringException,
        MkExceptionProxy @LocalSystemTimeException,
        MkExceptionProxy @LocalTimeZoneException,
        MkExceptionProxy @ParseTimeException,
        MkExceptionProxy @ParseTZInputException,
        MkExceptionProxy @SrcTZNoTimeStringException
      ]
