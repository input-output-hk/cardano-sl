-- | Exceptions related to error reporting.

module Pos.Reporting.Exceptions
       ( ReportingError(..)
       ) where

import           Control.Exception (Exception)
import           Universum

data ReportingError
    = CantRetrieveLogs [FilePath]
    | SendingError Text
    deriving Show

instance Exception ReportingError
