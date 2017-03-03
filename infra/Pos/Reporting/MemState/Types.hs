module Pos.Reporting.MemState.Types
       ( ReportingContext (..)
       ) where

import           Universum

data ReportingContext = ReportingContext
    { _rprReportServers :: ![Text]
    }
