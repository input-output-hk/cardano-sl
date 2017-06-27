{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( ReportingContext (..)
       , rcReportServers
       , rcLoggingConfig
       , HasReportingContext (..)
       , emptyReportingContext
       ) where

import           Control.Lens             (makeLenses)
import           System.Wlog.LoggerConfig (LoggerConfig)
import           Universum

-- | Context needed to provide remote reporting capabilities.
data ReportingContext = ReportingContext
    { _rcReportServers :: ![Text] -- ^ Report servers list (urls)
    , _rcLoggingConfig :: !LoggerConfig
    }

makeLenses ''ReportingContext

class HasReportingContext ctx where
    reportingContext :: Lens' ctx ReportingContext

emptyReportingContext :: ReportingContext
emptyReportingContext = ReportingContext [] mempty
