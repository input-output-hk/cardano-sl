{-# LANGUAGE TypeFamilies #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( ReportingContext (..)
       , HasReportServers (..)
       , HasLoggerConfig (..)
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

class HasReportServers ctx where
    reportServers :: Lens' ctx [Text]

instance HasReportServers ReportingContext where
    reportServers = rcReportServers

class HasLoggerConfig ctx where
    loggerConfig :: Lens' ctx LoggerConfig

instance HasLoggerConfig ReportingContext where
    loggerConfig = rcLoggingConfig

class HasReportingContext ctx where
    reportingContext :: Lens' ctx ReportingContext

emptyReportingContext :: ReportingContext
emptyReportingContext = ReportingContext [] mempty
