{-# LANGUAGE TypeFamilies #-}

-- | Reporting functionality abstracted.
module Pos.Reporting.MemState
       ( MisbehaviorMetrics (..)
       , ReportingContext (..)
       , HasReportServers (..)
       , HasMisbehaviorMetrics (..)
       , HasLoggerConfig (..)
       , HasReportingContext (..)
       , emptyReportingContext
       , initializeMisbehaviorMetrics
       , mmRollbacks
       , mmSscFailures
       , mmIgnoredCommitments
       , rcMisbehaviorMetrics
       ) where

import           Control.Lens (makeLenses)
import qualified System.Metrics as Metrics
import           System.Metrics.Counter (Counter)
import           System.Metrics.Gauge (Gauge)
import           System.Wlog.LoggerConfig (LoggerConfig)
import           Universum

import           Pos.System.Metrics.Constants (withCardanoNamespace)

-- | EKG metric values for misbehaviors
data MisbehaviorMetrics = MisbehaviorMetrics
    { _mmRollbacks          :: Gauge
    -- ^ Amount of rolled back blocks after latest fork
    , _mmSscFailures        :: Counter
    -- ^ Amount of SSC computation failures
    , _mmIgnoredCommitments :: Gauge
    -- ^ Number of epochs in a row without expected node's commitments
    }

makeLenses ''MisbehaviorMetrics

-- | Context needed to provide remote reporting capabilities.
data ReportingContext = ReportingContext
    { _rcReportServers      :: ![Text] -- ^ Report servers list (urls)
    , _rcLoggingConfig      :: !LoggerConfig
    , _rcMisbehaviorMetrics :: Maybe MisbehaviorMetrics
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

class HasMisbehaviorMetrics ctx where
    misbehaviorMetrics :: Lens' ctx (Maybe MisbehaviorMetrics)

instance HasReportingContext ctx => HasMisbehaviorMetrics ctx where
    misbehaviorMetrics = reportingContext . rcMisbehaviorMetrics

emptyReportingContext :: ReportingContext
emptyReportingContext = ReportingContext [] mempty Nothing

initializeMisbehaviorMetrics :: MonadIO m => Metrics.Store -> m MisbehaviorMetrics
initializeMisbehaviorMetrics store = liftIO $ do
    _mmRollbacks <- Metrics.createGauge (withCardanoNamespace "BlockRollbacks") store
    _mmSscFailures <- Metrics.createCounter (withCardanoNamespace "SCCComputationFailures") store
    _mmIgnoredCommitments <- Metrics.createGauge (withCardanoNamespace "IgnoredCommitments") store
    return MisbehaviorMetrics{..}
