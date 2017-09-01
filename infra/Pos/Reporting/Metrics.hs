{-# LANGUAGE Rank2Types #-}

-- | This module basically combines reporting functionality with metrics.

module Pos.Reporting.Metrics
       ( MetricMonitor (..)
       , MetricMonitorState
       , mkMetricMonitorState
       , recordValue
       ) where

import           Universum

import           Data.Time.Units              (Microsecond)
import           Formatting                   (Format, sformat)
import           Mockable                     (CurrentTime, Mockable, currentTime)
import qualified System.Metrics               as Metrics
import           System.Metrics.Gauge         (Gauge)
import qualified System.Metrics.Gauge         as Gauge
import           System.Wlog                  (logDebug)

import           Pos.Reporting.Methods        (MonadReporting, reportMisbehaviour)
import           Pos.System.Metrics.Constants (withCardanoNamespace)

-- | 'MetricMonitor' is primarily used to parameterize 'recordValue'
-- function (see below).
--
-- Note: currently this type only supports 'Double' values and uses
-- 'Gauge' metric type. Values are multipled by 1000 and rounded. We
-- can generalize it later if necessary.
data MetricMonitor = MetricMonitor
    { mmReportMisbehaviour :: Microsecond -> Maybe Double -> Double -> Maybe Bool
    -- ^ A predicate which determines which values should be also
    -- reported to reporting server, apart from tracking them in ekg.
    -- 'Nothing' means that value is good and shouldn't be reported at all.
    -- 'Just isCritical' means that value should be reported and 'isCritical'
    -- flag says whether misbehaviour is critical.
    --
    -- The first argument is how much time passed since last report of
    -- this type. The second one is previous reported value. The last
    -- one is the value we are processing.
    --
    -- A note on this type: one can imagine many things one can do with bad
    -- values, not only reporting it as a misbehaviour. A simple extension is
    -- to support other types of reports, e. g. error.
    -- More general approach is to permit doing arbitrary monadic action.
    -- Also we can use a custom ADT.
    -- 'Maybe Bool' was choosen as a reasonable trade-off between simplicity
    -- and generalization.
    , mmMisbehFormat       :: forall r. Format r (Double -> r)
    -- ^ A 'Format' of misbehaviour message.
    , mmDebugFormat        :: forall r. Maybe (Format r (Double -> r))
    -- ^ A 'Format' of debug message which is just logged with debug
    -- severity (if this format is 'Just' and value is not reported).
    , mmState              :: !MetricMonitorState
    -- ^ Internal state.
    }

-- | Mutable internal state of 'MetricMonitor'.
data MetricMonitorState = MetricMonitorState
    { mmsLastReportTime    :: !(TVar Microsecond)
    -- ^ Last time when value was reported (or 0).
    , mmsLastReportedValue :: !(TVar (Maybe Double))
    -- ^ Last reported value.
    , mmsGauge             :: !(Maybe Gauge)
    -- ^ An optional 'Distribution' where we track some value.
    }

-- | Make new initial 'MetricMonitorState'.
mkMetricMonitorState :: MonadIO m => Text -> Maybe Metrics.Store -> m MetricMonitorState
mkMetricMonitorState name storeMaybe = do
    mmsLastReportTime <- newTVarIO 0
    mmsLastReportedValue <- newTVarIO Nothing
    let modifiedName = withCardanoNamespace name
    mmsGauge <-
        case storeMaybe of
            Nothing -> return Nothing
            Just store ->
                liftIO $ Just <$> Metrics.createGauge modifiedName store
    return MetricMonitorState {..}

-- | Add a value to the dsitribution stored in the
-- 'MetricMonitor'. Report this value if it should be reported
-- according to 'MetricMonitor'.
recordValue :: (MonadReporting ctx m, Mockable CurrentTime m) => MetricMonitor -> Double -> m ()
recordValue MetricMonitor {..} v = do
    let MetricMonitorState {..} = mmState
    let roundedV = round (1000 * v)
    whenJust mmsGauge $ \gauge -> liftIO $ Gauge.set gauge roundedV
    lastReportTime <- readTVarIO mmsLastReportTime
    curTime <- currentTime
    lastReportedValue <- readTVarIO mmsLastReportedValue
    case mmReportMisbehaviour (curTime - lastReportTime) lastReportedValue v of
        Nothing -> whenJust mmDebugFormat $ logDebug . flip sformat v
        Just isCritical -> do
            -- REPORT:MISBEHAVIOUR(?) Some metric is bad.
            reportMisbehaviour isCritical (sformat mmMisbehFormat v)
            atomically $ do
                writeTVar mmsLastReportTime curTime
                writeTVar mmsLastReportedValue (Just v)
