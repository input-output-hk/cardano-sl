{-# LANGUAGE Rank2Types #-}

-- | This module basically combines reporting functionality with metrics.

module Pos.Reporting.Metrics
       ( MetricMonitor (..)
       , MetricMonitorState
       , mkMetricMonitorState
       , noReportMonitor
       , recordValue
       ) where

import           Universum

import           Data.Time.Units (Microsecond)
import           Formatting (Format, build, sformat)
import           Mockable (CurrentTime, Mockable, currentTime)
import qualified System.Metrics as Metrics
import           System.Metrics.Gauge (Gauge)
import qualified System.Metrics.Gauge as Gauge
import           System.Wlog (WithLogger, logDebug)

import           Pos.Reporting.Methods (MonadReporting)
import           Pos.System.Metrics.Constants (withCardanoNamespace)

-- | 'MetricMonitor' is primarily used to parameterize 'recordValue'
-- function (see below).
--
-- Note: currently this type only supports 'Gauge' metric type. We can
-- generalize it later if necessary.
data MetricMonitor value = MetricMonitor
    { mmReportMisbehaviour :: Microsecond -> Maybe value -> value -> Maybe Bool
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
    , mmMisbehFormat       :: forall r. Format r (value -> r)
    -- ^ A 'Format' of misbehaviour message.
    , mmDebugFormat        :: forall r. Maybe (Format r (value -> r))
    -- ^ A 'Format' of debug message which is just logged with debug
    -- severity (if this format is 'Just' and value is not reported).
    , mmConvertValue       :: value -> Int64
    -- ^ A way to convert abstract 'value' to 'Int64'.
    , mmState              :: !(MetricMonitorState value)
    -- ^ Internal state.
    }

-- | Mutable internal state of 'MetricMonitor'.
data MetricMonitorState value = MetricMonitorState
    { mmsLastReportTime    :: !(TVar Microsecond)
    -- ^ Last time when value was reported (or 0).
    , mmsLastReportedValue :: !(TVar (Maybe value))
    -- ^ Last reported value.
    , mmsGauge             :: !Gauge
    -- ^ A 'Gauge' where we track some value. Can be easily made
    -- optional later if needed.
    }

-- | Make new initial 'MetricMonitorState'.
mkMetricMonitorState ::
       MonadIO m => Text -> Metrics.Store -> m (MetricMonitorState value)
mkMetricMonitorState name store = do
    mmsLastReportTime <- newTVarIO 0
    mmsLastReportedValue <- newTVarIO Nothing
    let modifiedName = withCardanoNamespace name
    mmsGauge <- liftIO $ Metrics.createGauge modifiedName store
    return MetricMonitorState {..}

-- | Make 'MetricMonitorState' which never reports.
noReportMonitor ::
       Buildable value
    => (value -> Int64)
    -- ^ How to convert value to integer.
    -> (forall r. Maybe (Format r (value -> r)))
    -- ^ How to format value for debug.
    -> MetricMonitorState value
    -> MetricMonitor value
noReportMonitor converter debugFormat st =
    MetricMonitor
    { mmState = st
    , mmReportMisbehaviour = classifier
    , mmMisbehFormat = build -- won't be used due to classifier
    , mmConvertValue = converter
    , mmDebugFormat = debugFormat
    }
  where
    classifier _ _ _ = Nothing

-- | Update the value stored in the 'MetricMonitor's gauge.  Report
-- this value if it should be reported according to 'MetricMonitor'.
recordValue ::
       ( MonadIO m
       , MonadReporting m
       , Mockable CurrentTime m
       , WithLogger m
       )
    => MetricMonitor value
    -> value
    -> m ()
recordValue MetricMonitor {..} v = do
    let MetricMonitorState {..} = mmState
    let vInt = mmConvertValue v
    liftIO $ Gauge.set mmsGauge vInt
    lastReportTime <- readTVarIO mmsLastReportTime
    curTime <- currentTime
    lastReportedValue <- readTVarIO mmsLastReportedValue
    case mmReportMisbehaviour (curTime - lastReportTime) lastReportedValue v of
        Nothing -> whenJust mmDebugFormat $ logDebug . flip sformat v
        Just _ -> do
            atomically $ do
                writeTVar mmsLastReportTime curTime
                writeTVar mmsLastReportedValue (Just v)
