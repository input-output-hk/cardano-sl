{-# LANGUAGE Rank2Types #-}

-- | This module basically combines reporting functionality with metrics.

module Pos.Reporting.Metrics
       ( DistrMonitor (..)
       , DistrMonitorState
       , mkDistrMonitorState
       , recordValue
       ) where

import           Universum

import           Data.Time.Units              (Microsecond)
import           Formatting                   (Format, sformat)
import           Mockable                     (CurrentTime, Mockable, currentTime)
import qualified System.Metrics               as Metrics
import           System.Metrics.Distribution  (Distribution, add)
import           System.Wlog                  (logDebug)

import           Pos.Reporting.Methods        (MonadReporting, reportMisbehaviour)
import           Pos.System.Metrics.Constants (withCardanoNamespace)

-- | 'DistrMonitor' is primarily used to parameterized 'recordValue'
-- function (see below).
data DistrMonitor = DistrMonitor
    { dmReportMisbehaviour :: Microsecond -> Maybe Double -> Double -> Maybe Bool
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
    , dmMisbehFormat       :: forall r. Format r (Double -> r)
    -- ^ A 'Format' of misbehaviour message.
    , dmDebugFormat        :: forall r. Maybe (Format r (Double -> r))
    -- ^ A 'Format' of debug message which is just logged with debug
    -- severity (if this format is 'Just' and value is not reported).
    , dmState              :: !DistrMonitorState
    -- ^ Internal state.
    }

-- | Mutable internal state of 'DistrMonitor'.
data DistrMonitorState = DistrMonitorState
    { dmsLastReportTime    :: !(TVar Microsecond)
    -- ^ Last time when value was reported (or 0).
    , dmsLastReportedValue :: !(TVar (Maybe Double))
    -- ^ Last reported value.
    , dmsDistr             :: !(Maybe Distribution)
    -- ^ An optional 'Distribution' where we track some value.
    }

-- | Make new initial 'DistrMonitorState'.
mkDistrMonitorState :: MonadIO m => Text -> Maybe Metrics.Store -> m DistrMonitorState
mkDistrMonitorState name storeMaybe = do
    dmsLastReportTime <- newTVarIO 0
    dmsLastReportedValue <- newTVarIO Nothing
    let modifiedName = withCardanoNamespace name
    dmsDistr <-
        case storeMaybe of
            Nothing -> return Nothing
            Just store ->
                liftIO $ Just <$> Metrics.createDistribution modifiedName store
    return DistrMonitorState {..}

-- | Add a value to the dsitribution stored in the
-- 'DistrMonitor'. Report this value if it should be reported
-- according to 'DistrMonitor'.
recordValue :: (MonadReporting ctx m, Mockable CurrentTime m) => DistrMonitor -> Double -> m ()
recordValue DistrMonitor {..} v = do
    let DistrMonitorState {..} = dmState
    whenJust dmsDistr $ \distr -> liftIO $ add distr v
    lastReportTime <- readTVarIO dmsLastReportTime
    curTime <- currentTime
    lastReportedValue <- readTVarIO dmsLastReportedValue
    case dmReportMisbehaviour (curTime - lastReportTime) lastReportedValue v of
        Nothing -> whenJust dmDebugFormat $ logDebug . flip sformat v
        Just isCritical -> do
            -- REPORT:MISBEHAVIOUR(?) Some metric is bad.
            reportMisbehaviour isCritical (sformat dmMisbehFormat v)
            atomically $ do
                writeTVar dmsLastReportTime curTime
                writeTVar dmsLastReportedValue (Just v)
